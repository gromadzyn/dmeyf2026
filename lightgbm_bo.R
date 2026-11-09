# Baseline Experimentos Colaborativos DMEyF 2026 - competencia_02
#
# 01 Preprocesamiento
#    - clase_ternaria
#    - data quality: en cada foto_mes, las variables con todos sus valores
#      exactamente en cero y sin ningun NA pasan a NA
#    - FE historico: lags y delta lags de orden 1 y 2 sobre todas las
#      variables excepto numero_de_cliente, foto_mes y clase_ternaria
#      (762 columnas para entrenar)
# 02 Optimizacion de hiperparametros (mlr3mbo)
#    - undersampling 0.1 de los CONTINUA, training_pct 70 (holdout 70/30)
#    - 12 puntos de diseno inicial + 10 iteraciones bayesianas
#    - se reporta AUC y mejores hiperparametros
# 03 Produccion
#    - training final 201901 a 202104 con undersampling 0.1
#    - 10 semillas, se promedian las probabilidades
#    - testing en 202106: ganancia maxima y cantidad de envios
#    - se registra tiempo de entrenamiento y cantidad de columnas

format(Sys.time(), "%a %b %d %X %Y")

# limpio la memoria
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

# Librerias ---------------------------------------------------------------

instalar_y_cargar <- function(paquetes) {
  for (p in paquetes) {
    if (!require(p, character.only = TRUE)) {
      install.packages(p, repos = "https://cloud.r-project.org")
      require(p, character.only = TRUE)
    }
  }
}

instalar_y_cargar(c(
  "data.table", "R.utils", "yaml", "primes", "lightgbm",
  "mlr3", "mlr3mbo", "mlr3learners", "bbotk", "paradox", "DiceKriging"
))

# Parametros --------------------------------------------------------------

PARAM <- list()
PARAM$experimento <- "Exp_0" # ID de la planilla Experimentos_Colaborativos
PARAM$semilla_primigenia <- 170383

PARAM$dataset <- "competencia_02_crudo.csv.gz"

# meses
PARAM$train <- 201901:202104 # se filtra a los meses que existen
PARAM$train <- PARAM$train[PARAM$train %% 100 %in% 1:12]
PARAM$gap <- 202105
PARAM$future <- 202106

# un undersampling de 0.1 toma solo el 10% de los CONTINUA
# siempre se conservan todos los BAJA+1 y BAJA+2
PARAM$trainingstrategy$undersampling <- 0.1

# Bayesian Optimization
PARAM$hyperparametertuning$training_pct <- 70 # holdout 70/30
PARAM$hyperparametertuning$ksemillerio <- 1
PARAM$hyperparametertuning$repe <- 1
PARAM$hyperparametertuning$diseno_inicial <- 12
PARAM$hyperparametertuning$iteraciones <- 10 # iteraciones bayesianas

# Produccion
PARAM$produccion$semillas <- 10

# ganancia
PARAM$ganancia$acierto <- 1072500 # BAJA+2 contactado
PARAM$ganancia$error <- -27500 # no BAJA+2 contactado

# parametros fijos del LightGBM
PARAM$lgbm$param_fijos <- list(
  boosting = "gbdt",
  objective = "binary",
  metric = "auc",
  feature_pre_filter = FALSE,
  first_metric_only = FALSE,
  boost_from_average = TRUE,
  force_row_wise = TRUE,
  verbosity = -100,

  min_data_in_leaf = 0L,
  feature_fraction = 0.5,
  feature_fraction_bynode = 0.2,
  learning_rate = 0.005
)

# bordes de busqueda de la BO
PARAM$hyperparametertuning$bordes <- list(
  num_iterations = c(64L, 4096L),
  num_leaves = c(16L, 2048L),
  min_sum_hessian_in_leaf = c(1e-06, 0.1)
)

# Carpetas ----------------------------------------------------------------

HOME_DMeyF <- path.expand(Sys.getenv("DMEYF_HOME", "~/dmeyf"))
PATH_DATASETS <- file.path(HOME_DMeyF, "datasets")
PATH_EXP <- file.path(HOME_DMeyF, "exp", PARAM$experimento)

dir.create(PATH_EXP, showWarnings = FALSE, recursive = TRUE)
setwd(PATH_EXP)

# Funciones ---------------------------------------------------------------

# particionar agrega una columna llamada fold a un dataset
#   que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30
#  fold toma valores 1..length(division)

particionar <- function(data, division, agrupa = "", campo = "fold",
                        start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed, kind = "L'Ecuyer-CMRG")

  # vector con los numeros de fold repetidos segun el peso de cada uno
  bloque <- unlist(mapply(function(x, y) rep(y, x), division,
                          seq(from = start, length.out = length(division))))

  # dentro de cada grupo de agrupa, asigno los folds en orden aleatorio
  # recorriendo el bloque circularmente => particion estratificada
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa]

  invisible(data)
}

# AUC por ranking (Mann-Whitney), sin dependencias
auc_rank <- function(prob, label) {
  r <- frank(prob, ties.method = "average")
  n1 <- sum(label == 1L)
  n0 <- length(label) - n1
  (sum(r[label == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# ganancia maxima y cantidad de envios, ordenando por prob descendente
ganancia_maxima <- function(prob, clase_ternaria) {
  orden <- order(-prob)
  gan <- cumsum(ifelse(clase_ternaria[orden] == "BAJA+2",
                       PARAM$ganancia$acierto, PARAM$ganancia$error))
  pos <- which.max(gan)
  list(ganancia = gan[pos], envios = pos)
}

minutos <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "mins"))

# Checkpoints: si la corrida se corta, al volver a correr el script se retoma
#  - cada evaluacion de la BO se agrega a bo_evaluaciones.txt apenas termina
#  - cada semilla de produccion se guarda en semillas/ apenas termina
# para empezar de cero, borrar la carpeta del experimento
ARCH_BO_EVALS <- "bo_evaluaciones.txt"
DIR_SEMILLAS <- "semillas"

# escribe a un temporal y renombra, para no dejar archivos a medio escribir
fwrite_atomico <- function(x, file, ...) {
  tmp <- paste0(file, ".tmp")
  fwrite(x, file = tmp, ...)
  file.rename(tmp, file)
}

# 01 Preprocesamiento -----------------------------------------------------

t_inicio <- Sys.time()

dataset <- fread(file.path(PATH_DATASETS, PARAM$dataset))

# clase_ternaria, a partir del periodo consecutivo
dsimple <- dataset[, list(
  pos = .I,
  numero_de_cliente,
  periodo0 = as.integer(foto_mes / 100) * 12 + foto_mes %% 100
)]

setorder(dsimple, numero_de_cliente, periodo0)

periodo_ultimo <- dsimple[, max(periodo0)]
periodo_anteultimo <- periodo_ultimo - 1

dsimple[, c("periodo1", "periodo2") :=
          shift(periodo0, n = 1:2, fill = NA, type = "lead"), numero_de_cliente]

dsimple[periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA"]

dsimple[periodo0 < periodo_ultimo &
          (is.na(periodo1) | periodo0 + 1 < periodo1),
        clase_ternaria := "BAJA+1"]

dsimple[periodo0 < periodo_anteultimo & (periodo0 + 1 == periodo1) &
          (is.na(periodo2) | periodo0 + 2 < periodo2),
        clase_ternaria := "BAJA+2"]

setorder(dsimple, pos)
dataset[, clase_ternaria := dsimple$clase_ternaria]
rm(dsimple)

print(dcast(dataset[, .N, list(foto_mes, clase_ternaria)],
            foto_mes ~ clase_ternaria, value.var = "N"))

variables <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

# Data quality: variables todas en cero y sin NA en un foto_mes pasan a NA
setorder(dataset, foto_mes, numero_de_cliente)
rangos_mes <- dataset[, list(desde = min(.I), hasta = max(.I)), foto_mes]

log_dq <- list()
for (k in seq_len(nrow(rangos_mes))) {
  idx <- rangos_mes$desde[k]:rangos_mes$hasta[k]
  for (v in variables) {
    x <- dataset[[v]][idx]
    if (is.numeric(x) && !anyNA(x) && all(x == 0)) {
      set(dataset, i = idx, j = v, value = NA)
      log_dq[[length(log_dq) + 1]] <-
        data.table(foto_mes = rangos_mes$foto_mes[k], variable = v)
    }
  }
}
log_dq <- rbindlist(log_dq)
fwrite(log_dq, file = "data_quality.txt", sep = "\t")
cat("Data quality: ", nrow(log_dq), " pares (foto_mes, variable) pasados a NA\n")

# FE historico: lags y delta lags de orden 1 y 2
# el lag es el registro anterior del mismo cliente
setorder(dataset, numero_de_cliente, foto_mes)

mismo_cliente_1 <- dataset$numero_de_cliente ==
  shift(dataset$numero_de_cliente, 1L)
mismo_cliente_2 <- dataset$numero_de_cliente ==
  shift(dataset$numero_de_cliente, 2L)

for (v in variables) {
  x <- dataset[[v]]

  lag1 <- shift(x, 1L)
  lag1[!(mismo_cliente_1 %in% TRUE)] <- NA

  lag2 <- shift(x, 2L)
  lag2[!(mismo_cliente_2 %in% TRUE)] <- NA

  set(dataset, j = paste0(v, "_lag1"), value = lag1)
  set(dataset, j = paste0(v, "_lag2"), value = lag2)
  set(dataset, j = paste0(v, "_delta1"), value = x - lag1)
  set(dataset, j = paste0(v, "_delta2"), value = x - lag2)
}
rm(x, lag1, lag2, mismo_cliente_1, mismo_cliente_2)

campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria"))
cat("Columnas para entrenar: ", length(campos_buenos), "\n")
if (length(campos_buenos) != 762) {
  warning("El baseline indica 762 columnas para entrenar y hay ",
          length(campos_buenos))
}

# clase binaria: BAJA+1 y BAJA+2 son 1, CONTINUA es 0
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+1", "BAJA+2"), 1L, 0L)]

# undersampling de los CONTINUA en los meses de training
set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
dataset[, azar := runif(.N)]
dataset[, training := 0L]
dataset[foto_mes %in% PARAM$train &
          (azar <= PARAM$trainingstrategy$undersampling |
             clase_ternaria %in% c("BAJA+1", "BAJA+2")),
        training := 1L]

# me quedo solo con lo necesario para liberar memoria
dataset <- dataset[training == 1L | foto_mes %in% PARAM$future]
gc(full = TRUE, verbose = FALSE)

dataset_train <- dataset[training == 1L]
dfuture <- dataset[foto_mes %in% PARAM$future]
rm(dataset)
gc(full = TRUE, verbose = FALSE)

print(dataset_train[, .N, clase_ternaria])

t_preproc <- minutos(t_inicio)

# 02 Optimizacion de hiperparametros --------------------------------------

t_bo <- Sys.time()

# holdout estratificado 70 / 30
particionar(dataset_train,
            division = c(PARAM$hyperparametertuning$training_pct,
                         100 - PARAM$hyperparametertuning$training_pct),
            agrupa = "clase_ternaria",
            seed = PARAM$semilla_primigenia)

dbo_train <- lgb.Dataset(
  data = data.matrix(dataset_train[fold == 1L, campos_buenos, with = FALSE]),
  label = dataset_train[fold == 1L, clase01],
  free_raw_data = FALSE
)

mvalid <- data.matrix(dataset_train[fold == 2L, campos_buenos, with = FALSE])
yvalid <- dataset_train[fold == 2L, clase01]

EstimarAUC_lightgbm <- function(xs) {
  t0 <- Sys.time()

  param_completo <- modifyList(PARAM$lgbm$param_fijos, xs)
  param_completo$seed <- PARAM$semilla_primigenia

  modelo <- lgb.train(data = dbo_train, param = param_completo)
  auc <- auc_rank(predict(modelo, mvalid), yvalid)

  rm(modelo)
  gc(full = TRUE, verbose = FALSE)

  t_eval <- minutos(t0)

  # checkpoint: agrego la evaluacion al archivo apenas termina
  fwrite(data.table(num_iterations = xs$num_iterations,
                    num_leaves = xs$num_leaves,
                    min_sum_hessian_in_leaf = xs$min_sum_hessian_in_leaf,
                    auc = auc,
                    minutos = t_eval),
         file = ARCH_BO_EVALS, sep = "\t",
         append = file.exists(ARCH_BO_EVALS))

  message(format(Sys.time(), "%a %b %d %X %Y"),
          " AUC ", round(auc, 6),
          " (", round(t_eval, 1), " min) ",
          paste(names(xs), unlist(xs), sep = "=", collapse = " "))

  list(auc = auc, minutos = t_eval)
}

bordes <- PARAM$hyperparametertuning$bordes

dominio <- ps(
  num_iterations = p_int(bordes$num_iterations[1], bordes$num_iterations[2]),
  num_leaves = p_int(bordes$num_leaves[1], bordes$num_leaves[2]),
  min_sum_hessian_in_leaf = p_dbl(bordes$min_sum_hessian_in_leaf[1],
                                  bordes$min_sum_hessian_in_leaf[2])
)

# min_sum_hessian_in_leaf abarca 5 ordenes de magnitud: se busca en escala log
espacio_busqueda <- ps(
  num_iterations = p_int(bordes$num_iterations[1], bordes$num_iterations[2]),
  num_leaves = p_int(bordes$num_leaves[1], bordes$num_leaves[2]),
  min_sum_hessian_in_leaf = p_dbl(log(bordes$min_sum_hessian_in_leaf[1]),
                                  log(bordes$min_sum_hessian_in_leaf[2]),
                                  trafo = function(x) exp(x))
)

objetivo <- ObjectiveRFun$new(
  fun = EstimarAUC_lightgbm,
  domain = dominio,
  codomain = ps(auc = p_dbl(tags = "maximize"))
)

instancia <- oi(
  objective = objetivo,
  search_space = espacio_busqueda,
  terminator = trm("evals",
                   n_evals = PARAM$hyperparametertuning$diseno_inicial +
                     PARAM$hyperparametertuning$iteraciones)
)

# diseno inicial (los puntos "no inteligentes")
# es determinista dada la semilla, asi que al retomar es el mismo
set.seed(PARAM$semilla_primigenia)
diseno <- generate_design_sobol(espacio_busqueda,
                                n = PARAM$hyperparametertuning$diseno_inicial)
n_diseno <- nrow(diseno$data)

# retomo las evaluaciones hechas en una corrida anterior que se corto
n_previas <- 0L
if (file.exists(ARCH_BO_EVALS)) {
  previas <- fread(ARCH_BO_EVALS)
  previas <- previas[!is.na(auc)] # descarto una linea a medio escribir
  n_previas <- nrow(previas)

  # las primeras evaluaciones son el diseno inicial, en el mismo orden
  k <- min(n_previas, n_diseno)
  if (k > 0) {
    esperado <- diseno$data[1:k]
    coincide <-
      all(previas$num_iterations[1:k] == esperado$num_iterations) &&
      all(previas$num_leaves[1:k] == esperado$num_leaves) &&
      isTRUE(all.equal(log(previas$min_sum_hessian_in_leaf[1:k]),
                       esperado$min_sum_hessian_in_leaf, tolerance = 1e-8))
    if (!coincide) {
      stop("bo_evaluaciones.txt no corresponde a esta configuracion. ",
           "Para empezar de cero, borre la carpeta ", PATH_EXP)
    }
  }

  for (i in seq_len(n_previas)) {
    xdt <- previas[i, list(num_iterations, num_leaves,
                           min_sum_hessian_in_leaf = log(min_sum_hessian_in_leaf))]
    xss <- list(as.list(previas[i, list(num_iterations, num_leaves,
                                        min_sum_hessian_in_leaf)]))
    instancia$archive$add_evals(xdt, xss,
                                previas[i, list(auc, minutos)])
  }
  cat("BO: se retoman ", n_previas, " evaluaciones de la corrida anterior\n")
}

if (n_previas < n_diseno) {
  instancia$eval_batch(diseno$data[(n_previas + 1):n_diseno])
}

# iteraciones bayesianas: kriging matern 3/2 + expected improvement
surrogate <- srlrn(lrn("regr.km",
                       covtype = "matern3_2",
                       nugget.estim = TRUE, # la AUC es ruidosa
                       control = list(trace = FALSE)))

optimizador <- opt("mbo",
  loop_function = bayesopt_ego,
  surrogate = surrogate,
  acq_function = acqf("ei"),
  acq_optimizer = acqo(opt("random_search", batch_size = 1000),
                       terminator = trm("evals", n_evals = 10000))
)

# las iteraciones bayesianas se hacen de a una, fijando una semilla que
# depende de cuantas evaluaciones hay: asi una corrida retomada propone
# exactamente los mismos puntos que una corrida sin cortes
n_total <- PARAM$hyperparametertuning$diseno_inicial +
  PARAM$hyperparametertuning$iteraciones

while (instancia$archive$n_evals < n_total) {
  n_actual <- instancia$archive$n_evals
  set.seed(PARAM$semilla_primigenia + n_actual)
  instancia$terminator$param_set$values$n_evals <- n_actual + 1L
  optimizador$optimize(instancia)
}
instancia$terminator$param_set$values$n_evals <- n_total

tb_bayesiana <- as.data.table(instancia$archive$data)
tb_bayesiana <- tb_bayesiana[, c(
  "batch_nr", "auc", "minutos",
  names(PARAM$hyperparametertuning$bordes)
), with = FALSE]
# las columnas del archivo estan en el espacio de busqueda (log)
tb_bayesiana[, min_sum_hessian_in_leaf := exp(min_sum_hessian_in_leaf)]
tb_bayesiana[, iter := .I]
setorder(tb_bayesiana, -auc)

fwrite(tb_bayesiana, file = "BO_log.txt", sep = "\t")

mejores <- as.list(tb_bayesiana[1, names(PARAM$hyperparametertuning$bordes),
                                with = FALSE])
mejores$num_iterations <- as.integer(mejores$num_iterations)
mejores$num_leaves <- as.integer(mejores$num_leaves)

PARAM$out$bo$auc <- tb_bayesiana[1, auc]
PARAM$out$bo$mejores_hiperparametros <- mejores

cat("Mejor AUC: ", PARAM$out$bo$auc, "\n")
print(mejores)

rm(dbo_train, mvalid, yvalid)
gc(full = TRUE, verbose = FALSE)

t_bo <- minutos(t_bo)

# 03 Produccion -----------------------------------------------------------

t_prod <- Sys.time()

# training final: todos los meses de train con undersampling, sin holdout
dtrain_final <- lgb.Dataset(
  data = data.matrix(dataset_train[, campos_buenos, with = FALSE]),
  label = dataset_train[, clase01],
  free_raw_data = FALSE
)

mfuture <- data.matrix(dfuture[, campos_buenos, with = FALSE])

param_final <- modifyList(PARAM$lgbm$param_fijos, mejores)

# semillas derivadas de la semilla primigenia
set.seed(PARAM$semilla_primigenia)
semillas <- sample(primes::generate_primes(min = 100000, max = 1000000),
                   PARAM$produccion$semillas)
PARAM$produccion$lista_semillas <- semillas

tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
tb_semillas <- list()

dir.create(DIR_SEMILLAS, showWarnings = FALSE)

# checkpoint de las semillas: la prediccion de cada una se guarda apenas
# termina, junto con los hiperparametros con que se entreno
for (i in seq_along(semillas)) {
  t0 <- Sys.time()
  param_final$seed <- semillas[i]
  arch_semilla <- file.path(DIR_SEMILLAS, paste0("pred_", semillas[i], ".txt"))

  prob <- NULL
  if (file.exists(arch_semilla)) {
    previa <- fread(arch_semilla)
    if (identical(previa$numero_de_cliente, tb_prediccion$numero_de_cliente) &&
        isTRUE(all.equal(previa$num_iterations[1], mejores$num_iterations)) &&
        isTRUE(all.equal(previa$num_leaves[1], mejores$num_leaves)) &&
        isTRUE(all.equal(previa$min_sum_hessian_in_leaf[1],
                         mejores$min_sum_hessian_in_leaf))) {
      prob <- previa$prob
      t_semilla <- previa$minutos[1]
      cat("Semilla ", semillas[i], " retomada de la corrida anterior\n", sep = "")
    }
  }

  if (is.null(prob)) {
    modelo <- lgb.train(data = dtrain_final, param = param_final)
    prob <- predict(modelo, mfuture)
    t_semilla <- minutos(t0)

    if (i == 1) {
      fwrite(as.data.table(lgb.importance(modelo)),
             file = "impo.txt", sep = "\t")
    }

    fwrite_atomico(data.table(numero_de_cliente = tb_prediccion$numero_de_cliente,
                              prob = prob,
                              minutos = t_semilla,
                              num_iterations = mejores$num_iterations,
                              num_leaves = mejores$num_leaves,
                              min_sum_hessian_in_leaf =
                                mejores$min_sum_hessian_in_leaf),
                   file = arch_semilla, sep = "\t")

    rm(modelo)
    gc(full = TRUE, verbose = FALSE)
  }

  tb_prediccion[, paste0("prob_", i) := prob]

  g <- ganancia_maxima(prob, tb_prediccion$clase_ternaria)
  tb_semillas[[i]] <- data.table(semilla = semillas[i],
                                 ganancia = g$ganancia,
                                 envios = g$envios,
                                 minutos = t_semilla)
  cat("Semilla ", semillas[i], " ganancia=", format(g$ganancia, big.mark = ".", decimal.mark = ","),
      " envios=", g$envios, "\n", sep = "")
}

tb_semillas <- rbindlist(tb_semillas)
fwrite(tb_semillas, file = "ganancia_semillas.txt", sep = "\t")

# promedio de las probabilidades de las semillas
cols_prob <- paste0("prob_", seq_along(semillas))
tb_prediccion[, prob := rowMeans(.SD), .SDcols = cols_prob]

fwrite(tb_prediccion[, c("numero_de_cliente", "foto_mes", "prob", cols_prob),
                     with = FALSE],
       file = "prediccion.txt", sep = "\t")

g_ens <- ganancia_maxima(tb_prediccion$prob, tb_prediccion$clase_ternaria)

t_prod <- minutos(t_prod)

# Resumen -----------------------------------------------------------------

PARAM$out$produccion <- list(
  ganancia_maxima = g_ens$ganancia,
  envios = g_ens$envios,
  ganancia_media_por_semilla = mean(tb_semillas$ganancia),
  desvio_entre_semillas = sd(tb_semillas$ganancia)
)

PARAM$out$cantidad_columnas <- length(campos_buenos)
PARAM$out$registros_training <- nrow(dataset_train)
# tiempos de reloj de esta corrida (si se retomo, no incluyen la anterior)
# y suma de los tiempos de entrenamiento, que si incluyen lo retomado
PARAM$out$minutos <- list(
  preprocesamiento = t_preproc,
  optimizacion = t_bo,
  produccion = t_prod,
  total = minutos(t_inicio),
  entrenamiento_bo = sum(tb_bayesiana$minutos),
  entrenamiento_produccion = sum(tb_semillas$minutos)
)

write_yaml(PARAM, file = "PARAM.yml")

cat("\n==== ", PARAM$experimento, " ====\n",
    "Columnas: ", length(campos_buenos), "\n",
    "AUC BO: ", round(PARAM$out$bo$auc, 6), "\n",
    "Ganancia maxima (promedio ", length(semillas), " semillas): ",
    format(g_ens$ganancia, big.mark = ".", decimal.mark = ","), "\n",
    "Envios: ", g_ens$envios, "\n",
    "Ganancia media por semilla: ",
    format(round(mean(tb_semillas$ganancia)), big.mark = ".", decimal.mark = ","), "\n",
    "Desvio entre semillas: ",
    format(round(sd(tb_semillas$ganancia)), big.mark = ".", decimal.mark = ","), "\n",
    "Tiempo de entrenamiento BO + produccion (min): ",
    round(sum(tb_bayesiana$minutos) + sum(tb_semillas$minutos), 1), "\n",
    "Tiempo total de esta corrida (min): ", round(minutos(t_inicio), 1), "\n",
    sep = "")

format(Sys.time(), "%a %b %d %X %Y")
