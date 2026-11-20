# Target Engineering - Grupo B - DMEyF 2026 competencia_02
#
# Es el baseline de lightgbm_bo.R (Exp_0) con UNA sola diferencia: al
# entrenar (en la BO y en produccion) los BAJA+1 tienen peso w; BAJA+2 y
# CONTINUA tienen peso 1. El target binario sigue siendo BAJA+1 U BAJA+2.
# Con w = 1 es exactamente Exp_0, por eso Exp_0 es el brazo de control.
#
# Uso:  Rscript te_nucleo.R <peso> [corridas]
#   corridas: cuales de las 10 corre este proceso, ej. 1-5, 6-10, 3 o 1,4,7
#   (por defecto las 10). Asi una rama se reparte entre varias VMs chicas:
#   cada una corre su rango y todas respaldan en la misma carpeta del bucket.
#   Con corridas el log va a exp/TE_w<peso>_c<corridas>.log.
# El resultado queda en exp/TE_w<peso>/. El analisis (Wilcoxon, ensamble
# por rankings, mecanismo) lo hace te_analisis.R.
#
# Hilos: las ramas usan num_threads = 2 (VMs de 2 vCPU: LightGBM escala mal
# con muchos hilos, 6 VMs de 2 vCPU rinden ~2 veces una de 12). Exp_0 quedo
# con 12 hilos; la cantidad de hilos solo cambia redondeos, sin sesgo.
#
# 10 corridas: todo el pipeline (BO + produccion de 10 semillas) se repite
# con 10 semillas primigenias: 170383 y 9 primos sorteados a partir de ella
# (las mismas en Exp_0 y en todas las ramas). Cada corrida queda en
# exp/<experimento>/sp_<semilla>/ y es una observacion para Wilcoxon.
# El preprocesamiento no depende de la semilla: se hace una sola vez.
#
# 01 Preprocesamiento
#    - clase_ternaria
#    - data quality: en cada foto_mes, las variables con todos sus valores
#      exactamente en cero y sin ningun NA pasan a NA
#    - FE historico: lags y delta lags de orden 1 y 2 sobre todas las
#      variables excepto numero_de_cliente, foto_mes y clase_ternaria
#      (762 columnas para entrenar)
# 02 Optimizacion de hiperparametros (mlr3mbo)
#    Igual a BASELINE_FINAL.ipynb (repo germandasr/DMEyF_Exp_Colaborativos):
#    - holdout estratificado 70/30 sobre TODO el train; el undersampling 0.1
#      de los CONTINUA se hace solo en el 70%; se valida en el 30% completo
#    - la AUC de cada evaluacion es best_score: LightGBM mide la AUC del 30%
#      en cada iteracion y se queda con la maxima (sin early stopping)
#    - parametros fijos los de config.yaml (max_bin 31, deterministic, ...)
#    - min_sum_hessian_in_leaf se busca en escala lineal, como el notebook
#    - iteraciones = 10 TOTALES (diapositiva del baseline). El diseno inicial
#      de mlr3mbo (4 x 3 hiperparametros = 12 puntos Sobol) se evalua completo,
#      asi que terminan siendo 12 evaluaciones y ninguna bayesiana
#    - se reporta AUC y mejores hiperparametros
# 03 Produccion
#    - training final 201901 a 202104; con CADA semilla se vuelve a sortear
#      el undersampling 0.1 de los CONTINUA (como el notebook)
#    - 10 semillas (primos sorteados con la semilla primigenia, como el
#      notebook), se promedian las probabilidades
#    - testing en 202106: ganancia maxima y cantidad de envios
#    - se registra tiempo de entrenamiento y cantidad de columnas

format(Sys.time(), "%a %b %d %X %Y")

# limpio la memoria
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

# Librerias ---------------------------------------------------------------

# sin sudo no se puede escribir en la biblioteca del sistema: los paquetes
# que falten se instalan en la biblioteca personal (~/R/...)
lib_usuario <- path.expand(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep)[[1]][1])
dir.create(lib_usuario, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(lib_usuario, .libPaths()))

instalar_y_cargar <- function(paquetes) {
  for (p in paquetes) {
    if (!require(p, character.only = TRUE)) {
      install.packages(p, lib = lib_usuario, repos = "https://cloud.r-project.org",
                       Ncpus = parallel::detectCores())
      require(p, character.only = TRUE)
    }
  }
}

instalar_y_cargar(c(
  "data.table", "R.utils", "yaml", "lightgbm",
  "mlr3", "mlr3mbo", "mlr3learners", "bbotk", "paradox", "DiceKriging"
))

# Parametros --------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 1:2) {
  stop("Uso: Rscript te_nucleo.R <peso_baja1> [corridas]   ej: 0.5 1-5")
}

PARAM <- list()
PARAM$peso_baja1 <- as.numeric(args[1])
if (is.na(PARAM$peso_baja1) || PARAM$peso_baja1 < 0) {
  stop("El peso de BAJA+1 tiene que ser un numero >= 0")
}
PARAM$experimento <- sprintf("TE_w%.2f", PARAM$peso_baja1)
PARAM$semilla_primigenia_base <- 170383 # la misma de Exp_0: mismas semillas
PARAM$corridas <- 10 # una por semilla primigenia

# cuales corridas hace este proceso: "1-5", "6-10", "3", "1,4,7"
ARG_CORRIDAS <- if (length(args) == 2) args[2] else ""
if (!grepl("^[0-9,-]*$", ARG_CORRIDAS)) stop("corridas: algo como 1-5, 6-10 o 1,4,7")
CORRIDAS_A_CORRER <- if (ARG_CORRIDAS == "") seq_len(PARAM$corridas) else {
  sort(unique(unlist(lapply(strsplit(ARG_CORRIDAS, ",")[[1]], function(x) {
    ab <- as.integer(strsplit(x, "-")[[1]])
    if (length(ab) == 1) ab else seq(ab[1], ab[2])
  }))))
}
if (anyNA(CORRIDAS_A_CORRER) || length(CORRIDAS_A_CORRER) == 0 ||
    any(CORRIDAS_A_CORRER < 1 | CORRIDAS_A_CORRER > PARAM$corridas)) {
  stop("corridas tiene que estar entre 1 y ", PARAM$corridas, ": ", ARG_CORRIDAS)
}
SUFIJO <- if (ARG_CORRIDAS == "") "" else paste0("_c", ARG_CORRIDAS)

# primos de 6 cifras sorteados con una semilla, con el procedimiento de
# BASELINE_FINAL.ipynb (se usa para las semillas primigenias y las de produccion)
es_primo <- function(n) {
  if (n %% 2 == 0) return(FALSE)
  all(n %% seq(3, floor(sqrt(n)), by = 2) != 0)
}
sortear_primos <- function(semilla, n) {
  set.seed(semilla, kind = "L'Ecuyer-CMRG")
  primos <- integer(0)
  while (length(primos) < n) {
    cand <- sample(100000:999999, 1)
    if (es_primo(cand) && !(cand %in% primos)) primos <- c(primos, cand)
  }
  primos
}

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
PARAM$hyperparametertuning$iteraciones <- 10 # evaluaciones TOTALES de la BO
# diseno inicial por defecto de mlr3mbo (4 x cantidad de hiperparametros);
# se evalua completo aunque supere las iteraciones: terminan siendo 12
PARAM$hyperparametertuning$diseno_inicial <- 12

# Produccion
PARAM$produccion$semillas <- 10

# semillas primigenias: la base y 9 primos del mismo sorteo. Se saltean los
# primeros 10, que son las semillas de produccion de la corrida base
PARAM$semillas_primigenias <- c(
  PARAM$semilla_primigenia_base,
  sortear_primos(PARAM$semilla_primigenia_base,
                 PARAM$produccion$semillas + PARAM$corridas - 1)[
    -seq_len(PARAM$produccion$semillas)]
)

# ganancia
PARAM$ganancia$acierto <- 1072500 # BAJA+2 contactado
PARAM$ganancia$error <- -27500 # no BAJA+2 contactado

# parametros fijos del LightGBM: los de lgbm$fijos del config.yaml de
# BASELINE_FINAL.ipynb (num_iterations, num_leaves y min_sum_hessian_in_leaf
# los pone la BO)
PARAM$lgbm$param_fijos <- list(
  boosting = "gbdt",
  objective = "binary",
  metric = "auc",
  feature_pre_filter = FALSE,
  first_metric_only = FALSE,
  boost_from_average = TRUE,
  force_row_wise = TRUE,
  deterministic = TRUE,
  num_threads = 2L, # ramas en VMs de 2 vCPU (Exp_0 quedo con 12)
  verbosity = -100,
  max_depth = -1L,
  min_gain_to_split = 0,
  lambda_l1 = 0,
  lambda_l2 = 0,
  max_bin = 31L,

  bagging_fraction = 1,
  pos_bagging_fraction = 1,
  neg_bagging_fraction = 1,
  is_unbalance = FALSE,
  scale_pos_weight = 1,

  drop_rate = 0.1, # solo afectan a dart; estan para copiar el config.yaml
  max_drop = 50L,
  skip_drop = 0.5,

  extra_trees = FALSE,

  early_stopping = 0L,
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

if (parallel::detectCores() < PARAM$lgbm$param_fijos$num_threads) {
  warning("Esta maquina tiene ", parallel::detectCores(), " CPUs y LightGBM usa ",
          PARAM$lgbm$param_fijos$num_threads, " hilos: va a andar lento")
}

# Carpetas ----------------------------------------------------------------

HOME_DMeyF <- path.expand(Sys.getenv("DMEYF_HOME", "~/dmeyf"))
PATH_DATASETS <- file.path(HOME_DMeyF, "datasets")
PATH_EXP <- file.path(HOME_DMeyF, "exp", PARAM$experimento)

dir.create(PATH_EXP, showWarnings = FALSE, recursive = TRUE)

# Respaldo en bucket ------------------------------------------------------

# Si existe ~/dmeyf/bucket.txt con el nombre de un bucket, cada corrida
# (sp_<semilla>/), configuracion.yml y el log se suben a gs://<bucket>/exp/
# despues de cada checkpoint. Cada proceso sube y baja SOLO las corridas de
# su rango, asi varias VMs pueden compartir la carpeta de una rama sin
# pisarse. En una VM nueva se bajan antes de empezar las corridas de su rango
# que no esten en el disco, asi una VM Spot borrada no pierde lo calculado.
# Si la subida falla se avisa en el log y la corrida sigue igual.
ARCH_BUCKET <- file.path(HOME_DMeyF, "bucket.txt")
BUCKET <- if (file.exists(ARCH_BUCKET)) {
  sub("^gs://", "", sub("/+$", "", trimws(readLines(ARCH_BUCKET, n = 1L, warn = FALSE))))
} else ""
if (length(BUCKET) == 0 || is.na(BUCKET)) BUCKET <- ""
GS_EXP <- paste0("gs://", BUCKET, "/exp/", PARAM$experimento)
ARCH_LOG <- file.path(HOME_DMeyF, "exp", paste0(PARAM$experimento, SUFIJO, ".log"))
ARCH_CONFIG <- file.path(PATH_EXP, "configuracion.yml")
ARCH_RESUMEN <- paste0("resumen_corridas", SUFIJO, ".txt")

gcloud <- function(args) {
  salida <- tryCatch(
    suppressWarnings(system2("gcloud", args, stdout = TRUE, stderr = TRUE,
                             timeout = 900)),
    error = function(e) structure(conditionMessage(e), status = 1L))
  estado <- attr(salida, "status")
  list(ok = is.null(estado) || estado == 0L, salida = salida)
}

# sube la corrida sp (si se da), configuracion.yml, el resumen y el log
respaldar <- function(sp = NULL) {
  if (BUCKET == "") return(invisible(FALSE))
  r <- list(ok = TRUE)
  if (!is.null(sp)) {
    r <- gcloud(c("storage", "rsync", "--recursive",
                  shQuote("--exclude=.*\\.tmp$|^modelo_tmp\\.txt$"),
                  shQuote(file.path(PATH_EXP, paste0("sp_", sp))),
                  paste0(GS_EXP, "/sp_", sp)))
  }
  sueltos <- c(ARCH_CONFIG, file.path(PATH_EXP, ARCH_RESUMEN))
  for (f in sueltos[file.exists(sueltos)]) {
    if (r$ok) r <- gcloud(c("storage", "cp", shQuote(f), paste0(GS_EXP, "/", basename(f))))
  }
  if (r$ok && file.exists(ARCH_LOG)) {
    r <- gcloud(c("storage", "cp", shQuote(ARCH_LOG),
                  paste0(GS_EXP, SUFIJO, ".log")))
  }
  if (!r$ok) {
    cat("ATENCION: no se pudo respaldar en gs://", BUCKET, ": ",
        paste(tail(r$salida, 3), collapse = " | "), "\n", sep = "")
  }
  invisible(r$ok)
}

# checkpoints de cualquier corrida (o de una version vieja, sin sp_*)
hay_checkpoints <- function(dir = PATH_EXP) {
  length(list.files(dir, recursive = TRUE,
                    pattern = "^bo_evaluaciones\\.txt$|^pred_.*\\.txt$")) > 0
}

if (BUCKET == "") {
  cat("ATENCION: sin ", ARCH_BUCKET, ": no se respalda en ningun bucket\n", sep = "")
} else {
  # la configuracion del bucket (de esta u otra VM) se baja si no hay local,
  # para que el chequeo de abajo frene si el bucket tiene otra version
  if (!file.exists(ARCH_CONFIG)) {
    gcloud(c("storage", "cp", paste0(GS_EXP, "/configuracion.yml"), shQuote(ARCH_CONFIG)))
  }
  bajadas <- 0L
  for (sp in PARAM$semillas_primigenias[CORRIDAS_A_CORRER]) {
    d <- file.path(PATH_EXP, paste0("sp_", sp))
    if (hay_checkpoints(d)) next
    dir.create(d, showWarnings = FALSE)
    r <- gcloud(c("storage", "rsync", "--recursive", paste0(GS_EXP, "/sp_", sp), shQuote(d)))
    if (r$ok && hay_checkpoints(d)) bajadas <- bajadas + 1L
  }
  if (bajadas > 0) {
    cat("Respaldo: checkpoints de ", bajadas, " corrida(s) bajados de ", GS_EXP, "\n", sep = "")
  } else {
    cat("Respaldo: no hay checkpoints nuevos en ", GS_EXP, " para estas corridas\n", sep = "")
  }
}

# Los checkpoints solo se retoman si son de esta misma configuracion: la
# primera vez se escribe configuracion.yml y despues se compara. Una carpeta
# con checkpoints de otra version del script (por ejemplo, de antes de
# max_bin = 31) frena la corrida en vez de mezclarse con la nueva.
config_actual <- as.yaml(list(
  version = "baseline_final_ipynb",
  experimento = PARAM$experimento,
  semillas_primigenias = PARAM$semillas_primigenias,
  peso_baja1 = PARAM$peso_baja1,
  train = PARAM$train,
  future = PARAM$future,
  undersampling = PARAM$trainingstrategy$undersampling,
  hyperparametertuning = PARAM$hyperparametertuning,
  semillas = PARAM$produccion$semillas,
  param_fijos = PARAM$lgbm$param_fijos
))

config_distinta <- file.exists(ARCH_CONFIG) &&
  !identical(read_yaml(ARCH_CONFIG), yaml.load(config_actual))
if (hay_checkpoints() || config_distinta) {
  if (!file.exists(ARCH_CONFIG) || config_distinta) {
    stop("La carpeta ", PATH_EXP, " tiene checkpoints de otra configuracion ",
         "(otra version del script). Movela a otro lado para empezar de cero:\n",
         "  mkdir -p ", file.path(HOME_DMeyF, "exp_viejo"), " && mv ", PATH_EXP, " ",
         file.path(HOME_DMeyF, "exp_viejo"), "/\n",
         if (BUCKET != "") paste0("  gcloud storage mv ", GS_EXP, " gs://",
                                  BUCKET, "/exp_viejo/", PARAM$experimento, "\n"),
         call. = FALSE)
  }
} else {
  writeLines(config_actual, ARCH_CONFIG, sep = "")
}

if (BUCKET != "" && respaldar()) cat("Respaldo: OK en ", GS_EXP, "\n", sep = "")

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
#  - cada corrida (semilla primigenia) tiene su carpeta sp_<semilla>/ y
#    escribe su PARAM.yml al terminar: las terminadas no se rehacen
#  - con ~/dmeyf/bucket.txt, todo se sube al bucket despues de cada una
# para empezar de cero, borrar la carpeta del experimento
ARCH_BO_EVALS <- "bo_evaluaciones.txt"
DIR_SEMILLAS <- "semillas"

# importancia de variables (Gain, Cover, Frequency) igual a lgb.importance,
# pero leyendo el modelo guardado en disco de a pedazos: lgb.importance arma
# todo el modelo como un unico JSON en memoria, y con miles de arboles de
# miles de hojas supera el limite de 2^31-1 bytes de un string de R
importancia_lgbm <- function(modelo, archivo) {
  lgb.save(modelo, archivo) # lo escribe LightGBM desde C, sin ese limite
  on.exit(unlink(archivo))
  con <- file(archivo, "r")
  on.exit(close(con), add = TRUE)

  nombres <- NULL
  feat <- list(); gain <- list(); cover <- list(); k <- 0L
  repeat {
    lineas <- readLines(con, n = 20000L)
    if (length(lineas) == 0L) break
    if (is.null(nombres)) {
      fn <- grep("^feature_names=", lineas, value = TRUE)
      if (length(fn)) nombres <- strsplit(sub("^feature_names=", "", fn[1]), " ")[[1]]
    }
    sf <- grep("^split_feature=", lineas, value = TRUE)
    sg <- grep("^split_gain=", lineas, value = TRUE)
    ic <- grep("^internal_count=", lineas, value = TRUE)
    for (j in seq_along(sf)) {
      k <- k + 1L
      feat[[k]] <- as.integer(strsplit(sub("^split_feature=", "", sf[j]), " ")[[1]])
      gain[[k]] <- as.numeric(strsplit(sub("^split_gain=", "", sg[j]), " ")[[1]])
      cover[[k]] <- as.numeric(strsplit(sub("^internal_count=", "", ic[j]), " ")[[1]])
    }
  }

  dt <- data.table(Feature = nombres[unlist(feat) + 1L],
                   g = unlist(gain), c = unlist(cover))
  imp <- dt[, list(Gain = sum(g), Cover = sum(c), Frequency = .N), by = Feature]
  imp[, `:=`(Gain = Gain / sum(Gain), Cover = Cover / sum(Cover),
             Frequency = Frequency / sum(Frequency))]
  setorder(imp, -Gain)
  imp[]
}

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

# UNICO cambio respecto del baseline: peso w para BAJA+1, 1 para el resto
dataset[, peso := ifelse(clase_ternaria == "BAJA+1", PARAM$peso_baja1, 1)]
cat("Peso BAJA+1 = ", PARAM$peso_baja1, "\n", sep = "")

# me quedo con los meses de training (completos: el undersampling se hace
# despues, en la BO y con cada semilla de produccion) y con el future
dataset_train <- dataset[foto_mes %in% PARAM$train]
dfuture <- dataset[foto_mes %in% PARAM$future]
rm(dataset)
gc(full = TRUE, verbose = FALSE)

print(dataset_train[, .N, clase_ternaria])

# undersampling: todos los BAJA+1 y BAJA+2, y los CONTINUA con azar <= 0.1
filas_undersampling <- function(azar) {
  azar <= PARAM$trainingstrategy$undersampling |
    dataset_train$clase_ternaria %in% c("BAJA+1", "BAJA+2")
}

t_preproc <- minutos(t_inicio)

# Corridas ----------------------------------------------------------------

# una fila del resumen de corridas, a partir del PARAM de una corrida
fila_corrida <- function(P) {
  data.table(
    semilla_primigenia = P$semilla_primigenia,
    auc_bo = P$out$bo$auc,
    num_iterations = P$out$bo$mejores_hiperparametros$num_iterations,
    num_leaves = P$out$bo$mejores_hiperparametros$num_leaves,
    min_sum_hessian_in_leaf = P$out$bo$mejores_hiperparametros$min_sum_hessian_in_leaf,
    ganancia_maxima = P$out$produccion$ganancia_maxima,
    envios = P$out$produccion$envios,
    ganancia_media_por_semilla = P$out$produccion$ganancia_media_por_semilla,
    desvio_entre_semillas = P$out$produccion$desvio_entre_semillas,
    minutos_entrenamiento = P$out$minutos$entrenamiento_bo +
      P$out$minutos$entrenamiento_produccion
  )
}

# una corrida completa (BO + produccion + resumen) con la semilla primigenia
# sp, en la carpeta sp_<sp>/
correr <- function(sp, k) {
  # 02 Optimizacion de hiperparametros ------------------------------------


  t_corrida <- Sys.time()
  PARAM$semilla_primigenia <- sp
  cat("\n######## Corrida ", k, "/", length(PARAM$semillas_primigenias),
      ": semilla primigenia ", sp, " ########\n", sep = "")

  dir_corrida <- file.path(PATH_EXP, paste0("sp_", sp))
  dir.create(dir_corrida, showWarnings = FALSE)
  setwd(dir_corrida)
  on.exit(setwd(PATH_EXP))

  if (file.exists("PARAM.yml")) {
    cat("Corrida ya terminada: se lee su PARAM.yml\n")
    return(fila_corrida(read_yaml("PARAM.yml")))
  }

  t_bo <- Sys.time()

  # como BASELINE_FINAL.ipynb: primero el holdout estratificado 70 / 30 sobre
  # todo el train, despues el undersampling solo en el 70% (fold 1)
  particionar(dataset_train,
              division = c(PARAM$hyperparametertuning$training_pct,
                           100 - PARAM$hyperparametertuning$training_pct),
              agrupa = "clase_ternaria",
              seed = PARAM$semilla_primigenia)

  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  dataset_train[, training := as.integer(fold == 1L & filas_undersampling(runif(.N)))]

  # se construye ya (para liberar las matrices), asi que los parametros que
  # definen el binning tienen que ir aca y no recien en lgb.train
  dbo_train <- lgb.Dataset(
    data = data.matrix(dataset_train[training == 1L, campos_buenos, with = FALSE]),
    params = PARAM$lgbm$param_fijos[c("max_bin", "feature_pre_filter")],
    label = dataset_train[training == 1L, clase01],
    weight = dataset_train[training == 1L, peso]
  )
  lgb.Dataset.construct(dbo_train)
  PARAM$out$registros_training_bo <- nrow(dataset_train[training == 1L])

  # la validacion es el 30% completo (fold 2), sin undersampling y sin pesos:
  # la AUC no ponderada, con BAJA+1 como positivo, igual que el baseline
  # (limitacion declarada: favorece a w = 1)
  dbo_valid <- lgb.Dataset.create.valid(
    dbo_train,
    data = data.matrix(dataset_train[fold == 2L, campos_buenos, with = FALSE]),
    label = dataset_train[fold == 2L, clase01]
  )
  lgb.Dataset.construct(dbo_valid)
  gc(full = TRUE, verbose = FALSE)
  cat("BO: ", PARAM$out$registros_training_bo, " registros de training y ",
      nrow(dataset_train[fold == 2L]), " de validacion\n", sep = "")

  EstimarAUC_lightgbm <- function(xs) {
    t0 <- Sys.time()

    param_completo <- modifyList(PARAM$lgbm$param_fijos, xs)
    param_completo$seed <- PARAM$semilla_primigenia

    # como el notebook: sin early stopping, best_score es la maxima AUC de
    # validacion entre todas las iteraciones
    modelo <- lgb.train(data = dbo_train, valids = list(val = dbo_valid),
                        param = param_completo)
    auc <- modelo$best_score
    mejor_iter <- modelo$best_iter
    auc_final <- tail(unlist(modelo$record_evals$val$auc$eval), 1L)

    rm(modelo)
    gc(full = TRUE, verbose = FALSE)

    t_eval <- minutos(t0)

    # checkpoint: agrego la evaluacion al archivo apenas termina
    fwrite(data.table(num_iterations = xs$num_iterations,
                      num_leaves = xs$num_leaves,
                      min_sum_hessian_in_leaf = xs$min_sum_hessian_in_leaf,
                      auc = auc,
                      mejor_iter = mejor_iter,
                      auc_final = auc_final,
                      minutos = t_eval),
           file = ARCH_BO_EVALS, sep = "\t",
           append = file.exists(ARCH_BO_EVALS))

    respaldar(sp)

    message(format(Sys.time(), "%a %b %d %X %Y"),
            " AUC ", round(auc, 6), " (iter ", mejor_iter, ")",
            " (", round(t_eval, 1), " min) ",
            paste(names(xs), unlist(xs), sep = "=", collapse = " "))

    list(auc = auc, minutos = t_eval)
  }

  bordes <- PARAM$hyperparametertuning$bordes

  # como el notebook, los tres en escala lineal
  dominio <- ps(
    num_iterations = p_int(bordes$num_iterations[1], bordes$num_iterations[2]),
    num_leaves = p_int(bordes$num_leaves[1], bordes$num_leaves[2]),
    min_sum_hessian_in_leaf = p_dbl(bordes$min_sum_hessian_in_leaf[1],
                                    bordes$min_sum_hessian_in_leaf[2])
  )
  espacio_busqueda <- dominio

  objetivo <- ObjectiveRFun$new(
    fun = EstimarAUC_lightgbm,
    domain = dominio,
    codomain = ps(auc = p_dbl(tags = "maximize"))
  )

  # evaluaciones totales: las iteraciones, salvo que el diseno inicial sea mayor
  n_total <- max(PARAM$hyperparametertuning$iteraciones,
                 PARAM$hyperparametertuning$diseno_inicial)

  instancia <- oi(
    objective = objetivo,
    search_space = espacio_busqueda,
    terminator = trm("evals",
                     n_evals = n_total)
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
    if (n_previas > n_total) {
      stop("bo_evaluaciones.txt tiene ", n_previas, " evaluaciones y la BO es de ",
           n_total, ". Deje solo las primeras ", n_total, " o empiece de cero")
    }

    # las primeras evaluaciones son el diseno inicial, en el mismo orden
    k <- min(n_previas, n_diseno)
    if (k > 0) {
      esperado <- diseno$data[1:k]
      coincide <-
        all(previas$num_iterations[1:k] == esperado$num_iterations) &&
        all(previas$num_leaves[1:k] == esperado$num_leaves) &&
        isTRUE(all.equal(previas$min_sum_hessian_in_leaf[1:k],
                         esperado$min_sum_hessian_in_leaf, tolerance = 1e-8))
      if (!coincide) {
        stop("bo_evaluaciones.txt no corresponde a esta configuracion. ",
             "Para empezar de cero, borre la carpeta ", PATH_EXP)
      }
    }

    for (i in seq_len(n_previas)) {
      xdt <- previas[i, list(num_iterations, num_leaves, min_sum_hessian_in_leaf)]
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

  # iteraciones bayesianas (con 10 evaluaciones totales no hay ninguna):
  # kriging + expected improvement, igual que el notebook
  surrogate <- srlrn(lrn("regr.km",
                         optim.method = "BFGS",
                         nugget.estim = TRUE, # la AUC es ruidosa
                         control = list(trace = FALSE)))

  optimizador <- opt("mbo",
    loop_function = bayesopt_ego,
    surrogate = surrogate,
    acq_function = acqf("ei"),
    acq_optimizer = acqo(opt("random_search", batch_size = 1000),
                         terminator = trm("evals", n_evals = 1000))
  )

  # las iteraciones bayesianas se hacen de a una, fijando una semilla que
  # depende de cuantas evaluaciones hay: asi una corrida retomada propone
  # exactamente los mismos puntos que una corrida sin cortes

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

  rm(dbo_train, dbo_valid)
  dataset_train[, c("fold", "training") := NULL]
  gc(full = TRUE, verbose = FALSE)

  t_bo <- minutos(t_bo)

  # 03 Produccion -----------------------------------------------------------

  t_prod <- Sys.time()

  # training final: todos los meses de train, sin holdout; el undersampling
  # se sortea de nuevo con cada semilla (adentro del loop)
  mfuture <- data.matrix(dfuture[, campos_buenos, with = FALSE])

  param_final <- modifyList(PARAM$lgbm$param_fijos, mejores)

  # semillas: primos de 6 cifras sorteados con la semilla primigenia de esta
  # corrida, con el mismo procedimiento que BASELINE_FINAL.ipynb
  semillas <- sortear_primos(PARAM$semilla_primigenia, PARAM$produccion$semillas)
  PARAM$produccion$lista_semillas <- semillas
  registros_final <- integer(0)

  tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  tb_semillas <- list()

  dir.create(DIR_SEMILLAS, showWarnings = FALSE)
  intento_importancia <- FALSE

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
      # undersampling de los CONTINUA sorteado con la semilla de esta vuelta
      set.seed(semillas[i], kind = "L'Ecuyer-CMRG")
      idx <- which(filas_undersampling(runif(nrow(dataset_train))))
      registros_final <- c(registros_final, length(idx))
      dtrain_final <- lgb.Dataset(
        data = data.matrix(dataset_train[idx, campos_buenos, with = FALSE]),
        label = dataset_train[idx, clase01],
        weight = dataset_train[idx, peso]
      )

      modelo <- lgb.train(data = dtrain_final, param = param_final)
      prob <- predict(modelo, mfuture)
      t_semilla <- minutos(t0)

      # la prediccion se guarda primero: es lo que no se puede perder
      # clase_ternaria va en el archivo: el analisis la necesita para la
      # ganancia, tambien la de Exp_0 (que no la guarda)
      fwrite_atomico(data.table(numero_de_cliente = tb_prediccion$numero_de_cliente,
                                clase_ternaria = tb_prediccion$clase_ternaria,
                                prob = prob,
                                minutos = t_semilla,
                                num_iterations = mejores$num_iterations,
                                num_leaves = mejores$num_leaves,
                                min_sum_hessian_in_leaf =
                                  mejores$min_sum_hessian_in_leaf),
                     file = arch_semilla, sep = "\t")
      respaldar(sp)

      # la importancia es informativa: si falla, se avisa y el semilleo sigue
      if (!file.exists("impo.txt") && !intento_importancia) {
        intento_importancia <- TRUE
        tryCatch(
          fwrite(importancia_lgbm(modelo, "modelo_tmp.txt"),
                 file = "impo.txt", sep = "\t"),
          error = function(e) {
            warning("No se pudo calcular la importancia: ", conditionMessage(e))
          }
        )
      }

      rm(modelo, dtrain_final, idx)
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

  PARAM$out$bajas_en_11000 <- as.list(
    tb_prediccion[order(-prob)][1:min(11000L, .N), .N, clase_ternaria][
      , setNames(N, clase_ternaria)])

  PARAM$out$cantidad_columnas <- length(campos_buenos)
  PARAM$out$registros_training_total <- nrow(dataset_train)
  # registros del training final de las semillas entrenadas en esta corrida
  if (length(registros_final)) {
    PARAM$out$registros_training_final_promedio <- mean(registros_final)
  }
  # tiempos de reloj de esta corrida (si se retomo, no incluyen la anterior)
  # y suma de los tiempos de entrenamiento, que si incluyen lo retomado
  PARAM$out$minutos <- list(
    preprocesamiento = t_preproc,
    optimizacion = t_bo,
    produccion = t_prod,
    total = minutos(t_corrida),
    entrenamiento_bo = sum(tb_bayesiana$minutos),
    entrenamiento_produccion = sum(tb_semillas$minutos)
  )

  write_yaml(PARAM, file = "PARAM.yml")
  respaldar(sp)

  cat("\n==== ", PARAM$experimento, " - semilla primigenia ", sp, " ====\n",
      "Peso BAJA+1: ", PARAM$peso_baja1, "\n",
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
      "Tiempo total de esta corrida (min): ", round(minutos(t_corrida), 1), "\n",
      sep = "")

  fila_corrida(PARAM)

}

cat("Corridas de este proceso: ", paste(CORRIDAS_A_CORRER, collapse = ","),
    " (de ", PARAM$corridas, ")\n", sep = "")
tb_corridas <- rbindlist(lapply(CORRIDAS_A_CORRER, function(k) {
  correr(PARAM$semillas_primigenias[k], k)
}))
setwd(PATH_EXP)
fwrite(tb_corridas, file = ARCH_RESUMEN, sep = "\t")
respaldar()

cat("\n==== ", PARAM$experimento, ": ", nrow(tb_corridas), " corridas ====\n", sep = "")
print(tb_corridas)
cat("Ganancia maxima del ensamble, media entre corridas: ",
    format(round(mean(tb_corridas$ganancia_maxima)), big.mark = ".", decimal.mark = ","),
    "  desvio: ",
    format(round(sd(tb_corridas$ganancia_maxima)), big.mark = ".", decimal.mark = ","),
    "\nTiempo de entrenamiento total (min): ",
    round(sum(tb_corridas$minutos_entrenamiento), 1),
    "\nTiempo total de esta ejecucion (min): ", round(minutos(t_inicio), 1), "\n",
    sep = "")

format(Sys.time(), "%a %b %d %X %Y")
