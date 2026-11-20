# Analisis de Target Engineering - Grupo B
#
# Cada brazo (Exp_0 = w 1, control, y cada exp/TE_w<peso>/) corre 10 veces
# el pipeline completo, una por semilla primigenia (sp_<semilla>/). La
# unidad de analisis es la CORRIDA: su prediccion es el promedio de sus 10
# semillas de produccion (prediccion.txt), sobre el test 202106.
# No entrena nada y se puede correr en cualquier momento: usa las corridas
# que ya terminaron (con PARAM.yml), pareadas por semilla primigenia, que es
# la misma en todos los brazos.
#
# Sigue la diapositiva 5 de la propuesta (Experimento_colaborativo_3min).
#
# Familia 1 - pesos de BAJA+1
#   ganancia de cada corrida = ganancia maxima de su ensamble en 202106
#   ganancia media = promedio de las 10 corridas
#   Wilcoxon pareado por corrida contra Exp_0, unilateral (w gana),
#   corregido por Holm sobre todos los pesos comparados
#   H1 se acepta si algun w < 1 gana con p corregido < 0.05
#   Fase 2 (condicional): si H1 se acepta, se corren w +- 0.1 del mejor.
#   Los pesos de la fase 2 se detectan solos (carpetas exp/TE_w*), se
#   comparan contra Exp_0 aparte, con Holm sobre los pesos de la fase 2,
#   y entran tambien en los ensambles y en el chequeo de bordes.
#
# Familia 2 - ensamble del baseline con CADA peso, corrida por corrida
#   score = alfa * rank(Exp_0) + (1 - alfa) * rank(w), alfa en 0.1..0.9
#   Para cada w: Wilcoxon contra Exp_0 y contra w solo, cada uno corregido
#   por Holm sobre los 9 alfas de ese w.
#   H2 se decide con el mejor peso ponderado (el w != 1 con mayor ganancia
#   media): se acepta si algun alfa le gana a Exp_0 y a w.
#   OJO: el mejor w se elige en el mismo mes que se evalua (202106), asi
#   que H2 esta sesgada a favor.
#
# Mecanismo: en el corte donde cada corrida alcanza su ganancia maxima,
#   cuantos envios son BAJA+1 y cuantos BAJA+2
#
# Bordes: avisa si algun hiperparametro optimo quedo contra el borde
#   del rango de la BO, en cuantas de las corridas de cada brazo
#
# Uso:  Rscript te_analisis.R

library(data.table)

PATH_EXP <- file.path(path.expand(Sys.getenv("DMEYF_HOME", "~/dmeyf")), "exp")
PATH_SALIDA <- file.path(PATH_EXP, "TE_analisis")
dir.create(PATH_SALIDA, showWarnings = FALSE, recursive = TRUE)

EXP_BASE <- "Exp_0" # w = 1
PESOS <- c(0.75, 0.5, 0.25, 0, 1.5) # fase 1 (diseno original)
ALFAS <- seq(0.1, 0.9, by = 0.1)
NIVEL <- 0.05

# bordes de la BO (los mismos de te_nucleo.R y lightgbm_bo.R)
BORDES <- list(num_iterations = c(64, 4096), num_leaves = c(16, 2048),
               min_sum_hessian_in_leaf = c(1e-06, 0.1))
TOL_BORDE <- 0.05 # a menos del 5% del rango (escala lineal, como la BO)

GAN_ACIERTO <- 1072500
GAN_ERROR <- -27500

ganancia_maxima <- function(score, clase_ternaria) {
  orden <- order(-score)
  gan <- cumsum(ifelse(clase_ternaria[orden] == "BAJA+2", GAN_ACIERTO, GAN_ERROR))
  pos <- which.max(gan)
  list(ganancia = gan[pos], envios = pos)
}

# lectura de las predicciones --------------------------------------------

# devuelve data.table numero_de_cliente + una columna s<semilla primigenia>
# por corrida terminada (con PARAM.yml), con la prob promedio de sus semillas
HIPER <- c("num_iterations", "num_leaves", "min_sum_hessian_in_leaf")

corridas_terminadas <- function(exp) {
  dirs <- Sys.glob(file.path(PATH_EXP, exp, "sp_*"))
  dirs[file.exists(file.path(dirs, "PARAM.yml")) &
         file.exists(file.path(dirs, "prediccion.txt"))]
}

leer_corridas <- function(exp) {
  dirs <- corridas_terminadas(exp)
  if (!length(dirs)) return(NULL)
  out <- NULL
  for (d in dirs) {
    s <- paste0("s", sub("^sp_", "", basename(d)))
    p <- fread(file.path(d, "prediccion.txt"), select = c("numero_de_cliente", "prob"))
    setnames(p, "prob", s)
    out <- if (is.null(out)) p else merge(out, p, by = "numero_de_cliente")
  }
  out
}

nombre_exp <- function(w) if (w == 1) EXP_BASE else sprintf("TE_w%.2f", w)

# pesos de la fase 2: cualquier otra carpeta TE_w<peso>
carpetas <- basename(Sys.glob(file.path(PATH_EXP, "TE_w*")))
carpetas <- carpetas[grepl("^TE_w[0-9]+\\.[0-9]{2}$", carpetas)]
PESOS_FASE2 <- setdiff(as.numeric(sub("^TE_w", "", carpetas)), c(1, PESOS))
PESOS_TODOS <- c(PESOS, sort(PESOS_FASE2))
if (length(PESOS_FASE2)) cat("Pesos de la fase 2:", sort(PESOS_FASE2), "\n")

preds <- list()
for (w in c(1, PESOS_TODOS)) {
  p <- leer_corridas(nombre_exp(w))
  if (is.null(p)) cat("Sin corridas terminadas todavia:", nombre_exp(w), "\n")
  else preds[[as.character(w)]] <- p
}
if (is.null(preds[["1"]])) stop("Hace falta al menos una corrida terminada de ", EXP_BASE)

# clase_ternaria de 202106: la guarda cada archivo de semilla
arch_clase <- Sys.glob(file.path(PATH_EXP, c(EXP_BASE, "TE_w*"), "sp_*", "semillas", "pred_*.txt"))
if (!length(arch_clase)) stop("No encuentro ningun semillas/pred_*.txt")
clases <- fread(arch_clase[1], select = c("numero_de_cliente", "clase_ternaria"))

preds <- lapply(preds, function(p) merge(clases, p, by = "numero_de_cliente"))
corridas_de <- function(p) grep("^s[0-9]+$", colnames(p), value = TRUE)
cat("Corridas por brazo:",
    paste0(names(preds), "=", sapply(preds, function(p) length(corridas_de(p))),
           collapse = "  "), "\n")

# Familia 1 ---------------------------------------------------------------

tb_corridas <- rbindlist(lapply(names(preds), function(w) {
  p <- preds[[w]]
  rbindlist(lapply(corridas_de(p), function(s) {
    g <- ganancia_maxima(p[[s]], p$clase_ternaria)
    corte <- p$clase_ternaria[order(-p[[s]])][seq_len(g$envios)]
    data.table(peso = as.numeric(w), corrida = s,
               ganancia = g$ganancia, envios = g$envios,
               baja1_en_corte = sum(corte == "BAJA+1"),
               baja2_en_corte = sum(corte == "BAJA+2"))
  }))
}))

# Wilcoxon pareado por corrida (semilla primigenia), unilateral: a gana a ref
comparar <- function(a, ref) {
  x <- merge(a[, list(corrida, g = ganancia)], ref[, list(corrida, g0 = ganancia)],
             by = "corrida")
  if (nrow(x) < 2) return(data.table(pares = nrow(x), dif_media = NA_real_,
                                     p_valor = NA_real_))
  data.table(
    pares = nrow(x),
    dif_media = mean(x$g - x$g0),
    p_valor = if (all(x$g == x$g0)) 1 else suppressWarnings(
      wilcox.test(x$g, x$g0, paired = TRUE, alternative = "greater")$p.value)
  )
}

base <- tb_corridas[peso == 1]

resumen_f1 <- tb_corridas[, list(corridas = .N,
                                 ganancia_media = mean(ganancia),
                                 desvio = sd(ganancia),
                                 envios_medio = mean(envios),
                                 baja1_en_corte = mean(baja1_en_corte),
                                 baja2_en_corte = mean(baja2_en_corte)),
                          peso]
setorder(resumen_f1, -ganancia_media)

test_f1 <- rbindlist(lapply(intersect(PESOS, as.numeric(names(preds))), function(w) {
  cbind(peso = w, comparar(tb_corridas[peso == w], base))
}))
if (nrow(test_f1)) {
  test_f1[, p_holm := p.adjust(p_valor, method = "holm")]
  test_f1[, gana := !is.na(p_holm) & p_holm < NIVEL]
  test_f1[, acepta_H1 := gana & peso < 1]
}

cat("\n==== Familia 1: ganancia media por peso (w = 1 es ", EXP_BASE, ") ====\n", sep = "")
print(resumen_f1)
cat("\n==== Familia 1: Wilcoxon pareado contra ", EXP_BASE, " ====\n", sep = "")
print(test_f1)
cat("Con 10 corridas pareadas el p minimo de Wilcoxon es 1/1024; Holm sobre", nrow(test_f1),
    "pesos exige p sin corregir <=", signif(NIVEL / max(1, nrow(test_f1)), 2),
    "para el mejor.\n")
cat(if (nrow(test_f1) && any(test_f1$acepta_H1)) "H1 se ACEPTA" else
      "H1 NO se acepta (todavia)", "\n")

if (nrow(test_f1) && any(test_f1$acepta_H1)) {
  mejor_w <- test_f1[acepta_H1 == TRUE][which.max(dif_media), peso]
  refinar <- setdiff(round(pmax(0, mejor_w + c(-0.1, 0.1)), 2), c(1, PESOS))
  cat("\nFase 2: w =", mejor_w, "gana con p corregido <", NIVEL, "-> correr",
      paste(sprintf("TE_w%.2f", refinar), collapse = " y "), "\n")
}

# Fase 2: contra Exp_0, con Holm sobre los pesos de la fase 2
w_f2 <- intersect(PESOS_FASE2, as.numeric(names(preds)))
if (length(w_f2)) {
  test_fase2 <- rbindlist(lapply(w_f2, function(w) {
    cbind(peso = w, comparar(tb_corridas[peso == w], base))
  }))
  test_fase2[, p_holm := p.adjust(p_valor, method = "holm")]
  test_fase2[, gana := !is.na(p_holm) & p_holm < NIVEL]
  cat("\n==== Fase 2: Wilcoxon pareado contra ", EXP_BASE, " ====\n", sep = "")
  print(test_fase2)
  fwrite(test_fase2, file.path(PATH_SALIDA, "fase2_wilcoxon.txt"), sep = "\t")
}

# Familia 2 ---------------------------------------------------------------

p0 <- preds[["1"]]
ensamblar <- function(w) {
  pw <- preds[[w]]
  x <- merge(p0, pw[, -"clase_ternaria"], by = "numero_de_cliente",
             suffixes = c(".b", ".w"))
  rbindlist(lapply(intersect(corridas_de(p0), corridas_de(pw)), function(s) {
    r_b <- frank(x[[paste0(s, ".b")]], ties.method = "average")
    r_w <- frank(x[[paste0(s, ".w")]], ties.method = "average")
    rbindlist(lapply(ALFAS, function(a) {
      g <- ganancia_maxima(a * r_b + (1 - a) * r_w, x$clase_ternaria)
      data.table(peso = as.numeric(w), alfa = a, corrida = s,
                 ganancia = g$ganancia, envios = g$envios)
    }))
  }))
}

tb_ensamble <- rbindlist(lapply(setdiff(names(preds), "1"), ensamblar))

if (nrow(tb_ensamble)) {
  # tests para cada peso y cada alfa; Holm sobre los 9 alfas de cada peso
  test_f2 <- tb_ensamble[, {
    w_ <- .BY$peso
    cb <- comparar(.SD, base)
    cw <- comparar(.SD, tb_corridas[peso == w_])
    list(pares = cb$pares, ganancia_media = mean(ganancia),
         envios_medio = mean(envios),
         dif_vs_base = cb$dif_media, p_vs_base = cb$p_valor,
         dif_vs_w = cw$dif_media, p_vs_w = cw$p_valor)
  }, list(peso, alfa)]
  test_f2[, p_holm_vs_base := p.adjust(p_vs_base, method = "holm"), peso]
  test_f2[, p_holm_vs_w := p.adjust(p_vs_w, method = "holm"), peso]
  test_f2[, gana_a_ambos := !is.na(p_holm_vs_base) & p_holm_vs_base < NIVEL &
            !is.na(p_holm_vs_w) & p_holm_vs_w < NIVEL]
  setorder(test_f2, peso, alfa)

  # resumen por peso: el mejor alfa de cada w
  resumen_f2 <- test_f2[order(-ganancia_media), .SD[1], peso]
  resumen_f2[, alfas_que_ganan_a_ambos := test_f2[, sum(gana_a_ambos), peso][
    match(resumen_f2$peso, peso), V1]]
  setorder(resumen_f2, -ganancia_media)

  cat("\n==== Familia 2: ensamble de ", EXP_BASE, " con cada peso (mejor alfa de cada w) ====\n",
      sep = "")
  print(resumen_f2[, list(peso, mejor_alfa = alfa, ganancia_media, dif_vs_base,
                          p_holm_vs_base, dif_vs_w, p_holm_vs_w,
                          alfas_que_ganan_a_ambos)])

  # H2: el mejor peso ponderado
  mejor_pond <- resumen_f1[peso != 1][which.max(ganancia_media), peso]
  cat("\n==== Familia 2 / H2: mejor peso ponderado (w = ", mejor_pond, "), todos los alfas ====\n",
      sep = "")
  print(test_f2[peso == mejor_pond])
  cat(if (any(test_f2[peso == mejor_pond, gana_a_ambos])) "H2 se ACEPTA" else
        "H2 NO se acepta (todavia)",
      "\nOJO: el mejor w se eligio en el mismo mes (202106): sesgado a favor.\n")

  fwrite(test_f2, file.path(PATH_SALIDA, "f2_wilcoxon.txt"), sep = "\t")
  fwrite(resumen_f2, file.path(PATH_SALIDA, "f2_resumen.txt"), sep = "\t")
  fwrite(tb_ensamble, file.path(PATH_SALIDA, "f2_ensamble_corridas.txt"), sep = "\t")
}

# Bordes ------------------------------------------------------------------

# hiperparametros optimos de cada corrida de cada brazo (su PARAM.yml)
tb_bordes_corridas <- rbindlist(lapply(c(1, PESOS_TODOS), function(w) {
  rbindlist(lapply(corridas_terminadas(nombre_exp(w)), function(d) {
    h <- yaml::read_yaml(file.path(d, "PARAM.yml"))$out$bo$mejores_hiperparametros
    rbindlist(lapply(HIPER, function(v) {
      b <- BORDES[[v]]
      pos <- (h[[v]] - b[1]) / (b[2] - b[1])
      data.table(peso = w, corrida = basename(d), hiperparametro = v, valor = h[[v]],
                 posicion_en_rango = round(pos, 3),
                 en_borde = pos < TOL_BORDE | pos > 1 - TOL_BORDE)
    }))
  }))
}))
tb_bordes <- if (nrow(tb_bordes_corridas)) {
  tb_bordes_corridas[, list(corridas = .N, mediana = median(valor),
                            posicion_mediana = median(posicion_en_rango),
                            corridas_en_borde = sum(en_borde),
                            en_borde = sum(en_borde) > 0),
                     list(peso, hiperparametro)]
} else data.table()

if (nrow(tb_bordes)) {
  cat("\n==== Bordes: hiperparametros optimos (posicion 0 = minimo, 1 = maximo) ====\n")
  print(tb_bordes)
  if (any(tb_bordes$en_borde)) {
    cat("ATENCION: hay hiperparametros contra el borde del rango\n")
  }
  fwrite(tb_bordes, file.path(PATH_SALIDA, "bordes.txt"), sep = "\t")
  fwrite(tb_bordes_corridas, file.path(PATH_SALIDA, "bordes_corridas.txt"), sep = "\t")
}

fwrite(tb_corridas, file.path(PATH_SALIDA, "f1_corridas.txt"), sep = "\t")
fwrite(resumen_f1, file.path(PATH_SALIDA, "f1_resumen.txt"), sep = "\t")
fwrite(test_f1, file.path(PATH_SALIDA, "f1_wilcoxon.txt"), sep = "\t")
cat("\nResultados en", PATH_SALIDA, "\n")
