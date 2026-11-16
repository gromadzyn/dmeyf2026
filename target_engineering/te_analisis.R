# Analisis de Target Engineering - Grupo B
#
# Lee las predicciones por semilla (semillas/pred_<semilla>.txt) de Exp_0
# (w = 1, control) y de cada exp/TE_w<peso>/, todas sobre el test 202106.
# No entrena nada y se puede correr en cualquier momento: usa las semillas
# que ya terminaron, pareadas por numero de semilla.
#
# Sigue la diapositiva 5 de la propuesta (Experimento_colaborativo_3min).
#
# Familia 1 - pesos de BAJA+1
#   ganancia de cada semilla = ganancia maxima de esa semilla en 202106
#   ganancia media = promedio de las semillas (como el bot de la catedra)
#   Wilcoxon pareado por semilla contra Exp_0, unilateral (w gana),
#   corregido por Holm sobre todos los pesos comparados
#   H1 se acepta si algun w < 1 gana con p corregido < 0.05
#   Fase 2 (condicional): si H1 se acepta, se sugiere probar w +- 0.1
#
# Familia 2 - ensamble del baseline con el mejor peso, semilla por semilla
#   mejor peso = el w != 1 con mayor ganancia media
#   score = alfa * rank(Exp_0) + (1 - alfa) * rank(w), alfa en 0.1..0.9
#   Wilcoxon contra Exp_0 y contra w solo, cada uno corregido por Holm
#   sobre los 9 alfas. H2 se acepta si algun alfa gana a los dos.
#   OJO: el mejor w se elige en el mismo mes que se evalua (202106), asi
#   que H2 esta sesgada a favor. La grilla de todos los w va a un archivo
#   aparte, solo descriptiva.
#
# Mecanismo: en el corte donde cada semilla alcanza su ganancia maxima,
#   cuantos envios son BAJA+1 y cuantos BAJA+2
#
# Bordes: avisa si algun hiperparametro optimo quedo contra el borde
#   del rango de la BO
#
# Uso:  Rscript te_analisis.R

library(data.table)

PATH_EXP <- file.path(path.expand(Sys.getenv("DMEYF_HOME", "~/dmeyf")), "exp")
PATH_SALIDA <- file.path(PATH_EXP, "TE_analisis")
dir.create(PATH_SALIDA, showWarnings = FALSE, recursive = TRUE)

EXP_BASE <- "Exp_0" # w = 1
PESOS <- c(0.75, 0.5, 0.25, 0, 1.5)
ALFAS <- seq(0.1, 0.9, by = 0.1)
NIVEL <- 0.05

# bordes de la BO (los mismos de te_nucleo.R y lightgbm_bo.R)
BORDES <- list(num_iterations = c(64, 4096), num_leaves = c(16, 2048),
               min_sum_hessian_in_leaf = c(1e-06, 0.1))
TOL_BORDE <- 0.05 # a menos del 5% del rango (en escala log para el hessian)

GAN_ACIERTO <- 1072500
GAN_ERROR <- -27500

ganancia_maxima <- function(score, clase_ternaria) {
  orden <- order(-score)
  gan <- cumsum(ifelse(clase_ternaria[orden] == "BAJA+2", GAN_ACIERTO, GAN_ERROR))
  pos <- which.max(gan)
  list(ganancia = gan[pos], envios = pos)
}

# lectura de las predicciones --------------------------------------------

# devuelve data.table numero_de_cliente + una columna s<semilla> por semilla
# Solo usa las semillas entrenadas con los hiperparametros de la BO actual,
# que son los de la semilla escrita mas recientemente. Las que quedaron de
# una corrida anterior (por ejemplo, copiadas de la BO de 22 evaluaciones)
# se ignoran hasta que el script las reentrena.
HIPER <- c("num_iterations", "num_leaves", "min_sum_hessian_in_leaf")

leer_semillas <- function(exp) {
  archs <- Sys.glob(file.path(PATH_EXP, exp, "semillas", "pred_*.txt"))
  if (!length(archs)) return(NULL)
  archs <- archs[order(file.mtime(archs), decreasing = TRUE)]
  hiper <- lapply(archs, function(a) fread(a, select = HIPER, nrows = 1))
  vigentes <- sapply(hiper, function(h) isTRUE(all.equal(h, hiper[[1]])))
  if (any(!vigentes)) {
    cat(exp, ": se ignoran ", sum(!vigentes),
        " semillas entrenadas con otros hiperparametros\n", sep = "")
  }
  archs <- archs[vigentes]
  out <- NULL
  for (a in archs) {
    s <- paste0("s", sub("^pred_([0-9]+)\\.txt$", "\\1", basename(a)))
    p <- fread(a, select = c("numero_de_cliente", "prob"))
    setnames(p, "prob", s)
    out <- if (is.null(out)) p else merge(out, p, by = "numero_de_cliente")
  }
  out
}

nombre_exp <- function(w) if (w == 1) EXP_BASE else sprintf("TE_w%.2f", w)

preds <- list()
for (w in c(1, PESOS)) {
  p <- leer_semillas(nombre_exp(w))
  if (is.null(p)) cat("Sin semillas terminadas todavia:", nombre_exp(w), "\n")
  else preds[[as.character(w)]] <- p
}
if (is.null(preds[["1"]])) stop("Hace falta al menos una semilla de ", EXP_BASE)

# clase_ternaria de 202106: Exp_0 no la guarda, la tomo de un TE
arch_clase <- Sys.glob(file.path(PATH_EXP, "TE_w*", "semillas", "pred_*.txt"))
if (!length(arch_clase)) stop("Hace falta al menos una semilla de algun TE_w*")
clases <- fread(arch_clase[1], select = c("numero_de_cliente", "clase_ternaria"))

preds <- lapply(preds, function(p) merge(clases, p, by = "numero_de_cliente"))
semillas_de <- function(p) grep("^s[0-9]+$", colnames(p), value = TRUE)
cat("Semillas por brazo:",
    paste0(names(preds), "=", sapply(preds, function(p) length(semillas_de(p))),
           collapse = "  "), "\n")

# Familia 1 ---------------------------------------------------------------

tb_semillas <- rbindlist(lapply(names(preds), function(w) {
  p <- preds[[w]]
  rbindlist(lapply(semillas_de(p), function(s) {
    g <- ganancia_maxima(p[[s]], p$clase_ternaria)
    corte <- p$clase_ternaria[order(-p[[s]])][seq_len(g$envios)]
    data.table(peso = as.numeric(w), semilla = s,
               ganancia = g$ganancia, envios = g$envios,
               baja1_en_corte = sum(corte == "BAJA+1"),
               baja2_en_corte = sum(corte == "BAJA+2"))
  }))
}))

# Wilcoxon pareado por semilla, unilateral: a gana a ref
comparar <- function(a, ref) {
  x <- merge(a[, list(semilla, g = ganancia)], ref[, list(semilla, g0 = ganancia)],
             by = "semilla")
  if (nrow(x) < 2) return(data.table(pares = nrow(x), dif_media = NA_real_,
                                     p_valor = NA_real_))
  data.table(
    pares = nrow(x),
    dif_media = mean(x$g - x$g0),
    p_valor = if (all(x$g == x$g0)) 1 else suppressWarnings(
      wilcox.test(x$g, x$g0, paired = TRUE, alternative = "greater")$p.value)
  )
}

base <- tb_semillas[peso == 1]

resumen_f1 <- tb_semillas[, list(semillas = .N,
                                 ganancia_media = mean(ganancia),
                                 desvio = sd(ganancia),
                                 envios_medio = mean(envios),
                                 baja1_en_corte = mean(baja1_en_corte),
                                 baja2_en_corte = mean(baja2_en_corte)),
                          peso]
setorder(resumen_f1, -ganancia_media)

test_f1 <- rbindlist(lapply(setdiff(as.numeric(names(preds)), 1), function(w) {
  cbind(peso = w, comparar(tb_semillas[peso == w], base))
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
cat("Con 10 pares el p minimo de Wilcoxon es 1/1024; Holm sobre", nrow(test_f1),
    "pesos exige p sin corregir <=", signif(NIVEL / max(1, nrow(test_f1)), 2),
    "para el mejor.\n")
cat(if (nrow(test_f1) && any(test_f1$acepta_H1)) "H1 se ACEPTA" else
      "H1 NO se acepta (todavia)", "\n")

if (nrow(test_f1) && any(test_f1$acepta_H1)) {
  mejor_w <- test_f1[acepta_H1 == TRUE][which.max(dif_media), peso]
  refinar <- setdiff(round(pmax(0, mejor_w + c(-0.1, 0.1)), 2), c(1, PESOS))
  cat("\nFase 2: w =", mejor_w, "gana con p corregido <", NIVEL, "-> refinar en",
      paste(refinar, collapse = " y "), "\n")
}

# Familia 2 ---------------------------------------------------------------

p0 <- preds[["1"]]
ensamblar <- function(w) {
  pw <- preds[[w]]
  x <- merge(p0, pw[, -"clase_ternaria"], by = "numero_de_cliente",
             suffixes = c(".b", ".w"))
  rbindlist(lapply(intersect(semillas_de(p0), semillas_de(pw)), function(s) {
    r_b <- frank(x[[paste0(s, ".b")]], ties.method = "average")
    r_w <- frank(x[[paste0(s, ".w")]], ties.method = "average")
    rbindlist(lapply(ALFAS, function(a) {
      g <- ganancia_maxima(a * r_b + (1 - a) * r_w, x$clase_ternaria)
      data.table(peso = as.numeric(w), alfa = a, semilla = s,
                 ganancia = g$ganancia, envios = g$envios)
    }))
  }))
}

tb_ensamble <- rbindlist(lapply(setdiff(names(preds), "1"), ensamblar))

if (nrow(tb_ensamble)) {
  # grilla de todos los pesos: solo descriptiva, sin tests
  grilla_f2 <- tb_ensamble[, list(semillas = .N, ganancia_media = mean(ganancia),
                                  envios_medio = mean(envios)), list(peso, alfa)]
  setorder(grilla_f2, -ganancia_media)

  # H2: el mejor peso ponderado (w != 1 con mayor ganancia media)
  mejor_pond <- resumen_f1[peso != 1][which.max(ganancia_media), peso]
  ens <- tb_ensamble[peso == mejor_pond]
  test_f2 <- ens[, {
    cb <- comparar(.SD, base)
    cw <- comparar(.SD, tb_semillas[peso == mejor_pond])
    list(pares = cb$pares, ganancia_media = mean(ganancia),
         dif_vs_base = cb$dif_media, p_vs_base = cb$p_valor,
         dif_vs_w = cw$dif_media, p_vs_w = cw$p_valor)
  }, alfa]
  test_f2[, p_holm_vs_base := p.adjust(p_vs_base, method = "holm")]
  test_f2[, p_holm_vs_w := p.adjust(p_vs_w, method = "holm")]
  test_f2[, gana_a_ambos := !is.na(p_holm_vs_base) & p_holm_vs_base < NIVEL &
            !is.na(p_holm_vs_w) & p_holm_vs_w < NIVEL]

  cat("\n==== Familia 2: ensamble de ", EXP_BASE, " con el mejor peso (w = ",
      mejor_pond, ") ====\n", sep = "")
  print(test_f2)
  cat(if (any(test_f2$gana_a_ambos)) "H2 se ACEPTA" else "H2 NO se acepta (todavia)",
      "\nOJO: el mejor w se eligio en el mismo mes (202106): sesgado a favor.\n")

  fwrite(test_f2, file.path(PATH_SALIDA, "f2_wilcoxon.txt"), sep = "\t")
  fwrite(grilla_f2, file.path(PATH_SALIDA, "f2_grilla_todos_los_pesos.txt"), sep = "\t")
  fwrite(tb_ensamble, file.path(PATH_SALIDA, "f2_ensamble_semillas.txt"), sep = "\t")
}

# Bordes ------------------------------------------------------------------

# hiperparametros optimos de cada brazo: los guarda cada archivo de semilla
tb_bordes <- rbindlist(lapply(c(1, PESOS), function(w) {
  archs <- Sys.glob(file.path(PATH_EXP, nombre_exp(w), "semillas", "pred_*.txt"))
  if (!length(archs)) return(NULL)
  a <- archs[which.max(file.mtime(archs))]
  h <- fread(a, select = HIPER, nrows = 1)
  rbindlist(lapply(HIPER, function(v) {
    b <- BORDES[[v]]
    x <- h[[v]]
    if (v == "min_sum_hessian_in_leaf") { b <- log(b); x <- log(x) }
    pos <- (x - b[1]) / (b[2] - b[1])
    data.table(peso = w, hiperparametro = v, valor = h[[v]],
               posicion_en_rango = round(pos, 3),
               en_borde = pos < TOL_BORDE | pos > 1 - TOL_BORDE)
  }))
}))

if (nrow(tb_bordes)) {
  cat("\n==== Bordes: hiperparametros optimos (posicion 0 = minimo, 1 = maximo) ====\n")
  print(tb_bordes)
  if (any(tb_bordes$en_borde)) {
    cat("ATENCION: hay hiperparametros contra el borde del rango\n")
  }
  fwrite(tb_bordes, file.path(PATH_SALIDA, "bordes.txt"), sep = "\t")
}

fwrite(tb_semillas, file.path(PATH_SALIDA, "f1_semillas.txt"), sep = "\t")
fwrite(resumen_f1, file.path(PATH_SALIDA, "f1_resumen.txt"), sep = "\t")
fwrite(test_f1, file.path(PATH_SALIDA, "f1_wilcoxon.txt"), sep = "\t")
cat("\nResultados en", PATH_SALIDA, "\n")
