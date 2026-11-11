# Analisis de Target Engineering - Grupo B
#
# Lee las predicciones por semilla (semillas/pred_<semilla>.txt) de Exp_0
# (w = 1, control) y de cada exp/TE_w<peso>/, todas sobre el test 202106.
# No entrena nada y se puede correr en cualquier momento: usa las semillas
# que ya terminaron, pareadas por numero de semilla.
#
# Familia 1 - pesos de BAJA+1
#   ganancia de cada semilla = ganancia maxima de esa semilla en 202106
#   ganancia media = promedio de las semillas (como el bot de la catedra)
#   Wilcoxon pareado por semilla contra Exp_0, unilateral (w gana)
#   H1 se acepta si p < 0.05
#
# Familia 2 - ensamble con el baseline, semilla por semilla
#   score = alfa * rank(Exp_0) + (1 - alfa) * rank(w), alfa en 0.1..0.9
#   Wilcoxon contra Exp_0 y contra w solo. OJO: sin folds, (w, alfa) se
#   elige y se evalua en el mismo mes, asi que el mejor esta sesgado hacia
#   arriba; se informa la grilla completa para que se vea.
#
# Mecanismo: cuantos BAJA+1 y BAJA+2 entran en los primeros 11.000 envios
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
ENVIOS_MECANISMO <- 11000L

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
leer_semillas <- function(exp) {
  archs <- Sys.glob(file.path(PATH_EXP, exp, "semillas", "pred_*.txt"))
  if (!length(archs)) return(NULL)
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
    top <- p$clase_ternaria[order(-p[[s]])][seq_len(min(ENVIOS_MECANISMO, nrow(p)))]
    data.table(peso = as.numeric(w), semilla = s,
               ganancia = g$ganancia, envios = g$envios,
               baja1_en_11000 = sum(top == "BAJA+1"),
               baja2_en_11000 = sum(top == "BAJA+2"))
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
                                 baja1_en_11000 = mean(baja1_en_11000),
                                 baja2_en_11000 = mean(baja2_en_11000)),
                          peso]
setorder(resumen_f1, -ganancia_media)

test_f1 <- rbindlist(lapply(setdiff(as.numeric(names(preds)), 1), function(w) {
  cbind(peso = w, comparar(tb_semillas[peso == w], base))
}))
if (nrow(test_f1)) test_f1[, acepta_H1 := !is.na(p_valor) & p_valor < NIVEL]

cat("\n==== Familia 1: ganancia media por peso (w = 1 es ", EXP_BASE, ") ====\n", sep = "")
print(resumen_f1)
cat("\n==== Familia 1: Wilcoxon pareado contra ", EXP_BASE, " ====\n", sep = "")
print(test_f1)

if (nrow(test_f1) && any(test_f1$acepta_H1)) {
  mejor_w <- test_f1[acepta_H1 == TRUE][which.max(dif_media), peso]
  refinar <- setdiff(round(pmax(0, mejor_w + c(-0.1, 0.1)), 2), c(1, PESOS))
  cat("\nFase 2: w =", mejor_w, "gana con p <", NIVEL, "-> refinar en",
      paste(refinar, collapse = " y "), "\n")
}

# Familia 2 ---------------------------------------------------------------

p0 <- preds[["1"]]
tb_ensamble <- rbindlist(lapply(setdiff(names(preds), "1"), function(w) {
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
}))

if (nrow(tb_ensamble)) {
  test_f2 <- tb_ensamble[, {
    w_ <- .BY$peso
    cb <- comparar(.SD, base)
    cw <- comparar(.SD, tb_semillas[peso == w_])
    list(pares = cb$pares, ganancia_media = mean(ganancia),
         dif_vs_base = cb$dif_media, p_vs_base = cb$p_valor,
         dif_vs_w = cw$dif_media, p_vs_w = cw$p_valor)
  }, list(peso, alfa)]
  test_f2[, gana_a_ambos := !is.na(p_vs_base) & p_vs_base < NIVEL &
            !is.na(p_vs_w) & p_vs_w < NIVEL]
  setorder(test_f2, -ganancia_media)

  cat("\n==== Familia 2: ensamble por rankings (mejores 10 de la grilla) ====\n")
  print(head(test_f2, 10))
  cat("Elegido y evaluado en el mismo mes (202106): el mejor esta sesgado",
      "hacia arriba.\nGrilla completa en", file.path(PATH_SALIDA, "f2_ensamble.txt"), "\n")

  fwrite(test_f2, file.path(PATH_SALIDA, "f2_ensamble.txt"), sep = "\t")
  fwrite(tb_ensamble, file.path(PATH_SALIDA, "f2_ensamble_semillas.txt"), sep = "\t")
}

fwrite(tb_semillas, file.path(PATH_SALIDA, "f1_semillas.txt"), sep = "\t")
fwrite(resumen_f1, file.path(PATH_SALIDA, "f1_resumen.txt"), sep = "\t")
fwrite(test_f1, file.path(PATH_SALIDA, "f1_wilcoxon.txt"), sep = "\t")
cat("\nResultados en", PATH_SALIDA, "\n")
