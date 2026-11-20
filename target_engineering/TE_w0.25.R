# Target Engineering - Grupo B - peso de BAJA+1 = 0.25 (ponderacion)
#
# Unica diferencia con el baseline (Exp_0): BAJA+1 entra al entrenamiento
# con peso 0.25 (BAJA+2 y CONTINUA con peso 1). Todo lo demas, incluidos
# meses (train 201901-202104, test 202106), BO y semillas, esta en te_nucleo.R.
#
# Uso:  Rscript TE_w0.25.R [corridas]     ej: Rscript TE_w0.25.R 1-5
# Sin corridas hace las 10. Resultado en exp/TE_w0.25/ y log en
# exp/TE_w0.25.log (con corridas: exp/TE_w0.25_c<corridas>.log).
# Si se corta, volver a correr: retoma de los checkpoints.

PESO_BAJA1 <- 0.25

arg_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
dir_script <- if (length(arg_file)) dirname(normalizePath(sub("^--file=", "", arg_file))) else getwd()
nucleo <- file.path(dir_script, "te_nucleo.R")
log_dir <- file.path(path.expand(Sys.getenv("DMEYF_HOME", "~/dmeyf")), "exp")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
CORRIDAS <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(CORRIDAS)) CORRIDAS <- ""
log <- file.path(log_dir, sprintf("TE_w%.2f%s.log", PESO_BAJA1,
                                  if (CORRIDAS == "") "" else paste0("_c", CORRIDAS)))

cat(format(Sys.time()), " peso", PESO_BAJA1, "-> log en", log, "\n")
estado <- system2("Rscript", c(shQuote(nucleo), PESO_BAJA1, if (CORRIDAS != "") CORRIDAS),
                  stdout = log, stderr = log)
if (estado != 0) stop("Fallo la corrida (ver ", log, ")")
cat(format(Sys.time()), " peso", PESO_BAJA1, "terminado\n")
