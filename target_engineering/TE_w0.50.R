# Target Engineering - Grupo B - peso de BAJA+1 = 0.5 (ponderacion)
#
# Unica diferencia con el baseline (Exp_0): BAJA+1 entra al entrenamiento
# con peso 0.5 (BAJA+2 y CONTINUA con peso 1). Todo lo demas, incluidos
# meses (train 201901-202104, test 202106), BO y semillas, esta en te_nucleo.R.
#
# Uso:  Rscript TE_w0.50.R
# Resultado en exp/TE_w0.50/ y log en exp/TE_w0.50.log.
# Si se corta, volver a correr: retoma de los checkpoints.

PESO_BAJA1 <- 0.5

arg_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
dir_script <- if (length(arg_file)) dirname(normalizePath(sub("^--file=", "", arg_file))) else getwd()
nucleo <- file.path(dir_script, "te_nucleo.R")
log_dir <- file.path(path.expand(Sys.getenv("DMEYF_HOME", "~/dmeyf")), "exp")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
log <- file.path(log_dir, sprintf("TE_w%.2f.log", PESO_BAJA1))

cat(format(Sys.time()), " peso", PESO_BAJA1, "-> log en", log, "\n")
estado <- system2("Rscript", c(shQuote(nucleo), PESO_BAJA1), stdout = log, stderr = log)
if (estado != 0) stop("Fallo la corrida (ver ", log, ")")
cat(format(Sys.time()), " peso", PESO_BAJA1, "terminado\n")
