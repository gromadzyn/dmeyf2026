# Recupera las evaluaciones de la BO desde el log de una corrida de la
# version anterior de lightgbm_bo.R (sin checkpoints) que se corto, y arma
# bo_evaluaciones.txt para que la version actual retome desde ahi.
#
# Uso:  Rscript log_a_checkpoint.R [log] [carpeta_experimento]
#   por defecto: ~/dmeyf/Exp_0.log  ~/dmeyf/exp/Exp_0

library(data.table)

args <- commandArgs(trailingOnly = TRUE)
arch_log <- path.expand(if (length(args) >= 1) args[1] else "~/dmeyf/Exp_0.log")
dir_exp <- path.expand(if (length(args) >= 2) args[2] else "~/dmeyf/exp/Exp_0")
destino <- file.path(dir_exp, "bo_evaluaciones.txt")

if (file.exists(destino)) stop("Ya existe ", destino, "; no lo piso.")

lineas <- grep(" AUC .* num_iterations=", readLines(arch_log), value = TRUE)
if (length(lineas) == 0) stop("No hay evaluaciones de la BO en ", arch_log)

extraer <- function(patron) sub(patron, "\\1", lineas)
x <- data.table(
  num_iterations = as.integer(extraer(".*num_iterations=([0-9]+).*")),
  num_leaves = as.integer(extraer(".*num_leaves=([0-9]+).*")),
  min_sum_hessian_in_leaf =
    as.numeric(extraer(".*min_sum_hessian_in_leaf=([0-9.eE+-]+).*")),
  auc = as.numeric(extraer(".* AUC ([0-9.]+) .*")),
  minutos = as.numeric(extraer(".*\\(([0-9.]+) min\\).*"))
)

dir.create(dir_exp, showWarnings = FALSE, recursive = TRUE)
fwrite(x, destino, sep = "\t")
print(x)
cat("Guardadas", nrow(x), "evaluaciones en", destino, "\n")
