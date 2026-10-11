# Rscript scripts/entrainer_ml.R — calcul hors Shiny, reprise sans nouvelle évaluation TEST.
Sys.setenv(TZ = "Europe/Paris")
source("R/benchmark_imputation.R")
source("R/ml_preparation.R")
source("R/ml_evaluation.R")
source("R/ml_entrainement.R")
import <- new.env(parent = globalenv())
grDevices::pdf(file = NULL)
invisible(capture.output(sys.source("R/import.R", envir = import)))
grDevices::dev.off()
d <- import$heart
d$diagnostic <- as.integer(d$diagnostic > 0)
types <- readRDS("resultats/benchmark_imputation/configuration.rds")$types
# La provenance est uniquement une métadonnée de stratification/évaluation.
types <- types[setdiff(names(types), "provenance")]
resultat <- entrainer_experience_ml(d, types)
cat("Résultats enregistrés dans resultats/ml/evaluation_finale.rds\n")
