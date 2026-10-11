# Contrôles sans accès au TEST de l'expérience réelle.
Sys.setenv(TZ = "Europe/Paris")
source("R/benchmark_imputation.R")
source("R/ml_preparation.R")
source("R/ml_evaluation.R")
source("R/ml_entrainement.R")
config <- configuration_ml()
set.seed(10)
d <- expand.grid(diagnostic = 0:1, provenance = c("A", "B"), patient = 1:20)
s <- split_stratifie_ml(d, .2, 123)
stopifnot(!length(intersect(s$train, s$test)), length(s$test) == 16,
  identical(s, split_stratifie_ml(d, .2, 123)), all(s$strates$train > 0), all(s$strates$test > 0))
f <- folds_stratifies_ml(d[s$train, ], 5, 123)
stopifnot(all(tabulate(f, 5) > 0), length(f) == length(s$train))
rare <- rbind(d, data.frame(diagnostic = 1, provenance = "rare", patient = 1))
r <- split_stratifie_ml(rare, .2, 123)
stopifnot(nrow(rare) %in% r$train)

x <- data.frame(age = c(20, 30, 40, NA, 60, 70), sexe = c(0, 1, 0, 1, 0, 1))
types <- c(age = "quantitative", sexe = "nominale")
prep <- fit_preparation_ml(x, types)
stopifnot(prep$param$age$valeur == 40)
assessment <- data.frame(age = c(1000, NA), sexe = c(2, 0))
a <- appliquer_preparation_ml(prep, assessment)
stopifnot(all(is.finite(a)), prep$param$age$valeur == 40)
# Interdire les prédicteurs de provenance/diagnostic, quelle que soit leur position.
stopifnot(inherits(try(fit_preparation_ml(data.frame(provenance = 1:6), c(provenance = "quantitative")), silent = TRUE), "try-error"))
m <- metriques_prediction_ml(c(0, 0, 1, 1), c(.1, .6, .4, .9), .5)
stopifnot(m[["sensitivity"]] == .5, m[["specificity"]] == .5, m[["roc_auc"]] == .75,
  abs(m[["brier"]] - .185) < 1e-8)
# AUC ne doit pas s'inverser automatiquement pour des prédictions mauvaises.
stopifnot(metriques_prediction_ml(c(0, 1), c(.9, .1))[["roc_auc"]] == 0)

# Vérification inductive de toutes les familles d'imputation, sur des données simulées.
set.seed(11)
x <- data.frame(age = rnorm(80, 55, 10), cholesterol = rnorm(80, 240, 20),
  sexe = sample(0:1, 80, TRUE), douleur = sample(1:4, 80, TRUE))
types <- c(age = "quantitative", cholesterol = "quantitative", sexe = "nominale", douleur = "nominale")
x$age[c(1, 2, 10)] <- NA
for (v in c("cholesterol", "sexe")) for (nom in names(modeles_imputation_ml)) {
  if (types[[v]] != "quantitative" && nom %in% c("mean", "median", "EM")) next
  fit <- fit_imputation_cible_ml(x[1:60, ], v, types[[v]], types, nom, config$parametres_imputation, 5)
  p <- predire_imputation_cible_ml(fit, x[61:80, setdiff(names(x), v)])
  stopifnot(length(p) == 20, all(is.finite(p)))
}
# Une seule préparation de classification, apprise sur ces données artificielles.
xs <- x; xs$age[is.na(xs$age)] <- 55
prep <- fit_preparation_ml(xs[1:60, ], types)
a <- appliquer_preparation_ml(prep, xs[1:60, ]); b <- appliquer_preparation_ml(prep, xs[61:80, ])
y <- rep(0:1, 30)
for (nom in config$modeles$candidat) {
  fit <- fit_classifieur_ml(a, y, nom, 5)
  p <- predire_classifieur_ml(fit, b)
  stopifnot(length(p) == 20, all(is.finite(p)), all(p >= 0 & p <= 1))
}
cat("Validation unitaire ML : splits, folds, absence de prédicteurs interdits, imputations et cinq familles de classification.\n")
