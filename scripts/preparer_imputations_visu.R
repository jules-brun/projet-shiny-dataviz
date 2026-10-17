# Prépare les données imputées et l'AFDM utilisées par Visu/graphes_visu-heart.Rmd.
# Exécution depuis la racine du projet :
# Rscript scripts/preparer_imputations_visu.R
# Sorties : Visu/dataset/heart_imputations.rds, Visu/dataset/heart_afdm.rds
#
# Reprend le moteur de l'application (R/benchmark_imputation.R,
# R/pipeline_application.R, R/afdm_application.R) avec les meilleurs modèles du
# benchmark sauvegardé, pour que le Rmd n'ait besoin que de ggplot2 et ggrepel.
# Sans FactoMineR, ecg_repos est imputé par RandomForest (même rang 1 que l'AFDM
# dans meilleurs_modeles.csv) et l'AFDM n'est pas calculée.

invisible(capture.output(suppressWarnings({
  env_import <- new.env()
  pdf(NULL)
  sys.source("R/import.R", envir = env_import)
  dev.off()
})))
source("R/design.R", encoding = "UTF-8")
source("R/benchmark_imputation.R", encoding = "UTF-8")
source("R/pipeline_application.R", encoding = "UTF-8")
avec_afdm <- requireNamespace("FactoMineR", quietly = TRUE)

# Même préparation que server.R : diagnostic binaire, codes 0-4 conservés à part.
d <- env_import$heart
d$diagnostic_initial <- d$diagnostic
d$diagnostic <- as.integer(d$diagnostic > 0)

benchmark <- charger_benchmark_app()
selections <- as.list(benchmark$defauts)
if (!avec_afdm && selections$ecg_repos == "AFDM") selections$ecg_repos <- "RandomForest"

# Une imputation de référence et 20 jeux bootstrap (imputations multiples de l'application).
m <- 20L
imputations <- calculer_imputations_app(d, selections, benchmark, m = m,
  progression = function(i, total) message("Jeu ", i, " / ", total))

variables <- benchmark$cibles
garder <- function(x) x[c(variables, "provenance", "diagnostic")]
saveRDS(list(observe = garder(d), simple = garder(imputations$simple),
             jeux = lapply(imputations$jeux, garder), modeles = unlist(selections),
             m = m, seed = imputations$seed),
        "Visu/dataset/heart_imputations.rds")
message("Écrit : Visu/dataset/heart_imputations.rds")

if (!avec_afdm) {
  message("FactoMineR absent : AFDM non calculée.")
} else {
  # AFDM de l'application : 13 mesures actives, diagnostic et provenance supplémentaires,
  # jeu de référence ajusté une fois, les 20 jeux imputés projetés dans le même repère.
  res <- calculer_afdm_app(imputations)
  axes <- 1:5
  # Tableau long : une ligne par point et par jeu imputé, sur les 5 premières dimensions.
  nuage <- function(vue) {
    z <- nuage_acm_app(res, vue, c(1L, 2L))[c("id", "jeu")]
    coords <- do.call(rbind, lapply(res[[vue]], function(mat) mat[, axes, drop = FALSE]))
    colnames(coords) <- paste0("dim", axes)
    cbind(z, coords)
  }
  libelles_variables <- c(libelles_variables_acm, libelles_quantitatives_app)
  modalites <- nuage("modalites")
  modalites$variable <- sub(" :.*$", "", modalites$id)
  modalites$libelle <- libeller_points_acm(modalites$id)
  associations <- nuage("associations")
  associations$libelle <- unname(libelles_variables[associations$id])
  individus <- nuage("individus")
  individus$diagnostic <- res$diagnostic[as.integer(individus$id)]
  individus$provenance <- res$provenance[as.integer(individus$id)]
  individus$impute <- res$impute[as.integer(individus$id)]
  quanti <- as.data.frame(res$ref$quanti.var$coord[, axes, drop = FALSE])
  names(quanti) <- paste0("dim", axes)
  quanti$variable <- rownames(quanti)
  quanti$libelle <- unname(libelles_quantitatives_app[quanti$variable])
  # Indicateurs des modalités actives sur l'AFDM de référence, pour chaque dimension :
  # projection (coord), qualité de représentation (cos2), contribution, valeur-test.
  q <- res$ref$quali.var
  indicateurs <- do.call(rbind, lapply(axes, function(k) data.frame(
    id = rownames(q$coord), dimension = k, coord = q$coord[, k], cos2 = q$cos2[, k],
    contrib = q$contrib[, k], v_test = q$v.test[, k], row.names = NULL)))
  # Contrôle : diagnostic et provenance doivent rester supplémentaires.
  stopifnot(identical(names(res$ref$call$X)[res$ref$call$sup.var], c("diagnostic", "provenance")),
            !any(grepl("^(diagnostic|provenance) :", indicateurs$id)))
  saveRDS(list(inertie = data.frame(dimension = seq_len(nrow(res$ref$eig)),
                                    valeur_propre = res$ref$eig[, 1], pct = res$ref$eig[, 2]),
               associations = associations, quanti = quanti, modalites = modalites,
               indicateurs_modalites = indicateurs,
               individus = individus, supplementaires = res$supplementaires,
               variables = res$variables, m = res$m),
          "Visu/dataset/heart_afdm.rds")
  message("Écrit : Visu/dataset/heart_afdm.rds")
}
