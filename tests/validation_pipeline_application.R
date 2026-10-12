# Rscript tests/validation_pipeline_application.R
Sys.setenv(TZ = "Europe/Paris")
options(sass.cache = FALSE)
invisible(capture.output(serveur <- source("server.R")$value))
original <- donnees_uci
empreintes <- tools::md5sum(file.path("dataset", unname(import_uci$fichiers)))
html <- as.character(source("ui.R")$value)
stopifnot(all(vapply(c("donnees", "manquantes", "acm"), function(v)
  grepl(paste0('data-value="', v, '"'), html, fixed = TRUE), logical(1))),
  !grepl('data-value="overview"|data-value="relations"|data-value="associations"|data-value="apropos"', html),
  !grepl("UCI HEART DISEASE · EXPLORATION CLINIQUE", html, fixed = TRUE),
  grepl("Diagnostic cardiaque et prédiction", html, fixed = TRUE),
  grepl("Justification bibliographique", html, fixed = TRUE),
  import_uci$bilan_nettoyage$n_doublons_retires == 2)
# Les distributions intermédiaires gardent les zéros ; les ? sont déjà des NA.
stopifnot(sum(donnees_distribution$cholesterol == 0, na.rm = TRUE) == 172,
          sum(donnees_distribution$pa_repos == 0, na.rm = TRUE) == 1,
          !any(donnees_uci$cholesterol == 0, na.rm = TRUE),
          !any(donnees_uci$pa_repos == 0, na.rm = TRUE))
for (v in import_uci$colonnes) {
  disponibilite <- compter_disponibilite_app(original, v)
  stopifnot(sum(disponibilite$n) == nrow(original),
    sum(disponibilite$n[disponibilite$statut == "NA"]) == sum(is.na(original[[v]])))
  for (centre in names(centres_app)) {
    nb <- sum(original$provenance == centre)
    stopifnot(sum(disponibilite$n[disponibilite$provenance == centre]) == nb)
  }
}
# Les surfaces des cartes et graphiques ont un fond sombre explicite.
css <- paste(readLines("www/style.css"), collapse = "\n")
stopifnot(grepl(".card > .card-body", css, fixed = TRUE),
          grepl("font-size:17px", css, fixed = TRUE))
# Tester les vrais modèles gagnants, les cinq jeux et leur projection.
imp <- calculer_imputations_app(original, benchmark_app$defauts, benchmark_app, m = 5)
stopifnot(length(imp$jeux) == 5)
for (jeu in c(list(imp$simple), imp$jeux)) {
  stopifnot(!anyNA(jeu[names(benchmark_app$types)]),
    identical(jeu$diagnostic, original$diagnostic), identical(jeu$provenance, original$provenance))
  for (v in names(benchmark_app$types)) {
    obs <- !is.na(original[[v]])
    stopifnot(identical(jeu[[v]][obs], original[[v]][obs]))
  }
}
a <- calculer_acm_app(imp)
desaligne <- imp
desaligne$jeux[[1]] <- imp$jeux[[1]][rev(seq_len(nrow(original))), ]
stopifnot(inherits(try(calculer_acm_app(desaligne), silent = TRUE), "try-error"))
stopifnot(nrow(a$ref$ind$coord) == nrow(original),
  all(c("diagnostic", "provenance") %in% names(a$ref$call$X)),
  length(a$individus) == 5, ncol(a$individus[[1]]) > 2,
  all(vapply(a$individus, function(x) all(is.finite(x)), logical(1))))
# Aucun des centres ne figure parmi les modalités actives.
stopifnot(!any(grepl("provenance|diagnostic", rownames(a$ref$var$coord))))
for (vue in c("modalites", "individus")) for (ell in c(FALSE, TRUE)) {
  g <- graphe_acm_app(a, vue, c(2, 3), ell, 10)
  invisible(ggplot2::ggplot_build(g))
}
# Les points observés ne varient pas quand les jeux sont projetés dans le même repère.
fixes <- which(!a$impute)
stopifnot(isTRUE(all.equal(a$individus[[1]][fixes, ], a$individus[[5]][fixes, ])))
shiny::testServer(serveur, {
  session$setInputs(variable_distribution = "cholesterol", variable_benchmark = "cholesterol", variable_na = "cholesterol",
    nombre_imputations = 5, limite_ellipses = 10)
  for (nom in c("effectifs", "distribution", "na_cumules", "na_detail", "benchmark_erreurs"))
    stopifnot(!is.null(output[[nom]]))
  stopifnot(grepl("172 zéros", output$distribution_effectif, fixed = TRUE))
  session$setInputs(variable_distribution = "pa_repos", variable_na = "pa_repos")
  stopifnot(grepl("1 zéros", output$distribution_effectif, fixed = TRUE),
            !is.null(output$distribution), !is.null(output$na_cumules))
  session$setInputs(variable_distribution = "diagnostic")
  stopifnot(!is.null(output$distribution))
  # Vérifier aussi que le bouton lance réellement le calcul et publie les résultats.
  session$setInputs(calculer_imputation = 1)
  stopifnot(!is.null(resultat()), is.null(erreur()))
  session$setInputs(axe_x = "2", axe_y = "3", ellipses_acm = TRUE)
  for (nom in c("acm_valeurs_propres", "acm_modalites", "acm_individus"))
    stopifnot(!is.null(output[[nom]]))
  stopifnot(grepl("AFDM disponible", output$statut_imputation))
  # Régression : le mode choisi doit conserver les projections des cinq jeux
  # et la case doit publier des contours réellement calculables.
  session$setInputs(acm_mode = "modeles", acm_lourdes = TRUE, acm_nboot = 20,
    calculer_acm = 1, ellipses_acm = TRUE)
  stopifnot(is.null(erreur_acm()), !isTRUE(acm_resultat()$individus_reference),
    length(acm_resultat()$individus) == 5L,
    grepl("Contours de la couleur des points", output$statut_ellipses, fixed = TRUE))
  contours <- bilan_ellipses_acm(acm_resultat(), axes(), 10L)
  stopifnot(contours$individus[["affichees"]] == 10L,
    contours$modalites[["affichees"]] > 0L)
  session$setInputs(modele_cholesterol = "median")
  stopifnot(grepl("recalculer", output$statut_imputation))
})
stopifnot(identical(original, donnees_uci), identical(empreintes,
  tools::md5sum(file.path("dataset", unname(import_uci$fichiers)))))
cat("Validation réussie : trois pages, vrais modèles, cinq imputations, valeurs observées, ACM supplémentaire, axes et ellipses.\n")
