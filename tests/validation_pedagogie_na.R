# Exécuter à la racine : Rscript tests/validation_pedagogie_na.R
options(sass.cache = FALSE)
Sys.setenv(TZ = "Europe/Paris")
invisible(capture.output(serveur <- source("server.R")$value))

# Exemple contrôlé : 9 cellules, 3 NA, deux lignes incomplètes, 3 mesures jetées.
exemple <- data.frame(a = c(1, 2, NA), b = c(3, NA, 4), c = c(5, 6, NA), id = NA)
b <- calculer_perte_information(exemple, c("a", "b", "c"))
stopifnot(b$n_na == 3, b$n_observed == 6, b$incomplete_rows == 2,
  b$pct_na == 1/3, b$pct_rows_deleted == 2/3, b$observed_lost == 3,
  b$observed_retained == 3, b$pct_observed_information_lost == .5,
  b$observed_lost_per_na == 1)
complet <- calculer_perte_information(data.frame(a = 1:3), "a")
stopifnot(complet$observed_lost == 0, complet$observed_retained == 3,
  complet$pct_rows_deleted == 0, is.na(complet$observed_lost_per_na))
absent <- calculer_perte_information(data.frame(a = rep(NA_real_, 3)), "a")
stopifnot(absent$n_observed == 0, absent$pct_na == 1,
  is.na(absent$pct_observed_information_lost), absent$observed_lost_per_na == 0)
vide <- calculer_perte_information(data.frame(a = numeric()), "a")
stopifnot(vide$n_cells == 0, is.na(vide$pct_na))
stopifnot(inherits(try(calculer_perte_information(exemple, "inconnue"), silent = TRUE), "try-error"))

for (b in list(b, complet, absent, vide, bilan_perte_na)) {
  stopifnot(b$observed_lost + b$observed_retained == b$n_observed,
    b$observed_lost + b$observed_retained + b$n_na == b$n_cells,
    nzchar(phrase_perte_information(b)))
  invisible(ggplot2::ggplot_build(graphique_perte_information(b)))
}
stopifnot(identical(bilan_perte_na$variables, import_uci$colonnes),
  !any(c("provenance", "diagnostic_initial", "id_ligne") %in% bilan_perte_na$variables),
  bilan_perte_na$n_rows == 918, bilan_perte_na$complete_rows == 299)

pipeline <- graphique_pipeline_benchmark(names(benchmark_app$configuration$modeles),
  max(benchmark_app$resultats$repetition), benchmark_app$configuration$proportion)
invisible(ggplot2::ggplot_build(pipeline))
# Vérifier la correspondance du panneau des méthodes avec le benchmark configuré.
labels <- unlist(lapply(pipeline$layers, function(l) if (is.data.frame(l$data)) l$data$label))
stopifnot("Random Forest" %in% labels, "mice / PMM*" %in% labels,
  "7 · Répétitions\n30 simulations" %in% labels, !"CART" %in% labels)
html <- as.character(source("ui.R")$value)
stopifnot(!grepl("pipeline-placeholder", html, fixed = TRUE),
  all(vapply(c("perte_na_indicateurs", "perte_na_graphique", "perte_na_phrase", "pipeline_benchmark"),
    function(id) grepl(paste0('id="', id, '"'), html, fixed = TRUE), logical(1))))
original <- donnees_uci
shiny::testServer(serveur, {
  stopifnot(!is.null(output$perte_na_indicateurs), !is.null(output$perte_na_graphique),
    !is.null(output$pipeline_benchmark), grepl("6 741", output$perte_na_phrase, fixed = TRUE))
  session$setInputs(variable_benchmark = "cholesterol")
  stopifnot(!is.null(output$benchmark_erreurs), !is.null(output$choix_modeles))
})
stopifnot(identical(original, donnees_uci))
cat("Validation réussie : métriques, cas limites, périmètre clinique, deux graphiques et intégration Shiny.\n")
