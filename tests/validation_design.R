# Exécuter depuis la racine : Rscript tests/validation_design.R
invisible(capture.output(serveur <- source("server.R")$value))
interface <- source("ui.R")$value
html <- as.character(interface)
stopifnot(all(vapply(c("overview", "donnees", "manquantes", "relations", "associations"),
  function(v) grepl(paste0('data-value="', v, '"'), html, fixed = TRUE), logical(1))),
  identical(attr(interface, "lang"), "fr"),
  grepl("Justification bibliographique", html, fixed = TRUE))

# Vérifier que les identifiants existants restent raccordés au serveur.
identifiants <- c("source_apercu", "variable_apercu", "dimensions", "dictionnaire",
  "effectifs", "apercu", "bilan_doublons", "doublons", "recodages_na", "na_detail",
  "combinaisons_na", "na_global_ui", "absence_matrice", "absence_conclusion")
stopifnot(all(vapply(identifiants, function(id)
  grepl(paste0('id="', id, '"'), html, fixed = TRUE), logical(1))))
original <- donnees_uci
shiny::testServer(serveur, {
  session$setInputs(source_apercu = "toutes", variable_apercu = "age")
  indicateurs <- output$overview_indicateurs$html
  stopifnot(grepl(as.character(nrow(donnees_uci)), indicateurs, fixed = TRUE),
    grepl(as.character(sum(complete.cases(donnees_uci[import_uci$colonnes]))), indicateurs, fixed = TRUE),
    grepl("sans imputation", indicateurs, fixed = TRUE))
  for (nom in c("overview_centres", "overview_diagnostic", "effectifs", "apercu",
    "na_detail", "combinaisons_na", "na_global_ui", "absence_matrice"))
    stopifnot(!is.null(output[[nom]]))
  stopifnot(grepl("920", output$overview_chemin$html, fixed = TRUE),
    nzchar(output$absence_conclusion))
  session$setInputs(source_apercu = "cleveland", variable_apercu = "sexe")
  invisible(output$apercu)
  invisible(output$effectifs)
  session$setInputs(overview_donnees = 1, overview_na = 1)
})
stopifnot(identical(original, donnees_uci),
  identical(theme_heart()$plot.background$fill, palette_heart$card))
cat("Validation réussie : navigation, sorties conservées, indicateurs, filtres et graphiques sombres.\n")
