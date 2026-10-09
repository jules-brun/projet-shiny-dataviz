# Exécuter depuis la racine : Rscript tests/validation_design.R
invisible(capture.output(serveur <- source("server.R")$value))
interface <- source("ui.R")$value
html <- as.character(interface)
stopifnot(all(vapply(c("overview", "donnees", "manquantes", "relations", "associations", "acm", "apropos"),
  function(v) grepl(paste0('data-value="', v, '"'), html, fixed = TRUE), logical(1))),
  identical(attr(interface, "lang"), "fr"),
  grepl("Justification bibliographique", html, fixed = TRUE))

# Vérifier que les identifiants existants restent raccordés au serveur.
identifiants <- c("source_apercu", "variable_apercu", "dimensions", "dictionnaire",
  "question_apercu", "interpretation_apercu", "effectifs", "apercu",
  "sexe_apercu", "age_apercu", "quanti_apercu", "quali_apercu",
  "bilan_doublons", "doublons", "recodages_na", "na_detail",
  "combinaisons_na", "na_global_ui", "absence_matrice", "absence_conclusion")
stopifnot(all(vapply(identifiants, function(id)
  grepl(paste0('id="', id, '"'), html, fixed = TRUE), logical(1))))
stopifnot(grepl("diagnostic_centre", html, fixed = TRUE),
  grepl("age_sexe", html, fixed = TRUE))
original <- donnees_uci
shiny::testServer(serveur, {
  session$setInputs(source_apercu = "toutes", question_apercu = "age_sexe",
    variable_apercu = "age")
  session$flushReact()
  indicateurs <- output$overview_indicateurs$html
  stopifnot(grepl(as.character(nrow(donnees_uci)), indicateurs, fixed = TRUE),
    grepl(as.character(sum(complete.cases(donnees_uci[import_uci$colonnes]))), indicateurs, fixed = TRUE),
    grepl("sans imputation", indicateurs, fixed = TRUE))
  for (nom in c("overview_centres", "overview_diagnostic", "effectifs", "apercu",
    "na_detail", "combinaisons_na", "na_global_ui", "absence_matrice"))
    stopifnot(!is.null(output[[nom]]))
  session$setInputs(question_apercu = "age_sexe")
  session$flushReact()
  stopifnot(grepl("Âge et sexe renseignés", output$interpretation_apercu))
  invisible(output$apercu)
  session$setInputs(question_apercu = "diagnostic_centre")
  session$flushReact()
  stopifnot(grepl("diagnostics renseignés", output$interpretation_apercu),
    grepl("Cleveland", output$interpretation_apercu))
  invisible(output$apercu)
  session$setInputs(question_apercu = "variable", variable_apercu = "cholesterol")
  session$flushReact()
  stopifnot(grepl("valeurs observées", output$interpretation_apercu),
    grepl("intervalle interquartile", output$interpretation_apercu))
  invisible(output$apercu)
  # Nouvelles questions : graphe + texte avec test statistique
  session$setInputs(question_apercu = "quanti_diag", quanti_apercu = "fc_max")
  session$flushReact()
  stopifnot(grepl("Delta de Cliff", output$interpretation_apercu))
  invisible(output$apercu)
  session$setInputs(question_apercu = "tendance", quanti_apercu = "age")
  session$flushReact()
  stopifnot(grepl("Cochran-Armitage", output$interpretation_apercu))
  invisible(output$apercu)
  session$setInputs(question_apercu = "quali_diag", quali_apercu = "type_doul_thor")
  session$flushReact()
  stopifnot(grepl("Asymptomatique", output$interpretation_apercu),
    grepl("V de Cramér", output$interpretation_apercu))
  invisible(output$apercu)
  # Filtres sexe et âge, alerte si petit effectif
  session$setInputs(source_apercu = "switzerland", sexe_apercu = "0", age_apercu = c(28, 77),
    question_apercu = "age_sexe")
  session$flushReact()
  stopifnot(nrow(apercu()) < 20, all(apercu()$sexe == 0),
    grepl("Attention", output$interpretation_apercu))
  session$setInputs(source_apercu = "toutes", sexe_apercu = "tous", age_apercu = c(50, 60))
  session$flushReact()
  stopifnot(all(apercu()$age >= 50 & apercu()$age <= 60))
  # ACM : les trois gestions des NA
  for (m in c("complets", "simple", "multiple")) {
    session$setInputs(acm_mode = m, acm_vue = "modalites", acm_lourdes = m == "complets")
    session$flushReact()
    invisible(output$acm_graphe)
    stopifnot(grepl("Ce que|patients analysés", output$acm_texte))
  }
  stopifnot(grepl("ellipse", output$acm_texte), grepl("Cleveland 99", texte_acm(
    calculer_acm(donnees_uci, "complets", TRUE), "modalites")))
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
