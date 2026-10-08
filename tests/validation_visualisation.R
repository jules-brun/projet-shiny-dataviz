# Exécuter depuis la racine : Rscript tests/validation_visualisation.R
invisible(capture.output(serveur <- source("server.R")$value))
original <- donnees_uci
empreintes <- tools::md5sum(file.path("dataset", unname(import_uci$fichiers)))
colonnes <- import_uci$colonnes
complets <- preparer_parcours(original, colonnes, variables_categorielles)
stopifnot(nrow(complets$donnees) == sum(complete.cases(original[colonnes])),
          !anyNA(complets$donnees[colonnes]))
for (methode in c("AFDM", "ACP")) {
  p <- preparer_parcours(original, colonnes, variables_categorielles, TRUE, methode = methode)
  stopifnot(nrow(p$donnees) == sum(!is.na(original$diagnostic)), !anyNA(p$donnees[colonnes]))
  for (v in setdiff(colonnes, "diagnostic")) {
    observe <- !is.na(original[[v]])
    stopifnot(identical(p$donnees[[v]][observe], original[[v]][observe]))
    if (v %in% variables_categorielles)
      stopifnot(all(p$donnees[[v]] %in% unique(na.omit(original[[v]]))))
  }
  stopifnot(identical(p$donnees$diagnostic, original$diagnostic),
            identical(p$donnees$provenance, original$provenance))
}
html <- as.character(source("ui.R")$value)
stopifnot(grepl("Cas complets", html), grepl("Imputation par composantes", html),
          !grepl("variable_modele|resultat_modeles|dataTable|<table", html))
libelles <- setNames(import_uci$dictionnaire$libelle, colonnes)
for (imputation in c(FALSE, TRUE)) {
  shiny::testServer(parcours_serveur, args = list(reference = original, colonnes = colonnes,
    categories = variables_categorielles, libelles = libelles,
    sources = libelles_sources, imputation = imputation), {
    session$setInputs(ncp = 2, methode = "AFDM", x = "age", y = "fc_max", couleur = "diagnostic")
    for (nom in c("centres", "distributions", "categories", "correlations", "relations", "acm_individus", "acm_modalites"))
      stopifnot(!is.null(output[[nom]]))
    if (imputation) invisible(output$imputations)
    session$setInputs(x = "pa_repos", y = "cholesterol", couleur = "provenance")
    invisible(output$relations)
    stopifnot(grepl("observations analysées", output$resume))
  })
}
stopifnot(identical(original, donnees_uci),
          identical(empreintes, tools::md5sum(file.path("dataset", unname(import_uci$fichiers)))))
cat("Validation réussie : cas complets, deux imputations, valeurs observées et modalités conservées, graphiques et ACM.\n")
