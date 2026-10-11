# G2–G4 : dénominateurs, infobulles, conservation des tests et de l'architecture.
invisible(capture.output(serveur <- source('server.R')$value))
reference <- donnees_uci
empreintes <- tools::md5sum(file.path('dataset', unname(import_uci$fichiers)))
libelles <- setNames(import_uci$dictionnaire$libelle, import_uci$colonnes)
d <- data.frame(sexe = c(0, 0, 0, 1, 1, 1, NA),
                 diagnostic = c(1, 1, 0, 0, 1, NA, 1))
p <- calculer_parts_diagnostic(d, 'sexe', libelles)
stopifnot(identical(p$n, c(3L, 2L)), identical(p$positifs, c(2L, 1L)),
          isTRUE(all.equal(p$pct_positifs, c(200/3, 50))),
          !nrow(calculer_parts_diagnostic(transform(d, sexe = NA_real_), 'sexe', libelles)),
          identical(as.character(libeller_modalites(c(0, 99, NA), 'sexe')), c('Femme', '99', NA)))
# Modalités identiques entre variables : axes indépendants, pas de confusion.
d2 <- transform(d, angine_effort = sexe)
z <- calculer_parts_diagnostic(d2, c('sexe', 'angine_effort'), libelles)
g <- plotly::plotly_build(graphe_parts_diagnostic(z, 60))
stopifnot(inherits(g, 'plotly'), length(g$x$data) > 0L,
          all(vapply(g$x$data, function(t) is.null(t$x) || all(as.numeric(t$x) >= 0 & as.numeric(t$x) <= 100), logical(1))))
# Contrôle manuel sur toutes les modalités des données réelles.
r <- calculer_parts_diagnostic(reference, variables_categorielles, libelles)
for (v in variables_categorielles) {
  rv <- r[r$variable == v, ]
  ok <- !is.na(reference[[v]]) & reference$diagnostic %in% 0:1
  stopifnot(sum(rv$n) == sum(ok), sum(rv$positifs) == sum(reference$diagnostic[ok] == 1))
}
analyses <- associations_absence
shiny::testServer(serveur, {
  matrice <- output$absence_matrice
  stopifnot(grepl('p ajustée BH', matrice, fixed = TRUE),
            grepl('V de Cramér', matrice, fixed = TRUE),
            grepl('Raison', matrice, fixed = TRUE),
            grepl('correction BH globale', output$absence_note, fixed = TRUE))
})
for (imputation in c(FALSE, TRUE)) {
  shiny::testServer(parcours_serveur, args = list(reference = reference,
    colonnes = import_uci$colonnes, categories = variables_categorielles,
    libelles = libelles, sources = libelles_sources, imputation = imputation), {
    session$setInputs(ncp = 2L, methode = 'AFDM', x = 'age', y = 'fc_max', couleur = 'diagnostic')
    stopifnot(grepl('Centre', output$relations, fixed = TRUE),
              grepl('Observée', output$relations, fixed = TRUE))
    session$setInputs(x = 'pa_repos', y = 'cholesterol', couleur = 'provenance')
    if (imputation) stopifnot(grepl('Imputée', output$relations, fixed = TRUE))
    invisible(output$categories_positifs)
    session$setInputs(categories_vue = 'distribution')
    invisible(output$categories)
    session$setInputs(categories_vue = 'positifs')
    invisible(output$categories_positifs)
  })
}
html <- as.character(source('ui.R')$value)
stopifnot(grepl('categories_positifs', html), grepl('categories_vue', html),
          grepl('plotly', html), !grepl('5 · ACM|Présentation', html),
          identical(analyses, associations_absence), identical(reference, donnees_uci),
          identical(empreintes, tools::md5sum(file.path('dataset', unname(import_uci$fichiers)))))
cat('Validation G2–G4 réussie : proportions, tooltips, vues conservées, statistiques et sources inchangées.\n')
