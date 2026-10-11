# Intégration sélective Jules dans l'architecture noire, sans relancer le ML.
options(sass.cache = FALSE, heart.acm.cache_dir = NULL)
Sys.setenv(TZ = "Europe/Paris")
invisible(capture.output(serveur <- source("server.R")$value))
reference <- donnees_uci
empreintes <- tools::md5sum(file.path("dataset", unname(import_uci$fichiers)))
# Échantillon de quatre centres ; peu de simulations pour tester le protocole.
d <- reference[c(seq_len(80), 330:350, 600:620, 820:840), ]
set.seed(157); rng <- .Random.seed
cc <- calculer_acm_conditionnelle(d, mode = "complets", avec_lourdes = FALSE)
si <- calculer_acm_conditionnelle(d, mode = "simple", avec_lourdes = FALSE, nbsim = 2L)
mi <- calculer_acm_conditionnelle(d, mode = "multiple", avec_lourdes = FALSE, nboot = 3L, nbsim = 2L)
stopifnot(identical(rng, .Random.seed), si$n == nrow(d), mi$m == 3L,
  length(mi$individus) == 1L, length(mi$modalites) == 3L,
  identical(mi$individus[[1]], mi$ref$ind$coord),
  identical(mi$auxiliaires, "provenance"),
  !any(c("diagnostic", "diagnostic_initial", "id_ligne") %in% mi$entrees_imputation),
  !any(c("diagnostic", "provenance") %in% mi$variables),
  identical(names(mi$ref$call$X)[mi$ref$call$quali.sup], c("diagnostic", "provenance")),
  cc$n == sum(complete.cases(d[setdiff(variables_acm_app, acm_variables_lourdes)])),
  ncol(mi$ref$ind$coord) > 2L)
for (r in list(si, mi)) for (v in r$variables) {
  observe <- !is.na(d[[v]])
  stopifnot(!anyNA(r$complete[[v]]), identical(r$complete[[v]][observe], d[[v]][observe]))
}
# Le diagnostic ne peut influencer la reconstruction ni les axes.
change <- d; change$diagnostic <- 1L - change$diagnostic
change$diagnostic_initial <- 4L - change$diagnostic_initial
sans_circularite <- calculer_acm_conditionnelle(change, mode = "simple", avec_lourdes = FALSE, nbsim = 2L)
stopifnot(identical(si$complete, sans_circularite$complete),
  isTRUE(all.equal(si$ref$eig, sans_circularite$ref$eig)))
change$diagnostic[1:5] <- NA
avec_diag_na <- calculer_acm_conditionnelle(change, mode = "simple", avec_lourdes = FALSE, nbsim = 2L)
stopifnot(avec_diag_na$n == nrow(d), identical(si$complete, avec_diag_na$complete))
lourdes <- calculer_acm_conditionnelle(d, mode = "simple", avec_lourdes = TRUE, nbsim = 2L)
stopifnot(all(acm_variables_lourdes %in% lourdes$variables), !anyNA(lourdes$complete))
un_centre <- calculer_acm_conditionnelle(d[d$provenance == "cleveland", ], mode = "complets")
stopifnot(!length(un_centre$auxiliaires))
# Labels : codes inconnus et NA préservés ; diagnostic reste 0/1.
stopifnot(identical(as.character(libeller_modalites(c(0, 1, 99, NA), "sexe")),
  c("Femme", "Homme", "99", NA_character_)), pct_fr(66.34, 2) == "66,34 %",
  identical(as.character(libeller_modalites(0:1, "diagnostic")), c("0", "1")))
# Chaque dimension sélectionnable reste disponible ; aucun nouveau graphe.
for (r in list(cc, si, mi)) for (vue in c("individus", "modalites")) {
  g <- graphe_acm_app(r, vue, c(2L, 3L), TRUE)
  stopifnot(identical(g$theme$plot.background$fill, palette_heart$card))
  invisible(ggplot2::ggplot_build(g))
  if (vue == "individus") stopifnot(!any(vapply(g$layers,
    function(l) inherits(l$geom, "GeomPath"), logical(1))))
}
stopifnot(grepl("plan factoriel affiché", acm_texte_resultat(mi, c(2, 3)), fixed = TRUE))
# Clé de cache / lecture RDS / changement de configuration.
dossier <- tempfile("cache_acm_"); dir.create(dossier)
options(heart.acm.cache_dir = dossier)
a <- acm_resultat_cache(d, "complets", FALSE)
b <- acm_resultat_cache(d, "complets", FALSE)
stopifnot(identical(a, b), length(list.files(dossier)) == 1L)
rm(list = ls(acm_cache_memoire), envir = acm_cache_memoire)
c <- acm_resultat_cache(d, "complets", FALSE)
stopifnot(identical(a, c))
invisible(acm_resultat_cache(d, "complets", TRUE))
stopifnot(length(list.files(dossier)) == 2L)
# Contrôles Shiny : les changements ne recalculent pas sans clic.
shiny::testServer(serveur, {
  session$setInputs(acm_mode = "complets", acm_lourdes = FALSE, acm_nboot = 20,
    calculer_acm = 1, axe_x = "1", axe_y = "2", ellipses_acm = TRUE, limite_ellipses = 10)
  avant <- acm_resultat()
  stopifnot(avant$mode == "complets", is.null(erreur_acm()),
    grepl("66,34", output$acm_taux_na, fixed = TRUE))
  session$setInputs(acm_lourdes = TRUE)
  stopifnot(identical(avant, acm_resultat()), grepl("Réglages modifiés", output$statut_acm))
  session$setInputs(calculer_acm = 2)
  stopifnot(all(acm_variables_lourdes %in% acm_resultat()$variables))
  for (nom in c("acm_valeurs_propres", "acm_modalites", "acm_individus", "acm_resume"))
    stopifnot(!is.null(output[[nom]]))
  session$setInputs(acm_mode = "modeles", calculer_acm = 3)
  stopifnot(!is.null(erreur_acm()), identical(acm_resultat()$mode, "complets"))
})
options(heart.acm.cache_dir = NULL); unlink(dossier, recursive = TRUE)
stopifnot(identical(reference, donnees_uci), identical(empreintes,
  tools::md5sum(file.path("dataset", unname(import_uci$fichiers)))))
cat("Validation ACM : trois stratégies, diagnostic exclu, provenance auxiliaire, projection commune, individus de référence, axes, cache et bouton.\n")
