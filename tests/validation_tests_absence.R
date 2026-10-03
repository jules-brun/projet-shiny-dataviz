# Exécuter depuis la racine : Rscript tests/validation_tests_absence.R
source("R/tests_absence.R")
dictionnaire_test <- data.frame(nom_fr = c("x", "y", "z"),
  libelle = c("Variable X", "Variable Y", "Variable Z"), type = "Catégorielle")
d <- data.frame(x = c(NA, NA, 1, 1, 1, 1), y = c(0, 1, 0, 0, 1, NA),
  z = c(NA, NA, NA, 1, 1, 1), provenance = c("a", "a", "b", "a", "b", "b"),
  diagnostic = c(0, 1, 0, 1, 0, 1), diagnostic_initial = 4, id_ligne = 1:6)
p <- preparer_variables_absence(d, dictionnaire_test)
paire <- tester_paire_absence(d, "x", "y", p)
r <- paire$resultat
manuel <- matrix(c(2L, 1L, 1L, 1L), nrow = 2,
  dimnames = list(c("Observée (0)", "Manquante (1)"), c("0", "1")))
stopifnot(all(paire$contingence == manuel), r$n_utilise == 5,
  r$n_exclus_y_na == 1, r$n_x_manquantes == 2, r$n_x_observees == 3,
  r$methode == "Fisher exact (bilatéral)",
  isTRUE(all.equal(r$p_brute, fisher.test(manuel)$p.value)),
  isTRUE(all.equal(r$v_cramer, sqrt(suppressWarnings(chisq.test(manuel, correct = FALSE))$statistic / 5), check.attributes = FALSE)))
# Aucun cas complet global n'est exigé : z manque chez des patients inclus pour y.
stopifnot(sum(complete.cases(d[c("x", "y", "z")])) < r$n_utilise)
a <- analyser_absences(d, dictionnaire_test)
valide <- a$resultats$statut == "Calculé"
stopifnot(isTRUE(all.equal(a$resultats$p_ajustee[valide], p.adjust(a$resultats$p_brute[valide], "BH"))),
  all(is.na(a$resultats$p_ajustee[!valide])),
  !any(c("diagnostic_initial", "id_ligne") %in% a$colonnes),
  all(a$resultats$statut[a$resultats$variable_absence == a$resultats$variable_croisee] == "Non applicable"))
# Sans NA : aucune ligne. Tout X absent : un statut unique, non-calcul explicite.
sans_na <- d
sans_na[c("x", "y", "z")] <- lapply(sans_na[c("x", "y", "z")], function(x) replace(x, is.na(x), 1))
stopifnot(nrow(analyser_absences(sans_na, dictionnaire_test)$resultats) == 0)
entier <- d
entier$x <- NA_real_
pp <- preparer_variables_absence(entier, dictionnaire_test)
stopifnot(grepl("Un seul statut", tester_paire_absence(entier, "x", "y", pp)$resultat$raison))
# Y constante ou entièrement manquante.
constant <- d
constant$y <- 1
pp <- preparer_variables_absence(constant, dictionnaire_test)
stopifnot(grepl("deux modalités", tester_paire_absence(constant, "x", "y", pp)$resultat$raison))
constant$y <- NA_real_
pp <- preparer_variables_absence(constant, dictionnaire_test)
stopifnot(grepl("Aucune observation", tester_paire_absence(constant, "x", "y", pp)$resultat$raison))
# Classes par quantiles : bornes partagées, doublons retirés, classe unique.
c1 <- classes_absence(rep(5, 12), "age")
c2 <- classes_absence(c(rep(0, 50), rep(1, 20), rep(2, 30)), "age")
c3 <- classes_absence(c(rep(0, 90), rep(1, 10)), "age")
c4 <- classes_absence(rep(NA_real_, 10), "age")
stopifnot(nzchar(c1$raison), length(unique(c2$bornes)) == length(c2$bornes),
  nlevels(c2$valeurs) == 2, sum(c2$tableau$n) == 100,
  nzchar(c3$raison), nzchar(c4$raison))
quant <- d
quant$y <- c(0, 10, 20, 30, 40, NA)
dict_quant <- dictionnaire_test
dict_quant$type[dict_quant$nom_fr == "y"] <- "Quantitative"
qa <- analyser_absences(quant, dict_quant)
for (x in qa$lignes) if (x != "y")
  stopifnot(identical(colnames(qa$paires[[cle_paire_absence(x, "y")]]$contingence),
    levels(droplevels(qa$preparation$valeurs$y[!is.na(quant$y)]))))
# Pearson sans correction sur grands effectifs et choix à petits effectifs.
grand <- d[rep(seq_len(nrow(d)), each = 100), ]
pg <- preparer_variables_absence(grand, dictionnaire_test)
r <- tester_paire_absence(grand, "x", "y", pg)$resultat
stopifnot(grepl("Pearson", r$methode), r$attendu_min >= 1, r$proportion_attendus_inf_5 <= .2)
# Monte-Carlo reproducible, positif, à B = 100000 et sans changer le RNG appelant.
mc <- d
mc$y <- c(0, 1, 2, 0, 1, 2)
pm <- preparer_variables_absence(mc, dictionnaire_test)
set.seed(73)
graine_initiale <- .Random.seed
r1 <- tester_paire_absence(mc, "x", "y", pm)$resultat
r2 <- tester_paire_absence(mc, "x", "y", pm)$resultat
stopifnot(identical(.Random.seed, graine_initiale), identical(r1$p_brute, r2$p_brute),
  r1$simule, r1$B == 100000, r1$p_brute >= 1 / 100001,
  !grepl("^0$", trimws(formater_p_absence(r1$p_brute))))
# Un échec explicite reste limité à sa paire, sans masquer l'erreur.
reglages_invalides <- reglages_tests_absence
reglages_invalides$B <- NA_integer_
echec <- tester_paire_absence(mc, "x", "y", pm, reglages_invalides)
stopifnot(echec$resultat$statut == "Erreur de calcul", nzchar(echec$resultat$raison))
# Validation des données réelles et des rendus Shiny.
invisible(capture.output(serveur <- source("server.R")$value))
original <- reference_na$donnees
empreintes <- tools::md5sum(file.path("dataset", unname(import_uci$fichiers)))
a <- associations_absence
stopifnot(nrow(a$resultats) == length(a$lignes) * length(a$colonnes),
  a$n_total == nrow(original),
  identical(a$lignes, dictionnaire_na$nom_fr[vapply(original[dictionnaire_na$nom_fr], anyNA, logical(1))]))
for (i in which(a$resultats$statut != "Non applicable")) {
  r <- a$resultats[i, ]
  tab <- a$paires[[cle_paire_absence(r$variable_absence, r$variable_croisee)]]$contingence
  stopifnot(sum(tab) == r$n_utilise, r$n_utilise + r$n_exclus_y_na == nrow(original),
    r$n_x_manquantes + r$n_x_observees == r$n_utilise)
}
valide <- a$resultats$statut == "Calculé"
stopifnot(isTRUE(all.equal(a$resultats$p_ajustee[valide], p.adjust(a$resultats$p_brute[valide], "BH"))))
serveur_test <- function(input, output, session) {
  etat <- serveur_tests_absence(input, output, session, a, dictionnaire_na, libelles_sources)
}
shiny::testServer(serveur_test, {
  for (nom in c("absence_perimetre", "absence_matrice", "absence_details", "absence_classes", "absence_syntheses")) invisible(output[[nom]])
  # Consultation de toutes les cellules, y compris les non-calculables.
  for (i in seq_len(nrow(a$resultats))) {
    r <- a$resultats[i, ]
    session$setInputs(absence_x = r$variable_absence, absence_y = r$variable_croisee)
    stopifnot(identical(etat$resultat_paire()$p_ajustee, r$p_ajustee))
    invisible(output$absence_paire_resume)
    if (r$statut != "Non applicable" && ncol(etat$paire()$contingence) > 0) {
      invisible(output$absence_contingence)
      invisible(output$absence_proportions)
      invisible(output$absence_barres)
      prop <- etat$proportions()
      stopifnot(sum(prop$Effectif) == r$n_utilise, sum(prop$X_manquantes) == r$n_x_manquantes)
    }
  }
})
shiny::testServer(serveur, {
  session$setInputs(absence_x = "cholesterol", absence_y = "provenance", recit_variable = "cholesterol", variable_modele = "cholesterol")
  invisible(output$absence_matrice)
  invisible(output$absence_barres)
  invisible(output$recit_fiche_bilan)
  stopifnot(grepl("ANOVA", output$resultat_modeles))
})
stopifnot(identical(original, reference_na$donnees),
  identical(empreintes, tools::md5sum(file.path("dataset", unname(import_uci$fichiers)))))
cat("Validation réussie : tableaux manuels, cas limites, simulations, BH et cohérence des rendus.\n")
