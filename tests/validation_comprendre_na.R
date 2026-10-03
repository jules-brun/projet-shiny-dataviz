# Exécuter depuis la racine : Rscript tests/validation_comprendre_na.R
# Contrôles descriptifs et rendus réactifs du parcours narratif, sans traitement.
fichiers_sources <- file.path("dataset", c("processed.cleveland.data", "processed.hungarian.data",
  "processed.switzerland.data", "processed.va.data"))
empreintes <- tools::md5sum(fichiers_sources)
invisible(capture.output(serveur <- source("server.R")$value))
# Les anciens calculs narratifs restent testables indépendamment de l'interface.
source("R/comprendre_na.R")
source("R/serveur_na.R")
interface <- source("ui.R")$value
reference <- reference_na$donnees
original <- reference
sources <- names(libelles_sources)
variables <- dictionnaire_na$nom_fr
bilan <- bilan_population_complete(reference, c(variables, "diagnostic"), sources)
stopifnot(sum(bilan$n_initial) == nrow(reference),
  sum(bilan$n_complet) == nrow(donnees_modelisation),
  all(bilan$n_initial == bilan$n_complet + bilan$n_exclus),
  isTRUE(all.equal(sum(bilan$pct_population_initiale), 100)),
  isTRUE(all.equal(sum(bilan$pct_population_complete), 100)))
for (v in variables) {
  b <- bilan_variable_absence(reference, import_uci$heart_avant_recodage, v, sources)
  stopifnot(sum(b$n_na) == sum(is.na(reference[[v]])),
    sum(b$na_initiaux) == sum(is.na(import_uci$heart_avant_recodage[[v]])),
    all(b$n_na == b$na_initiaux + b$zeros_recodes),
    all(b$pct_na == 100 * b$n_na / b$n),
    all(b$absence_totale == (b$n_na == b$n)))
  d <- description_absence(reference, v)
  stopifnot(sum(d$groupes$n) == nrow(reference),
    sum(d$groupes$n_age + d$groupes$na_age) == nrow(reference),
    sum(d$groupes$n_sexe + d$groupes$na_sexe) == nrow(reference))
}
# Distinguer un zéro valide d'un zéro recodé ; réconcilier le journal existant.
stopifnot(any(reference$sexe == 0), sum(is.na(reference$sexe)) == 0,
  any(reference$depress_st == 0, na.rm = TRUE),
  !any(reference$cholesterol == 0, na.rm = TRUE),
  !any(reference$pa_repos == 0, na.rm = TRUE),
  sum(import_uci$na_origine$nombre) == sum(is.na(reference[import_uci$colonnes])),
  sum(import_uci$journal_recodage$n_zeros_recodes) ==
    sum(is.na(reference[import_uci$colonnes])) - sum(is.na(import_uci$heart_avant_recodage[import_uci$colonnes])))
cholesterol <- bilan_variable_absence(reference, import_uci$heart_avant_recodage, "cholesterol", sources)
suisse <- cholesterol[cholesterol$provenance == "switzerland", ]
stopifnot(suisse$absence_totale, suisse$zeros_recodes == suisse$n,
          suisse$na_initiaux == 0)
# Vérifier la fiche et chaque graphique pour toutes les variables (dont sans NA).
serveur_test <- function(input, output, session) {
  etat <- serveur_comprendre_na(input, output, session, reference, import_uci, dictionnaire_na, libelles_sources)
}
shiny::testServer(serveur_test, {
  sorties_fixes <- c("recit_nettoyage", "recit_origines", "recit_carte", "recit_structure",
    "recit_cas_complets", "recit_composition", "recit_selection", "recit_conclusion", "recit_references")
  for (nom in sorties_fixes) invisible(output[[nom]])
  for (v in variables) {
    session$setInputs(recit_variable = v)
    stopifnot(sum(etat$fiche()$n_na) == sum(is.na(reference[[v]])))
    for (nom in c("recit_fiche_constat", "recit_fiche_bilan", "recit_definition", "recit_collecte",
                  "recit_proportions", "recit_age", "recit_sexe")) invisible(output[[nom]])
  }
  stopifnot(grepl(as.character(sum(bilan$n_complet)), output$recit_selection),
    grepl("presque exclusivement", output$recit_selection))
})
# Cas extrêmes : variable totalement absente, aucun âge disponible, centre vide.
ref_extreme <- reference
ref_extreme$cholesterol <- NA_real_
ref_extreme$age <- NA_real_
import_extreme <- as.list(import_uci)
import_extreme$heart_avant_recodage <- ref_extreme
serveur_extreme <- function(input, output, session) {
  etat <- serveur_comprendre_na(input, output, session, ref_extreme, import_extreme, dictionnaire_na, libelles_sources)
}
shiny::testServer(serveur_extreme, {
  session$setInputs(recit_variable = "cholesterol")
  stopifnot(all(etat$fiche()$pct_na == 100), all(etat$fiche()$absence_totale),
    etat$description()$groupes$n[1] == 0,
    grepl("Aucun cas complet", output$recit_selection))
  for (nom in c("recit_age", "recit_proportions", "recit_sexe", "recit_composition")) invisible(output[[nom]])
})
ref_cleveland <- reference[reference$provenance == "cleveland", ]
bilan_vide <- bilan_variable_absence(ref_cleveland,
  import_uci$heart_avant_recodage[import_uci$heart_avant_recodage$provenance == "cleveland", ], "age", sources)
stopifnot(all(is.na(bilan_vide$pct_na[bilan_vide$n == 0])),
          !any(bilan_vide$absence_totale))
# Trois premiers onglets et absence des anciens contrôles dans la nouvelle interface.
html <- as.character(interface)
stopifnot(grepl("Comprendre les données manquantes", html),
  !grepl("na_trait_|na_calcul|na_portee_knn|na_export_", html),
  grepl("1 · Données", html), grepl("2 · Valeurs manquantes", html), grepl("3 · Modélisation", html))
shiny::testServer(serveur, {
  session$setInputs(source_apercu = "toutes", n_apercu = 6, variable_modele = "cholesterol", recit_variable = "cholesterol")
  for (nom in c("indicateurs", "apercu", "effectifs", "dictionnaire", "doublons", "journal_zeros",
    "carte_na", "na_global_ui", "na_sources_ui", "na_detail", "origine_na", "bilan_modelisation",
    "effectifs_modelisation", "formules_modeles", "comparaison_modeles", "conclusion_modeles")) invisible(output[[nom]])
  stopifnot(grepl("ANOVA", output$resultat_modeles))
  invisible(output$absence_matrice)
})
stopifnot(identical(reference, original), identical(reference_na$donnees, original),
          identical(tools::md5sum(fichiers_sources), empreintes))
cat("Validation réussie : effectifs, groupes vides, NA à 0/100 %, zéros, sources inchangées et quatre onglets.\n")
