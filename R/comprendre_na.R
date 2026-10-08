# Calculs descriptifs du parcours : aucune imputation ni sélection de traitement.
# Les tableaux d'importation conservent la décomposition NA initiaux / zéros recodés.
bilan_variable_absence <- function(donnees, avant_recodage, variable, sources) {
  do.call(rbind, lapply(sources, function(src) {
    idx <- donnees$provenance == src
    n <- sum(idx)
    initiaux <- sum(is.na(avant_recodage[[variable]][idx]))
    finaux <- sum(is.na(donnees[[variable]][idx]))
    data.frame(provenance = src, n = n, na_initiaux = initiaux,
      zeros_recodes = finaux - initiaux, n_na = finaux,
      pct_na = if (n) 100 * finaux / n else NA_real_,
      absence_totale = n > 0 && finaux == n)
  }))
}

bilan_population_complete <- function(donnees, variables, sources) {
  complet <- complete.cases(donnees[variables])
  avant <- as.integer(table(factor(donnees$provenance, levels = sources)))
  apres <- as.integer(table(factor(donnees$provenance[complet], levels = sources)))
  data.frame(provenance = sources, n_initial = avant, n_complet = apres,
    n_exclus = avant - apres,
    pct_conserves = ifelse(avant > 0, 100 * apres / pmax(avant, 1), NA_real_),
    pct_population_initiale = if (sum(avant)) 100 * avant / sum(avant) else NA_real_,
    pct_population_complete = if (sum(apres)) 100 * apres / sum(apres) else NA_real_)
}

# Un indicateur par variable ; conserver aussi les groupes vides pour les figures.
description_absence <- function(donnees, variable) {
  statut <- factor(ifelse(is.na(donnees[[variable]]), "Manquante", "Observée"),
                   levels = c("Observée", "Manquante"))
  d <- data.frame(statut = statut, age = donnees$age, sexe = donnees$sexe)
  groupes <- do.call(rbind, lapply(levels(statut), function(st) {
    x <- d[d$statut == st, ]
    data.frame(statut = st, n = nrow(x), n_age = sum(!is.na(x$age)),
      na_age = sum(is.na(x$age)), n_sexe = sum(!is.na(x$sexe)), na_sexe = sum(is.na(x$sexe)))
  }))
  list(donnees = d, groupes = groupes)
}

interpretation_cas_complets <- function(bilan, libelles) {
  total <- sum(bilan$n_initial)
  complet <- sum(bilan$n_complet)
  if (!complet) return(paste("Aucun cas complet sur", total,
    "observations : aucun modèle complet ne peut être ajusté sur cette sélection."))
  centre <- bilan$provenance[which.max(bilan$n_complet)]
  n <- max(bilan$n_complet)
  pct <- 100 * n / complet
  phrase <- paste0(complet, " cas complets sur ", total, " observations nettoyées, dont ", n,
    " de ", libelles[[centre]], " (", sprintf("%.1f", pct), " % des cas complets). ")
  if (pct >= 95 && sum(bilan$n_initial > 0) > 1) phrase <- paste0(phrase,
    "La sélection transforme de fait l'analyse multicentrique en une analyse presque exclusivement issue de ", libelles[[centre]], ". ")
  paste0(phrase, "L'analyse sur cas complets n'est pas automatiquement invalide, mais sa portée concerne cette population sélectionnée. Un risque de sélection doit être examiné selon l'objectif et le mécanisme d'absence.")
}

# Définitions traduites de heart-disease.names ; aucune raison de non-mesure inventée.
definitions_variables_na <- c(
  age = "Âge du patient, exprimé en années.",
  sexe = "Sexe tel que codé dans UCI : 0 = féminin, 1 = masculin. Variable catégorielle sans unité physique.",
  type_doul_thor = "Type de douleur thoracique : angine typique, angine atypique, douleur non angineuse ou absence de symptômes. Codes 1 à 4, sans unité physique.",
  pa_repos = "Pression artérielle au repos à l'admission, en mmHg.",
  cholesterol = "Cholestérol sérique, en mg/dL.",
  glyc_jeun_elevee = "Indicateur de glycémie à jeun supérieure à 120 mg/dL : 1 = vrai, 0 = faux. Variable catégorielle, pas une mesure continue de glycémie.",
  ecg_repos = "Résultat de l'ECG au repos : normal, anomalie ST-T ou hypertrophie ventriculaire gauche selon le codage UCI. Codes 0 à 2, sans unité physique.",
  fc_max = "Fréquence cardiaque maximale atteinte, exprimée en battements par minute.",
  angine_effort = "Angine induite par l'effort : 1 = oui, 0 = non. Variable catégorielle sans unité physique.",
  depress_st = "Dépression du segment ST induite par l'effort relativement au repos. L'unité n'est pas explicitée dans le fichier de documentation fourni.",
  pente_st = "Pente du segment ST au pic de l'effort : ascendante, plate ou descendante. Codes 1 à 3, sans unité physique.",
  nb_vaisseaux = "Nombre de vaisseaux majeurs visualisés par fluoroscopie, de 0 à 3. Le modèle existant le traite comme une catégorie.",
  test_thallium = "Résultat du test au thallium : 3 = normal, 6 = défaut fixe, 7 = défaut réversible. Variable catégorielle sans unité physique."
)

# Sources vérifiées : UCI et références physiologiques déjà utilisées ;
# ouvrage de l'auteur, publication méthodologique originale et documentation officielle.
references_comprendre_na <- list(
  list(titre = "Janosi et al. (1989) — Heart Disease, UCI, DOI 10.24432/C52P4X", url = "https://doi.org/10.24432/C52P4X"),
  list(titre = "MedlinePlus (National Library of Medicine) — Cholesterol", url = "https://medlineplus.gov/cholesterol.html"),
  list(titre = "NHLBI — Low Blood Pressure", url = "https://www.nhlbi.nih.gov/health/low-blood-pressure"),
  list(titre = "Van Buuren (2018) — Flexible Imputation of Missing Data, §1.2 : MCAR, MAR et MNAR", url = "https://stefvanbuuren.name/fimd/sec-MCAR.html"),
  list(titre = "Van Buuren (2018) — Flexible Imputation of Missing Data, §2.2 : définitions et limites d’identification", url = "https://stefvanbuuren.name/fimd/sec-idconcepts.html"),
  list(titre = "Van Buuren et Groothuis-Oudshoorn (2011) — mice: Multivariate Imputation by Chained Equations in R, JSS 45(3)", url = "https://doi.org/10.18637/jss.v045.i03"),
  list(titre = "Van Buuren (2018) — Flexible Imputation of Missing Data, §6.3 : choix des prédicteurs d'imputation", url = "https://stefvanbuuren.name/fimd/sec-modelform.html"),
  list(titre = "Scikit-learn — Common pitfalls : prévention des fuites d'information", url = "https://scikit-learn.org/stable/common_pitfalls.html"),
  list(titre = "Scikit-learn — Cross-validation : apprentissage des transformations dans les plis", url = "https://scikit-learn.org/stable/modules/cross_validation.html"),
  list(titre = "R / stats — Pearson : chisq.test", url = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/chisq.test.html"),
  list(titre = "R / stats — Fisher exact et simulation Monte-Carlo : fisher.test", url = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fisher.test.html"),
  list(titre = "R / stats — Correction Benjamini-Hochberg : p.adjust", url = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html")
)
