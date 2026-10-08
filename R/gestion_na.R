# Gestion des NA : fonctions pures, sans modification de la référence.
# Distance mixte de Gower : documentation et choix détaillés dans README_SHINY.txt.
choix_traitements <- c("Conserver les NA" = "conserver",
  "Supprimer les observations manquantes" = "lignes",
  "Supprimer la variable" = "variable", "Imputation simple" = "simple",
  "Imputation par k plus proches voisins" = "knn")

creer_dictionnaire_na <- function(dictionnaire, categories) {
  d <- dictionnaire[dictionnaire$nom_fr != "diagnostic", c("nom_fr", "libelle")]
  d$type <- ifelse(d$nom_fr %in% categories, "Catégorielle", "Quantitative")
  d$unite <- unname(c(age = "ans", sexe = "modalité", type_doul_thor = "modalité",
    pa_repos = "mmHg", cholesterol = "mg/dL", glyc_jeun_elevee = "modalité",
    ecg_repos = "modalité", fc_max = "battements/minute", angine_effort = "modalité",
    depress_st = "unité ST", pente_st = "modalité", nb_vaisseaux = "modalité (0 à 3)",
    test_thallium = "modalité")[d$nom_fr])
  # Les codes sont explicitement nominaux, même lorsqu'ils sont numériques.
  d$modalites <- I(lapply(d$nom_fr, function(v) switch(v,
    sexe = 0:1, type_doul_thor = 1:4, glyc_jeun_elevee = 0:1,
    ecg_repos = 0:2, angine_effort = 0:1, pente_st = 1:3,
    nb_vaisseaux = 0:3, test_thallium = c(3, 6, 7), numeric())))
  d
}

modalite_frequente <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  valeurs <- sort(unique(x))
  valeurs[which.max(tabulate(match(x, valeurs), nbins = length(valeurs)))]
}

# Comparaisons uniquement sur les autres prédicteurs réellement observés.
# Quantitatif : différence absolue / étendue ; catégorie : égalité ou différence.
# Une composante manquante dans l'une des deux lignes n'entre pas dans la moyenne.
# Sans composante comparable, la distance reste NA (donneur non admissible).
distance_gower_observee <- function(d, receveur, donneurs, variables, dictionnaire) {
  somme <- poids <- numeric(length(donneurs))
  for (v in variables) {
    x <- d[[v]]
    observations <- x[!is.na(x)]
    if (length(unique(observations)) < 2L || is.na(x[receveur])) next
    ok <- !is.na(x[donneurs])
    if (dictionnaire$type[match(v, dictionnaire$nom_fr)] == "Quantitative") {
      contribution <- abs(x[donneurs] - x[receveur]) / diff(range(observations))
    } else contribution <- as.numeric(x[donneurs] != x[receveur])
    somme[ok] <- somme[ok] + contribution[ok]
    poids[ok] <- poids[ok] + 1
  }
  ifelse(poids > 0, somme / pmax(poids, 1), NA_real_)
}

construire_scenario <- function(reference, centres, traitements, k, portee_simple,
                                portee_knn, dictionnaire, imputer = TRUE) {
  variables <- dictionnaire$nom_fr
  stopifnot(all(traitements[variables] %in% unname(choix_traitements)),
    portee_simple %in% c("global", "centre"), portee_knn %in% c("global", "centre"))
  initial <- reference[reference$provenance %in% centres, , drop = FALSE]
  retenues <- variables[traitements[variables] != "variable"]
  d <- initial[c("id_ligne", "provenance", "diagnostic", "diagnostic_initial", retenues)]
  exclusions <- data.frame(id_ligne = character(), provenance = character(),
                            variable = character(), raison = character())
  for (v in retenues[traitements[retenues] == "lignes"]) {
    ids <- which(is.na(d[[v]]))
    exclusions <- rbind(exclusions, data.frame(id_ligne = d$id_ligne[ids],
      provenance = d$provenance[ids], variable = rep(v, length(ids)),
      raison = rep("Valeur manquante : suppression de ligne", length(ids))))
  }
  # L'union évite de compter plusieurs fois les lignes exclues pour plusieurs NA.
  d <- d[!d$id_ligne %in% exclusions$id_ligne, , drop = FALSE]
  observes <- d
  masque <- matrix(FALSE, nrow(d), length(retenues), dimnames = list(d$id_ligne, retenues))
  evenements <- data.frame(id_ligne = character(), provenance = character(),
    variable = character(), methode = character(), statut = character(),
    valeur = numeric(), donneurs = character(), message = character())
  if (imputer) for (v in retenues[traitements[retenues] %in% c("simple", "knn")]) {
    methode <- traitements[[v]]
    quantitatif <- dictionnaire$type[match(v, variables)] == "Quantitative"
    for (i in which(is.na(observes[[v]]))) {
      portee <- if (methode == "simple") portee_simple else portee_knn
      candidats <- which(!is.na(observes[[v]]) &
        (portee == "global" | observes$provenance == observes$provenance[i]))
      valeur <- NA_real_
      message <- ""
      if (methode == "simple") {
        if (!length(candidats)) message <- "Aucune valeur observée dans le périmètre donneur : NA conservé."
        else valeur <- if (quantitatif) median(observes[[v]][candidats]) else modalite_frequente(observes[[v]][candidats])
      } else {
        kval <- k[[v]]
        if (is.null(kval) || !is.finite(kval) || kval < 1 || kval != floor(kval))
          stop("k doit être un entier positif.")
        distances <- distance_gower_observee(observes, i, candidats, setdiff(retenues, v), dictionnaire)
        admissibles <- which(is.finite(distances))
        if (length(admissibles) < kval) {
          message <- paste("Donneurs comparables insuffisants :", length(admissibles),
                           "disponibles pour k =", kval, "; NA conservé.")
          candidats <- candidats[admissibles]
        } else {
          # À distance égale, départager par identifiant stable ; jamais au hasard.
          ordre <- order(distances[admissibles], observes$id_ligne[candidats[admissibles]])
          candidats <- candidats[admissibles[ordre[seq_len(kval)]]]
          valeur <- if (quantitatif) median(observes[[v]][candidats]) else modalite_frequente(observes[[v]][candidats])
        }
      }
      if (is.finite(valeur)) {
        d[[v]][i] <- valeur
        masque[i, v] <- TRUE
      }
      if (is.finite(valeur) && all(is.na(initial[[v]][initial$provenance == observes$provenance[i]])))
        message <- "Variable entièrement manquante dans ce centre : estimation intercentres sous hypothèse de transférabilité."
      evenements <- rbind(evenements, data.frame(id_ligne = observes$id_ligne[i],
        provenance = observes$provenance[i], variable = v, methode = methode,
        statut = if (is.finite(valeur)) "Imputée" else "Échec : NA conservé",
        valeur = valeur, donneurs = paste(observes$id_ligne[candidats], collapse = " | "), message = message))
    }
  }
  journal <- do.call(rbind, lapply(variables, function(v) data.frame(
    Variable = v, Traitement = names(choix_traitements)[match(traitements[[v]], choix_traitements)],
    NA_avant = sum(is.na(initial[[v]])),
    Exclusions_pour_cette_variable = sum(exclusions$variable == v),
    NA_retires_avec_les_lignes = if (v %in% retenues) sum(is.na(initial[[v]][initial$id_ligne %in% exclusions$id_ligne])) else NA_integer_,
    Cellules_imputees = if (v %in% retenues) sum(masque[, v]) else 0L,
    Echecs_imputation = sum(evenements$variable == v & evenements$statut == "Échec : NA conservé"),
    NA_restants = if (v %in% retenues) sum(is.na(d[[v]])) else NA_integer_)))
  avant <- as.integer(table(factor(initial$provenance, levels = centres)))
  apres <- as.integer(table(factor(d$provenance, levels = centres)))
  bilan <- data.frame(provenance = centres, Avant = avant, Après = apres,
    Retirées = avant - apres, État = ifelse(apres == 0, "Aucune observation", "Centre présent"))
  list(initial = initial, donnees = d, observes = observes, predicteurs = retenues,
    centres = centres, masque = masque, exclusions = exclusions, evenements = evenements,
    journal = journal, bilan = bilan,
    parametres = list(centres = centres, traitements = traitements, k = k,
      portee_simple = portee_simple, portee_knn = portee_knn, imputer = imputer))
}

# Tableau long de chaque cellule, consultable et exportable avec son statut.
statuts_cellules <- function(scenario) {
  d <- scenario$donnees
  vide <- data.frame(id_ligne = character(), provenance = character(), variable = character(),
                      valeur = numeric(), statut = character())
  if (!length(scenario$predicteurs) || !nrow(d)) return(vide)
  do.call(rbind, lapply(scenario$predicteurs, function(v) data.frame(
    id_ligne = d$id_ligne, provenance = d$provenance, variable = v, valeur = d[[v]],
    statut = ifelse(is.na(d[[v]]), "Manquante", ifelse(scenario$masque[, v], "Imputée", "Observée")))))
}

bilan_cellules <- function(scenario) {
  lignes <- list()
  for (centre in scenario$centres) for (v in scenario$predicteurs) {
    idx <- which(scenario$donnees$provenance == centre)
    n <- length(idx)
    na <- sum(is.na(scenario$donnees[[v]][idx]))
    imp <- sum(scenario$masque[idx, v])
    lignes[[length(lignes) + 1L]] <- data.frame(provenance = centre, variable = v,
      n = n, n_na = na, n_imputees = imp, n_observees = n - na - imp,
      pct_na = if (n) 100 * na / n else NA_real_,
      etiquette = if (n) sprintf("%.1f %%\n%d NA", 100 * na / n, na) else "aucune\nobservation")
  }
  if (!length(lignes)) return(data.frame())
  do.call(rbind, lignes)
}
