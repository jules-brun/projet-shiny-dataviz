# Matrice exploratoire sur les données nettoyées avant imputation.
# Les catégories viennent du dictionnaire ; aucun identifiant n'entre dans les tests.
# Seules ces neuf variables constituent les lignes d'indicateurs d'absence.
variables_absence_ciblees <- c("pa_repos", "cholesterol", "glyc_jeun_elevee",
  "fc_max", "angine_effort", "depress_st", "pente_st", "nb_vaisseaux", "test_thallium")
reglages_tests_absence <- list(n_classes = 3L, quantile_type = 7L,
  B = 100000L, graine = 20261003L, seuil = .05)

# Restaurer l'état aléatoire du reste de l'application après chaque simulation.
avec_graine_absence <- function(graine, calcul) {
  presente <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  ancienne <- if (presente) get(".Random.seed", envir = .GlobalEnv) else NULL
  genres <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(genres))
    if (presente) assign(".Random.seed", ancienne, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(graine, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  calcul()
}

classes_absence <- function(x, variable, reglages = reglages_tests_absence) {
  observes <- x[!is.na(x)]
  bornes <- if (length(observes)) unique(as.numeric(quantile(observes,
    probs = seq(0, 1, length.out = reglages$n_classes + 1L),
    type = reglages$quantile_type, names = FALSE))) else numeric()
  vide <- data.frame(variable = character(), classe = character(), borne_inf = numeric(),
    borne_sup = numeric(), inclusion = character(), n = integer())
  if (!length(bornes)) return(list(valeurs = factor(rep(NA_character_, length(x))),
    bornes = bornes, tableau = vide, raison = "Variable quantitative entièrement manquante."))
  if (length(bornes) == 1L) {
    etiquette <- paste("Valeur unique :", format(bornes, digits = 12))
    valeurs <- factor(ifelse(is.na(x), NA_character_, etiquette))
    tableau <- data.frame(variable = variable, classe = etiquette, borne_inf = bornes,
      borne_sup = bornes, inclusion = "Valeur unique", n = length(observes))
  } else {
    # min et max observés sont inclus ; les autres intervalles sont ouverts à gauche.
    etiquettes <- vapply(seq_len(length(bornes) - 1L), function(i) paste0(
      if (i == 1L) "[" else "]", format(bornes[i], digits = 12, trim = TRUE), "; ",
      format(bornes[i + 1L], digits = 12, trim = TRUE), "]"), character(1))
    valeurs <- cut(x, breaks = bornes, include.lowest = TRUE, right = TRUE, labels = etiquettes)
    tableau <- data.frame(variable = variable, classe = etiquettes,
      borne_inf = head(bornes, -1L), borne_sup = tail(bornes, -1L),
      inclusion = c("Bornes incluses", rep("Ouvert à gauche, fermé à droite", length(etiquettes) - 1L)),
      n = as.integer(table(valeurs)))
  }
  raison <- if (nlevels(droplevels(valeurs)) < 2L)
    "Moins de deux classes quantitatives non vides après retrait des coupures dupliquées." else ""
  list(valeurs = droplevels(valeurs), bornes = bornes, tableau = tableau, raison = raison)
}

preparer_variables_absence <- function(donnees, dictionnaire, reglages = reglages_tests_absence) {
  colonnes <- c(dictionnaire$nom_fr, "provenance", "diagnostic")
  quantitatives <- dictionnaire$nom_fr[dictionnaire$type == "Quantitative"]
  valeurs <- raisons <- coupures <- list()
  tableaux <- list()
  # Chaque découpage est calculé UNE fois et partagé par toutes les lignes X.
  for (v in colonnes) {
    if (v %in% quantitatives) {
      resultat <- classes_absence(donnees[[v]], v, reglages)
      valeurs[[v]] <- resultat$valeurs
      raisons[[v]] <- resultat$raison
      coupures[[v]] <- resultat$bornes
      tableaux[[v]] <- resultat$tableau
    } else {
      valeurs[[v]] <- droplevels(factor(donnees[[v]]))
      raisons[[v]] <- ""
    }
  }
  list(valeurs = valeurs, raisons = raisons, coupures = coupures,
       classes = if (length(tableaux)) do.call(rbind, tableaux) else classes_absence(numeric(), "")$tableau)
}

# Une erreur de calcul reste attachée à la paire ; elle n'arrête pas le balayage.
tester_paire_absence <- function(donnees, x, y, preparation,
                                reglages = reglages_tests_absence, graine = reglages$graine) {
  disponible <- !is.na(donnees[[y]])
  categorie <- droplevels(preparation$valeurs[[y]][disponible])
  statut <- droplevels(factor(as.integer(is.na(donnees[[x]][disponible])), levels = 0:1,
                             labels = c("Observée (0)", "Manquante (1)")))
  tab <- table(statut, categorie, dnn = c("Statut de X", "Catégorie de Y"))
  r <- data.frame(variable_absence = x, variable_croisee = y,
    n_utilise = sum(disponible), n_exclus_y_na = sum(!disponible),
    n_x_manquantes = sum(is.na(donnees[[x]][disponible])),
    n_x_observees = sum(!is.na(donnees[[x]][disponible])), n_modalites_y = nlevels(categorie),
    methode = NA_character_, attendu_min = NA_real_, proportion_attendus_inf_5 = NA_real_,
    statistique_pearson = NA_real_, ddl = NA_integer_, p_brute = NA_real_, p_ajustee = NA_real_,
    v_cramer = NA_real_, statut = "Non calculable", raison = "", simule = FALSE,
    B = NA_integer_, graine = NA_integer_, avertissement = "")
  if (identical(x, y)) {
    r$statut <- "Non applicable"
    r[c("n_utilise", "n_exclus_y_na", "n_x_manquantes", "n_x_observees", "n_modalites_y")] <- NA_integer_
    r$raison <- "L'indicateur d'absence n'est pas testé contre sa propre variable."
    return(list(resultat = r, contingence = NULL, attendus = NULL))
  }
  if (!r$n_utilise) r$raison <- "Aucune observation avec Y renseignée."
  else if (nlevels(statut) < 2L) r$raison <- "Un seul statut d'absence de X dans le sous-échantillon où Y est observée."
  else if (nzchar(preparation$raisons[[y]])) r$raison <- preparation$raisons[[y]]
  else if (nlevels(categorie) < 2L) r$raison <- "Moins de deux modalités observées de Y."
  if (nzchar(r$raison)) return(list(resultat = r, contingence = tab, attendus = NULL))
  attendus <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  dimnames(attendus) <- dimnames(tab)
  r$attendu_min <- min(attendus)
  r$proportion_attendus_inf_5 <- mean(attendus < 5)
  # Pearson non corrigé sert aussi au V descriptif lorsque la p-value est de Fisher.
  r$statistique_pearson <- sum((tab - attendus)^2 / attendus)
  r$ddl <- (nrow(tab) - 1L) * (ncol(tab) - 1L)
  r$v_cramer <- sqrt(r$statistique_pearson / (sum(tab) * min(nrow(tab) - 1L, ncol(tab) - 1L)))
  avertissements <- character()
  tryCatch({
    resultat <- withCallingHandlers({
      if (all(attendus >= 1) && mean(attendus >= 5) >= .8) {
        r$methode <- "Chi-deux de Pearson (sans correction)"
        chisq.test(tab, correct = FALSE)
      } else if (ncol(tab) == 2L) {
        r$methode <- "Fisher exact (bilatéral)"
        fisher.test(tab, alternative = "two.sided")
      } else {
        r$methode <- "Fisher — simulation Monte-Carlo"
        r$simule <- TRUE
        r$B <- reglages$B
        r$graine <- graine
        avec_graine_absence(graine, function() fisher.test(tab, simulate.p.value = TRUE, B = reglages$B))
      }
    }, warning = function(w) {
      avertissements <<- c(avertissements, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    p <- resultat$p.value
    if (!is.finite(p) || p < 0 || p > 1) stop("P-value non valide.")
    # Fisher simulé utilise (1 + nombre de réplications extrêmes) / (B + 1).
    # Si ce contrat n'est pas respecté, ne pas fabriquer une valeur de remplacement.
    if (r$simule && p < 1 / (reglages$B + 1) - .Machine$double.eps)
      stop("P-value simulée sous la résolution Monte-Carlo attendue.")
    r$p_brute <- p
    r$statut <- "Calculé"
  }, error = function(e) {
    r$statut <<- "Erreur de calcul"
    r$raison <<- conditionMessage(e)
  })
  r$avertissement <- paste(unique(avertissements), collapse = " ; ")
  list(resultat = r, contingence = tab, attendus = attendus)
}

cle_paire_absence <- function(x, y) paste(x, y, sep = "::")

analyser_absences <- function(donnees, dictionnaire, reglages = reglages_tests_absence,
                               variables_absence = variables_absence_ciblees) {
  stopifnot(reglages$n_classes >= 2, reglages$n_classes == floor(reglages$n_classes),
            reglages$B > 0, reglages$B == floor(reglages$B))
  candidates <- intersect(variables_absence, dictionnaire$nom_fr)
  lignes <- candidates[vapply(donnees[candidates], anyNA, logical(1))]
  colonnes <- c(dictionnaire$nom_fr, "provenance", "diagnostic")
  preparation <- preparer_variables_absence(donnees, dictionnaire, reglages)
  paires <- list()
  resultats <- list()
  # Les variables techniques ne peuvent pas être ajoutées par une formule implicite.
  for (x in lignes) for (y in colonnes) {
    indice <- match(x, dictionnaire$nom_fr) * length(colonnes) + match(y, colonnes)
    paire <- tester_paire_absence(donnees, x, y, preparation, reglages, reglages$graine + indice)
    paires[[cle_paire_absence(x, y)]] <- paire
    resultats[[length(resultats) + 1L]] <- paire$resultat
  }
  long <- if (length(resultats)) do.call(rbind, resultats) else {
    # Gabarit typé pour le cas où aucune variable explicative n'a de NA.
    gabarit <- tester_paire_absence(donnees, colonnes[1], colonnes[1], preparation, reglages)$resultat
    gabarit[FALSE, ]
  }
  valides <- long$statut == "Calculé" & is.finite(long$p_brute)
  long$p_ajustee[valides] <- p.adjust(long$p_brute[valides], method = "BH")
  long$affichage <- ifelse(long$statut == "Non applicable", "Non applicable",
    ifelse(!valides, "Non calculable", ifelse(long$p_ajustee < reglages$seuil,
      "Association détectée (BH < 5 %)", "Non significatif (BH ≥ 5 %)")))
  list(resultats = long, paires = paires, preparation = preparation, lignes = lignes,
    colonnes = colonnes, n_total = nrow(donnees), n_tests_valides = sum(valides),
    reglages = reglages, perimetre = "Toutes les données nettoyées des quatre centres, avant imputation")
}

formater_p_absence <- function(p) {
  ifelse(is.na(p), "—", ifelse(p == 0, "< 1e-300", formatC(p, digits = 3, format = "g")))
}

synthese_ligne_absence <- function(analyse, x, libelles) {
  d <- analyse$resultats[analyse$resultats$variable_absence == x, ]
  associees <- d$variable_croisee[is.finite(d$p_ajustee) & d$p_ajustee < analyse$reglages$seuil]
  impossibles <- d$variable_croisee[d$statut %in% c("Non calculable", "Erreur de calcul")]
  if (!nrow(d)) return("Cette variable n'a pas de NA dans le périmètre : elle ne constitue pas une ligne de la matrice.")
  texte <- if (!any(d$statut == "Calculé"))
    "Aucun test n'est réalisable sur cette ligne : aucune conclusion d'association ne peut être tirée."
  else if (length(associees)) paste0("Une association avec l'absence est détectée pour : ",
    paste(libelles[associees], collapse = ", "),
    ". Cela peut remettre en cause l'hypothèse MCAR sur le périmètre étudié.") else
    "Aucune association n'a été détectée avec les variables et découpages examinés ; cela ne démontre pas MCAR."
  global <- d[d$variable_croisee == "provenance", ]
  compte <- if (nrow(global)) paste(global$n_x_manquantes, "valeurs manquantes sur", analyse$n_total,
                                  "observations avant les exclusions propres à Y.") else ""
  paste(compte, texte,
    if (length(impossibles)) paste0("Tests non réalisables : ", paste(libelles[impossibles], collapse = ", "), ".") else "",
    "Les nombres d'absences utilisés dépendent de Y ; un faible effectif limite la puissance. Les raisons et effectifs exacts sont consultables dans le tableau détaillé.")
}

# Seule la matrice est affichée ; aucun autre contrôle dans le quatrième onglet.
serveur_tests_absence <- function(input, output, session, analyse, dictionnaire, libelles_sources) {
  libelles <- c(setNames(dictionnaire$libelle, dictionnaire$nom_fr),
                provenance = "Provenance", diagnostic = "Diagnostic binaire")
  output$absence_matrice <- renderPlot({
    d <- analyse$resultats
    validate(need(nrow(d) > 0, "Aucune variable explicative ne contient de NA dans ce périmètre."))
    d$variable_absence <- factor(d$variable_absence, levels = rev(analyse$lignes))
    d$variable_croisee <- factor(d$variable_croisee, levels = analyse$colonnes)
    d$texte <- ifelse(d$statut == "Non applicable", "Non\napplicable",
      ifelse(d$statut != "Calculé", "Non\ncalculable", formater_p_absence(d$p_ajustee)))
    categories <- c("Association détectée (BH < 5 %)", "Non significatif (BH ≥ 5 %)",
                    "Non calculable", "Non applicable")
    d$affichage <- factor(d$affichage, levels = categories)
    ggplot(d, aes(variable_croisee, variable_absence, fill = affichage)) +
      geom_tile(colour = "white", linewidth = .6) +
      geom_text(aes(label = texte, colour = affichage == categories[1]), size = 2.8, show.legend = FALSE) +
      scale_color_manual(values = c("FALSE" = "#143052", "TRUE" = "white")) +
      scale_fill_manual(values = setNames(c("#165DDE", "#EAF2FF", "#CBD5E1", "#F3F4F6"), categories), drop = FALSE) +
      scale_x_discrete(labels = function(x) vapply(libelles[x], function(t) paste(strwrap(t, width = 23), collapse = "\n"), character(1)), drop = FALSE) +
      scale_y_discrete(labels = libelles, drop = FALSE) +
      labs(x = "Variable Y croisée (sur les observations où elle est renseignée)",
        y = "Variable X dont on étudie l'absence", fill = NULL,
        caption = paste(analyse$n_total, "observations nettoyées des quatre centres, sans imputation —",
          analyse$n_tests_valides, "tests valides, correction BH globale. P-values Monte-Carlo estimées ; diagonale non testée.")) +
      theme_minimal(base_size = 10) + theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 2))
  }, res = 110)
}
