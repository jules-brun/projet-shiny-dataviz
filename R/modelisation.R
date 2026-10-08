# Comparaison exploratoire de deux modèles emboîtés sur un échantillon commun.
comparer_scenario <- function(scenario, variable, dictionnaire) {
  predicteurs <- scenario$predicteurs
  d <- scenario$donnees
  formuler <- function(x) if (length(x)) reformulate(x, response = "diagnostic") else diagnostic ~ 1
  complet <- formuler(predicteurs)
  reduit <- formuler(setdiff(predicteurs, variable))
  formules <- c(paste("Complet :", paste(deparse(complet), collapse = " ")),
                paste("Réduit :", paste(deparse(reduit), collapse = " ")))
  utilisables <- complete.cases(d[c("diagnostic", predicteurs)])
  echantillon <- d[utilisables, , drop = FALSE]
  ids <- echantillon$id_ligne
  imputes <- if (length(predicteurs)) rowSums(scenario$masque[utilisables, , drop = FALSE]) > 0 else rep(FALSE, length(ids))
  effectifs <- data.frame(provenance = scenario$centres,
    Patients = as.integer(table(factor(echantillon$provenance, levels = scenario$centres))))
  resultat <- list(formules = formules, ids = ids, effectifs = effectifs,
    resume = data.frame(Patients = nrow(echantillon), Diagnostics_négatifs = sum(echantillon$diagnostic == 0),
      Diagnostics_positifs = sum(echantillon$diagnostic == 1),
      Exclus_pour_NA_résiduels = sum(!utilisables), Patients_avec_imputation = sum(imputes)),
    messages = character(), statistiques = NULL, odds_ratio = NULL,
    inferential = !any(imputes), parametres = scenario$parametres)
  messages <- character()
  tryCatch({
    if (!length(predicteurs)) stop("Aucun prédicteur conservé : comparaison impossible.")
    if (!variable %in% predicteurs) stop("La variable évaluée n'est pas conservée.")
    if (!nrow(echantillon)) stop("Aucun cas complet disponible.")
    if (length(unique(echantillon$diagnostic)) != 2L)
      stop("Le diagnostic doit comporter des cas positifs et négatifs.")
    categories <- intersect(predicteurs, dictionnaire$nom_fr[dictionnaire$type == "Catégorielle"])
    modele_donnees <- echantillon[c("diagnostic", predicteurs)]
    for (v in predicteurs) {
      if (length(unique(modele_donnees[[v]])) < 2L)
        stop(paste("Absence de variation pour", v, ": modèle complet non identifiable."))
      if (v %in% categories) {
        attendues <- dictionnaire$modalites[[match(v, dictionnaire$nom_fr)]]
        valeurs <- sort(unique(modele_donnees[[v]]))
        if (!all(valeurs %in% attendues)) stop(paste("Modalité invalide pour", v))
        absentes <- setdiff(attendues, valeurs)
        if (length(absentes)) messages <- c(messages, paste("Modalités absentes pour", v, ":", paste(absentes, collapse = ", "), "; elles ne peuvent pas être estimées."))
        modele_donnees[[v]] <- factor(modele_donnees[[v]], levels = valeurs)
      }
    }
    # Les identifiants servent aux contrôles ; ils ne figurent pas dans les données des modèles.
    rownames(modele_donnees) <- ids
    avertissements <- character()
    ajustements <- withCallingHandlers({
      entier <- glm(complet, data = modele_donnees, family = binomial(), na.action = na.fail)
      restreint <- glm(reduit, data = modele_donnees, family = binomial(), na.action = na.fail)
      list(entier = entier, restreint = restreint)
    }, warning = function(w) {
      avertissements <<- c(avertissements, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    entier <- ajustements$entier
    restreint <- ajustements$restreint
    stopifnot(identical(rownames(model.frame(entier)), rownames(model.frame(restreint))),
              identical(rownames(model.frame(entier)), ids))
    if (!entier$converged || !restreint$converged) stop("Échec de convergence : comparaison non interprétable.")
    if (anyNA(coef(entier)) || anyNA(coef(restreint))) stop("Coefficients non identifiables : examiner les catégories et la colinéarité.")
    if (entier$df.residual <= 0) stop("Degrés de liberté résiduels insuffisants.")
    extreme <- any(fitted(entier) < 1e-8 | fitted(entier) > 1 - 1e-8) ||
      any(fitted(restreint) < 1e-8 | fitted(restreint) > 1 - 1e-8)
    if (length(avertissements) || extreme) {
      messages <- c(messages, "Ajustement potentiellement instable : avertissement de R ou probabilités proches de 0/1. Une séparation est possible ; aucune conclusion automatique n'est formulée.")
      # Le détail natif est conservé dans l'objet de résultat pour audit.
      resultat$avertissements_R <- unique(avertissements)
      resultat$inferential <- FALSE
    }
    if (all(as.integer(fitted(entier) >= .5) == modele_donnees$diagnostic)) {
      messages <- c(messages, "Classification parfaite des données d'ajustement : suspicion de séparation à examiner.")
      resultat$inferential <- FALSE
    }
    comparaison <- anova(restreint, entier, test = "LRT")
    if (!is.finite(comparaison[[5]][2]) || comparaison[[3]][2] <= 0)
      stop("Test du rapport de vraisemblance indisponible ou degrés de liberté insuffisants.")
    resultat$statistiques <- data.frame(
      Écart_déviance = comparaison[[4]][2], DDL = comparaison[[3]][2],
      Valeur_p_LRT = comparaison[[5]][2], AIC_réduit = AIC(restreint), AIC_complet = AIC(entier),
      AIC_réduit_moins_complet = AIC(restreint) - AIC(entier))
    if (dictionnaire$type[match(variable, dictionnaire$nom_fr)] == "Quantitative") {
      increment <- if (variable == "cholesterol") 10 else 1
      beta <- coef(entier)[[variable]] * increment
      erreur <- sqrt(vcov(entier)[variable, variable]) * increment
      resultat$odds_ratio <- data.frame(
        Interprétation = paste("Pour une augmentation de", increment,
          dictionnaire$unite[match(variable, dictionnaire$nom_fr)], "à autres variables constantes"),
        OR_ajusté = exp(beta), IC95_inf = exp(beta - qnorm(.975) * erreur),
        IC95_sup = exp(beta + qnorm(.975) * erreur))
      if (!all(is.finite(unlist(resultat$odds_ratio[-1])))) {
        messages <- c(messages, "Odds ratio ou intervalle non fini : estimation instable.")
        resultat$inferential <- FALSE
      }
    }
    if (any(imputes)) messages <- c(messages,
      "Valeurs imputées utilisées : p-values et IC à 95 % naïfs, calculés comme si ces estimations étaient connues. L'incertitude d'imputation n'est pas prise en compte.")
    resultat$ids_complet <- rownames(model.frame(entier))
    resultat$ids_reduit <- rownames(model.frame(restreint))
    resultat$messages <- messages
    resultat
  }, error = function(e) {
    resultat$messages <- c(messages, paste("Comparaison indisponible :", conditionMessage(e)))
    resultat$inferential <- FALSE
    resultat
  })
}
