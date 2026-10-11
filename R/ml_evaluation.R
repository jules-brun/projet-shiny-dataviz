# Mesures pour diagnostic 1 = maladie. Orientation ROC fixée, jamais auto-inversée.
metriques_prediction_ml <- function(y, p, seuil = .5) {
  stopifnot(length(y) == length(p), all(y %in% 0:1), all(is.finite(p)), all(p >= 0 & p <= 1))
  pred <- as.integer(p >= seuil)
  tn <- sum(y == 0 & pred == 0); fp <- sum(y == 0 & pred == 1)
  fn <- sum(y == 1 & pred == 0); tp <- sum(y == 1 & pred == 1)
  ratio <- function(a, b) if (b == 0) NA_real_ else a/b
  sens <- ratio(tp, tp+fn); spec <- ratio(tn, tn+fp); precision <- ratio(tp, tp+fp)
  auc <- if (length(unique(y)) == 2) as.numeric(pROC::auc(pROC::roc(y, p,
    levels = c(0, 1), direction = "<", quiet = TRUE))) else NA_real_
  # Average precision : aire PR en escalier, ties regroupés.
  niveaux <- sort(unique(p), decreasing = TRUE)
  if (sum(y == 1) > 0) {
    groupes <- match(p, niveaux)
    pos <- tabulate(groupes[y == 1], length(niveaux)); total <- tabulate(groupes, length(niveaux))
    pr_auc <- sum(pos / sum(pos) * cumsum(pos) / cumsum(total))
  } else pr_auc <- NA_real_
  c(roc_auc = auc, sensitivity = sens, specificity = spec, accuracy = mean(pred == y),
    balanced_accuracy = mean(c(sens, spec)), precision = precision,
    f1 = ratio(2*tp, 2*tp+fp+fn), pr_auc = pr_auc, brier = mean((p-y)^2))
}
resume_metriques_ml <- function(d) {
  metriques <- c("roc_auc", "sensitivity", "specificity", "accuracy", "balanced_accuracy", "precision", "f1", "pr_auc", "brier")
  do.call(rbind, lapply(split(d, d$candidat), function(z) {
    resultat <- data.frame(candidat = z$candidat[1], modele = z$modele[1], folds = nrow(z))
    for (v in metriques) {
      resultat[[v]] <- mean(z[[v]], na.rm = TRUE)
      resultat[[paste0(v, "_sd")]] <- sd(z[[v]], na.rm = TRUE)
    }
    resultat
  }))
}
roc_points_ml <- function(y, p) {
  if (length(unique(y)) < 2) return(data.frame(fpr = numeric(), tpr = numeric()))
  r <- pROC::roc(y, p, levels = c(0, 1), direction = "<", quiet = TRUE)
  data.frame(fpr = 1-r$specificities, tpr = r$sensitivities)
}
calibration_ml <- function(y, p, groupes = 5L) {
  # Bornes fixes 0, .2, ... 1 ; aucune nouvelle sélection sur TEST.
  bin <- cut(p, seq(0, 1, length.out = groupes+1), include.lowest = TRUE)
  do.call(rbind, lapply(split(seq_along(p), bin, drop = TRUE), function(i) {
    n <- length(i); freq <- mean(y[i]); z <- qnorm(.975)
    centre <- (freq + z^2/(2*n))/(1+z^2/n)
    marge <- z*sqrt(freq*(1-freq)/n+z^2/(4*n^2))/(1+z^2/n)
    data.frame(predite = mean(p[i]), observee = freq, n = n, bas = centre-marge, haut = centre+marge)
  }))
}
robustesse_provenance_ml <- function(predictions, seuil) {
  do.call(rbind, lapply(split(predictions, predictions$provenance), function(d) {
    n1 <- sum(d$diagnostic == 1); n0 <- sum(d$diagnostic == 0)
    m <- metriques_prediction_ml(d$diagnostic, d$probabilite, seuil)
    # Les métriques sur des effectifs trop faibles ne sont pas interprétées.
    if (n1 < 10) m["sensitivity"] <- NA
    if (n0 < 10) m["specificity"] <- NA
    if (n1 < 10 || n0 < 10 || nrow(d) < 30) m[c("roc_auc", "balanced_accuracy")] <- NA
    data.frame(provenance = d$provenance[1], n = nrow(d), n_malades = n1, n_non_malades = n0,
      sensitivity = unname(m["sensitivity"]), specificity = unname(m["specificity"]),
      roc_auc = unname(m["roc_auc"]),
      note = if (n1 < 10 || n0 < 10 || nrow(d) < 30)
        "Effectifs faibles : certaines métriques ne sont pas présentées." else "Estimation exploratoire par centre.")
  }))
}
