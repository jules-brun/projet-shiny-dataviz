# AFDM sur les 13 mesures cliniques, diagnostic et provenance supplémentaires.
# Un seul ajustement : les jeux multiples sont projetés dans ce même repère.
tableau_afdm_app <- function(d, variables, niveaux = NULL) {
  x <- d[variables]
  for (v in setdiff(variables, variables_quantitatives_app)) {
    valeurs <- paste0(v, " : ", d[[v]])
    x[[v]] <- if (is.null(niveaux)) factor(valeurs) else factor(valeurs, levels = niveaux[[v]])
  }
  x
}

modalites_afdm_app <- function(coord, facteurs) {
  do.call(rbind, lapply(names(facteurs), function(v) {
    lev <- levels(facteurs[[v]])
    lev <- lev[vapply(lev, function(l) any(facteurs[[v]] == l, na.rm = TRUE), logical(1))]
    if (!length(lev)) return(NULL)
    z <- t(vapply(lev, function(l) colMeans(coord[which(facteurs[[v]] == l), , drop = FALSE]),
      numeric(ncol(coord))))
    rownames(z) <- lev; colnames(z) <- colnames(coord); z
  }))
}

associations_afdm_app <- function(coord, x) {
  # r² pour une mesure ; rapport de corrélation η² pour une catégorie.
  z <- t(vapply(x, function(v) {
    if (is.numeric(v)) return(as.numeric(stats::cor(v, coord))^2)
    vapply(seq_len(ncol(coord)), function(k) {
      ok <- !is.na(v); y <- coord[ok, k]; f <- droplevels(v[ok])
      if (!length(y) || sum((y - mean(y))^2) < 1e-14) return(NA_real_)
      moyennes <- tapply(y, f, mean)
      sum((moyennes[as.character(f)] - mean(y))^2) / sum((y - mean(y))^2)
    }, numeric(1))
  }, numeric(ncol(coord))))
  colnames(z) <- colnames(coord); z
}

calculer_afdm_app <- function(imputations) {
  d <- imputations$simple
  variables <- c(variables_quantitatives_app, variables_acm_app)
  if (!length(imputations$jeux) || length(imputations$jeux) != imputations$m)
    stop("Nombre de jeux AFDM incohérent.")
  for (j in c(list(d), imputations$jeux)) {
    if (!all(variables %in% names(j)) || anyNA(j[variables]) ||
        any(!is.finite(as.matrix(j[variables_quantitatives_app]))))
      stop("Compléter toutes les variables sur Gestion des NA avant de calculer l’AFDM.")
    if (!identical(rownames(j), rownames(d)) || !identical(j$diagnostic, d$diagnostic) ||
        !identical(j$provenance, d$provenance)) stop("Jeu AFDM mal aligné.")
  }
  x <- tableau_afdm_app(d, variables)
  constantes <- names(x)[vapply(x, function(v) length(unique(v)) < 2L, logical(1))]
  variables <- setdiff(variables, constantes); x <- x[variables]
  if (nrow(x) < 5L || !any(vapply(x, is.numeric, logical(1))) ||
      !any(vapply(x, is.factor, logical(1)))) stop("AFDM : au moins cinq individus et des variables mixtes non constantes sont nécessaires.")
  niveaux <- lapply(x[vapply(x, is.factor, logical(1))], levels)
  diagnostic <- factor(paste0("diagnostic : ", d$diagnostic),
    levels = paste0("diagnostic : ", 0:1))
  provenance <- factor(paste0("provenance : ", d$provenance))
  base <- x
  if (!anyNA(diagnostic) && nlevels(droplevels(diagnostic)) > 1L) base$diagnostic <- diagnostic
  if (nlevels(provenance) > 1L) base$provenance <- provenance
  sup <- intersect(c("diagnostic", "provenance"), names(base))
  ncp <- min(nrow(x) - 1L, sum(vapply(x, function(v) if (is.numeric(v)) 1L else nlevels(v) - 1L, integer(1))))
  ref <- FactoMineR::FAMD(base, sup.var = if (length(sup)) match(sup, names(base)) else NULL,
    ncp = ncp, graph = FALSE)
  if (ncol(ref$ind$coord) < 2L) stop("AFDM : deux dimensions exploitables sont nécessaires.")
  coords <- lapply(imputations$jeux, function(j) {
    xm <- tableau_afdm_app(j, variables, niveaux)
    if (anyNA(xm)) stop("Modalité inconnue dans un jeu AFDM.")
    as.matrix(predict(ref, newdata = xm)$coord)
  })
  facteurs <- function(j) {
    xm <- tableau_afdm_app(j, variables, niveaux)
    cbind(xm[vapply(xm, is.factor, logical(1))], diagnostic = diagnostic, provenance = provenance)
  }
  modalites <- lapply(seq_along(coords), function(i) modalites_afdm_app(coords[[i]], facteurs(imputations$jeux[[i]])))
  associations <- lapply(seq_along(coords), function(i) {
    xm <- tableau_afdm_app(imputations$jeux[[i]], variables, niveaux)
    associations_afdm_app(coords[[i]], cbind(xm, diagnostic = diagnostic, provenance = provenance))
  })
  masque <- imputations$masque_complet
  if (is.null(masque) || nrow(masque) != nrow(d) || !all(variables %in% colnames(masque)))
    stop("Le masque des valeurs manquantes de l’AFDM est incomplet.")
  if (!is.null(rownames(masque)) && !identical(rownames(masque), rownames(d)))
    stop("Masque AFDM mal aligné.")
  list(ref = ref, individus = coords, modalites = modalites, associations = associations,
    diagnostic = factor(d$diagnostic, levels = 0:1), provenance = factor(d$provenance),
    impute = rowSums(masque[, variables, drop = FALSE]) > 0L,
    m = imputations$m, variables = variables, variables_exclues = constantes,
    individus_reference = FALSE, analyse = "AFDM", supplementaires = sup,
    selections = imputations$selections)
}

graphique_variables_afdm <- function(res, axes = c(1L, 2L), ellipses = FALSE) {
  nuage <- nuage_acm_app(res, "associations", axes)
  points <- points_acm_app(res, "associations", axes)
  points$supplementaire <- points$id %in% res$supplementaires
  points$libelle <- unname(c(libelles_variables_acm, libelles_quantitatives_app)[points$id])
  g <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = palette_heart$line) +
    ggplot2::geom_vline(xintercept = 0, colour = palette_heart$line)
  if (ellipses && res$m >= 3L) {
    e <- ellipses_app(nuage)
    if (!is.null(e)) g <- g + ggplot2::geom_path(data = e, ggplot2::aes(x, y, group = id, colour = id),
      linewidth = .7, show.legend = FALSE)
  }
  g + ggplot2::geom_point(data = points, ggplot2::aes(x, y, colour = id, shape = supplementaire), size = 3) +
    ggrepel::geom_text_repel(data = points, ggplot2::aes(x, y, colour = id, label = libelle),
      seed = 42, max.overlaps = Inf, size = 4.2, show.legend = FALSE) +
    ggplot2::scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 18), guide = "none") +
    ggplot2::scale_colour_discrete(labels = c(libelles_variables_acm, libelles_quantitatives_app), guide = "none") +
    ggplot2::coord_equal() + ggplot2::labs(
      x = sprintf("Lien à la dimension %d (%.1f %%)", axes[1], res$ref$eig[axes[1], 2]),
      y = sprintf("Lien à la dimension %d (%.1f %%)", axes[2], res$ref$eig[axes[2], 2]),
      caption = paste0("r² pour les mesures ; η² pour les catégories · Positions moyennes des jeux imputés",
        "\nDiagnostic et provenance supplémentaires",
        if (ellipses) "\nEllipses descriptives de variabilité d’imputation." else "")) + theme_heart()
}
