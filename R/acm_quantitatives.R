# Variables quantitatives UCI, utilisées en supplémentaires dans l'ACM.
variables_quantitatives_app <- c("age", "pa_repos", "cholesterol", "fc_max", "depress_st")
libelles_quantitatives_app <- c(age = "Âge", pa_repos = "Pression au repos",
  cholesterol = "Cholestérol", fc_max = "Fréquence cardiaque maximale", depress_st = "Dépression ST")

calculer_quantitatives_acm <- function(donnees, selections, benchmark, m = 1L,
  graine = 20261010L, imputations = NULL) acm_avec_graine(graine, {
  stopifnot(length(m) == 1L, is.finite(m), m == as.integer(m), m >= 1L, m <= 100L)
  v <- variables_quantitatives_app
  b <- benchmark; b$cibles <- intersect(v, b$cibles)
  choix <- selections[b$cibles]
  reutiliser <- !is.null(imputations) && identical(imputations$selections[b$cibles], choix) &&
    identical(rownames(imputations$simple), rownames(donnees)) &&
    (m == 1L || length(imputations$jeux) >= m)
  if (reutiliser) {
    simple <- imputations$simple[v]
    jeux <- if (m == 1L) list(simple) else lapply(imputations$jeux[seq_len(m)], function(j) j[v])
  } else {
    simple <- imputer_jeu_app(donnees, choix, b, seed = graine)[v]
    jeux <- if (m == 1L) list(simple) else lapply(seq_len(m), function(i)
      imputer_jeu_app(donnees, choix, b, bootstrap = TRUE, seed = graine + i)[v])
  }
  list(simple = simple, jeux = jeux, m = m, selections = choix,
       lignes = rownames(donnees), masque = is.na(donnees[v]))
})

graphique_correlations_factorielles <- function(res, axes = c(1L, 2L)) {
  afdm <- identical(res$analyse, "AFDM")
  q <- if (afdm) res$ref$quanti.var$coord else res$ref$quanti.sup$coord
  if (is.null(q) || !nrow(q)) stop("Aucune variable quantitative non constante à projeter.")
  stopifnot(length(unique(axes)) == 2L, all(axes >= 1L), all(axes <= ncol(q)))
  p <- data.frame(variable = rownames(q), x = q[, axes[1]], y = q[, axes[2]])
  p$libelle <- unname(libelles_quantitatives_app[p$variable])
  a <- seq(0, 2*pi, length.out = 181)
  cercle <- data.frame(x = cos(a), y = sin(a))
  titres <- sprintf("Corrélation avec la dimension %d (%.1f %%)", axes, res$ref$eig[axes, 2])
  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = palette_heart$line) +
    ggplot2::geom_vline(xintercept = 0, colour = palette_heart$line) +
    ggplot2::geom_path(data = cercle, ggplot2::aes(x, y), colour = palette_heart$muted) +
    ggplot2::geom_segment(data = p, ggplot2::aes(x = 0, y = 0, xend = x, yend = y, colour = variable),
      linewidth = .9, arrow = grid::arrow(length = grid::unit(.2, "cm")), show.legend = FALSE) +
    ggplot2::geom_point(data = p, ggplot2::aes(x, y, colour = variable), size = 3) +
    ggrepel::geom_text_repel(data = p, ggplot2::aes(x, y, label = libelle, colour = variable),
      size = 4.2, seed = 42, max.overlaps = Inf, min.segment.length = 0, show.legend = FALSE) +
    ggplot2::scale_colour_discrete(labels = libelles_quantitatives_app) +
    ggplot2::coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
    ggplot2::labs(x = titres[1], y = titres[2], colour = "Variable quantitative",
      caption = paste0(if (afdm) "Variables actives de l’AFDM" else "Variables supplémentaires de l’ACM",
        " · Corrélations du jeu de référence\nLa variabilité des imputations est représentée sur les cartes de points.")) +
    theme_heart() + ggplot2::theme(legend.position = "bottom")
}
