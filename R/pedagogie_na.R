# Blocs pédagogiques de Gestion des NA. Aucune imputation ni mutation des données.
# Le périmètre est fourni explicitement pour exclure les identifiants techniques.
calculer_perte_information <- function(donnees, variables) {
  if (!length(variables) || anyDuplicated(variables) ||
      !all(variables %in% names(donnees))) stop("Périmètre d'analyse invalide.")
  masque <- is.na(donnees[variables])
  incomplets <- rowSums(masque) > 0
  n_cells <- length(masque)
  n_na <- sum(masque)
  n_observed <- n_cells - n_na
  observed_lost <- sum(!masque[incomplets, , drop = FALSE])
  observed_retained <- sum(!masque[!incomplets, , drop = FALSE])
  ratio <- function(a, b) if (b == 0) NA_real_ else a / b
  list(variables = variables, n_rows = nrow(donnees), n_variables = length(variables),
       n_cells = n_cells, n_na = n_na, n_observed = n_observed,
       incomplete_rows = sum(incomplets), complete_rows = sum(!incomplets),
       pct_na = ratio(n_na, n_cells),
       pct_rows_deleted = ratio(sum(incomplets), nrow(donnees)),
       observed_lost = observed_lost, observed_retained = observed_retained,
       pct_observed_information_lost = ratio(observed_lost, n_observed),
       observed_lost_per_na = ratio(observed_lost, n_na))
}

# Les champs pct_* sont des proportions [0, 1] ; conversion en % à l'affichage.
format_nombre_na <- function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE)
format_pct_na <- function(x) if (is.na(x)) "Non défini" else sprintf("%.1f %%", 100 * x)

phrase_perte_information <- function(bilan) {
  if (!bilan$n_cells) return("Aucune cellule dans le périmètre d'analyse.")
  if (!bilan$n_na) return("Aucune valeur manquante : tous les individus et toutes les valeurs observées sont conservés.")
  paste0("Supprimer les individus incomplets ferait perdre ",
    format_nombre_na(bilan$observed_lost), " valeurs observées pour seulement ",
    format_nombre_na(bilan$n_na), " valeurs manquantes retirées, soit ",
    sprintf("%.1f", bilan$observed_lost_per_na), " valeurs observées perdues par NA.")
}

cartes_perte_information <- function(bilan) {
  boite <- function(titre, valeur, note, icone) bslib::value_box(
    title = titre, value = valeur, shiny::p(note),
    showcase = icone_heart(icone, "1.8em"),
    theme = bslib::value_box_theme(bg = palette_heart$panel, fg = palette_heart$text),
    fill = FALSE)
  bslib::layout_columns(
    boite("Cellules manquantes", format_pct_na(bilan$pct_na),
      paste(format_nombre_na(bilan$n_na), "NA sur", format_nombre_na(bilan$n_cells), "cellules"), "grid-3x3"),
    boite("Individus supprimés", format_pct_na(bilan$pct_rows_deleted),
      paste(format_nombre_na(bilan$incomplete_rows), "sur", format_nombre_na(bilan$n_rows), "individus"), "person-dash"),
    boite("Information observée perdue", format_pct_na(bilan$pct_observed_information_lost),
      paste(format_nombre_na(bilan$observed_lost), "sur", format_nombre_na(bilan$n_observed), "valeurs observées"), "database-dash"),
    col_widths = bslib::breakpoints(xs = 12, md = 4), fill = FALSE)
}

graphique_perte_information <- function(bilan) {
  categories <- c("Observées conservées", "Observées jetées", "Valeurs manquantes")
  d <- data.frame(categorie = factor(categories, levels = categories),
    n = c(bilan$observed_retained, bilan$observed_lost, bilan$n_na))
  # Intervalles explicites : la barre représente exactement toutes les cellules.
  d$xmax <- cumsum(d$n)
  d$xmin <- d$xmax - d$n
  d$milieu <- (d$xmin + d$xmax) / 2
  d$label <- ifelse(d$n > 0, paste0(format_nombre_na(d$n), "\n",
    if (bilan$n_cells) sprintf("%.1f %%", 100 * d$n / bilan$n_cells) else ""), "")
  g <- ggplot2::ggplot(d) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = .15, ymax = .85, fill = categorie),
      colour = palette_heart$card, linewidth = 1) +
    ggplot2::scale_fill_manual(values = setNames(c(palette_heart$blue, palette_heart$red, palette_heart$amber),
      categories), drop = FALSE, name = NULL) +
    ggplot2::scale_x_continuous(limits = c(0, max(1, bilan$n_cells)), expand = c(0, 0),
      breaks = NULL) + ggplot2::scale_y_continuous(limits = c(0, 1), breaks = NULL) +
    ggplot2::labs(x = NULL, y = NULL,
      subtitle = paste(format_nombre_na(bilan$n_rows), "individus ×", bilan$n_variables,
        "variables =", format_nombre_na(bilan$n_cells), "cellules")) +
    theme_heart() + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom", legend.text = ggplot2::element_text(size = 13)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  # Pour les très petits segments, éviter un texte qui déborde de la barre.
  lisibles <- d[d$n > 0 & d$n / max(1, bilan$n_cells) >= .07, ]
  g + ggplot2::geom_text(data = lisibles, ggplot2::aes(milieu, .5, label = label),
    colour = palette_heart$bg, fontface = "bold", size = 4.5)
}

# Schéma statique en deux rangées, avec un panneau commun aux modèles comparés.
# Les noms proviennent du benchmark sauvegardé : aucune méthode fictive ajoutée.
graphique_pipeline_benchmark <- function(modeles, repetitions = 30L, proportion = .1) {
  noms <- c(mean = "Moyenne", median = "Médiane", mode = "Mode", RandomForest = "Random Forest",
    Knn = "KNN", ACP = "ACP", AFDM = "AFDM", EM = "EM", mice = "mice / PMM*")
  modeles <- unique(modeles)
  etiquettes <- unname(noms[modeles])
  etiquettes[is.na(etiquettes)] <- modeles[is.na(etiquettes)]
  etapes <- data.frame(
    x = c(1.65, 4.85, 8.05, 11.25, 11.25, 8.05, 4.85, 1.65),
    y = c(rep(8.6, 4), rep(1.5, 4)),
    label = c("1 · Valeur connue\nchol = 243", "2 · Masquage artificiel\nchol = NA",
      "3 · Imputation\nconditionnelle", "4 · Valeur prédite\nchol = 238",
      "5 · Comparaison au vrai\n238 − 243 = −5", "6 · Erreur sur le masque\nRMSE / taux d'erreur",
      paste0("7 · Répétitions\n", repetitions, " simulations"), "8 · Meilleure méthode\npour chaque variable"))
  fleches <- data.frame(x = c(3.03, 6.23, 9.43, 11.25, 9.87, 6.67, 3.47),
    xend = c(3.47, 6.67, 9.87, 11.25, 9.43, 6.23, 3.03),
    y = c(8.6, 8.6, 8.6, 7.98, 1.5, 1.5, 1.5),
    yend = c(8.6, 8.6, 8.6, 2.12, 1.5, 1.5, 1.5))
  ncol <- 3L
  nr <- max(1, ceiling(length(etiquettes) / ncol))
  m <- data.frame(label = etiquettes,
    x = rep(c(1.8, 4.85, 7.9), length.out = length(etiquettes)),
    y = 6.0 - (seq_along(etiquettes) - 1L) %/% ncol * (2.1 / max(1, nr - 1)))
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = .15, xmax = 9.55, ymin = 2.8, ymax = 7.15,
      fill = palette_heart$panel, colour = palette_heart$line, linewidth = .8) +
    ggplot2::annotate("text", x = 4.85, y = 6.8,
      label = paste0("UN MÊME MASQUE POUR TOUTES LES MÉTHODES · ",
        round(100 * proportion), " % des valeurs observées"),
      colour = palette_heart$amber, fontface = "bold", size = 4) +
    ggplot2::geom_segment(data = fleches, ggplot2::aes(x, y, xend = xend, yend = yend),
      colour = palette_heart$blue, linewidth = .8,
      arrow = grid::arrow(length = grid::unit(.16, "cm"), type = "closed")) +
    ggplot2::annotate("segment", x = 4.85, xend = 4.85, y = 7.98, yend = 7.2,
      colour = palette_heart$amber, linetype = "dashed",
      arrow = grid::arrow(length = grid::unit(.15, "cm"))) +
    ggplot2::annotate("segment", x = 8.05, xend = 8.05, y = 7.2, yend = 7.98,
      colour = palette_heart$amber, linetype = "dashed",
      arrow = grid::arrow(length = grid::unit(.15, "cm"))) +
    ggplot2::geom_label(data = m, ggplot2::aes(x, y, label = label),
      fill = palette_heart$card, colour = palette_heart$text, size = 4.2,
      label.padding = grid::unit(.3, "lines"), linewidth = .3) +
    ggplot2::annotate("text", x = 4.85, y = 3.15,
      label = "Prédiction à partir des autres variables.
La valeur vraie masquée est réservée à l’évaluation.",
      colour = palette_heart$muted, size = 3.7) +
    ggplot2::geom_label(data = etapes, ggplot2::aes(x, y, label = label),
      fill = palette_heart$card, colour = palette_heart$text, size = 4.2,
      label.padding = grid::unit(.55, "lines"), linewidth = .5) +
    ggplot2::annotate("label", x = 11.25, y = 5.2,
      label = "Comparer\nchaque prédiction\nà la valeur\nconnue : 243", colour = palette_heart$muted, fill = palette_heart$card,
      linewidth = 0, label.padding = grid::unit(.4, "lines"), size = 4) +
    ggplot2::coord_cartesian(xlim = c(0, 12.9), ylim = c(.5, 9.6), clip = "off") +
    ggplot2::labs(x = NULL, y = NULL,
      caption = "Exemple illustratif : 243 → 238. RMSE pour les mesures ; taux d'erreur pour les catégories.\n* PMM pour les cibles quantitatives dans mice ; modèles de classification pour les catégories.") +
    theme_heart() + ggplot2::theme(axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
}
