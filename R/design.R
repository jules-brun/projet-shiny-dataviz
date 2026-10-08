# Identité visuelle partagée : aucune transformation des données.
palette_heart <- list(bg = "#080F1B", card = "#111D2D", panel = "#162438",
  text = "#EEF3FA", muted = "#A2B2C7", line = "#26364C",
  red = "#EF6473", blue = "#78A9DF", teal = "#64C4B2", amber = "#E5B36C")

theme_heart <- function(base_size = 12, base_family = "sans") {
  p <- palette_heart
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = p$text),
      plot.background = ggplot2::element_rect(fill = p$card, colour = NA),
      panel.background = ggplot2::element_rect(fill = p$card, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = p$line, linewidth = .3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = p$muted),
      axis.title = ggplot2::element_text(colour = p$muted),
      plot.title = ggplot2::element_text(face = "bold", margin = ggplot2::margin(b = 12)),
      plot.subtitle = ggplot2::element_text(colour = p$muted, margin = ggplot2::margin(b = 12)),
      plot.caption = ggplot2::element_text(colour = p$muted, hjust = 0, size = base_size - 2,
        margin = ggplot2::margin(t = 16)),
      legend.background = ggplot2::element_rect(fill = p$card, colour = NA),
      legend.key = ggplot2::element_rect(fill = p$card, colour = NA),
      legend.text = ggplot2::element_text(colour = p$muted),
      legend.title = ggplot2::element_text(colour = p$text),
      strip.background = ggplot2::element_rect(fill = p$panel, colour = NA),
      strip.text = ggplot2::element_text(colour = p$text, face = "bold", margin = ggplot2::margin(8, 8, 8, 8)),
      plot.margin = ggplot2::margin(14, 18, 14, 12)
    )
}

icone_heart <- function(nom, taille = "1.15em") bsicons::bs_icon(nom, size = taille, a11y = "deco")

carte_heart <- function(titre, ..., icone = "bar-chart", plein_ecran = TRUE) {
  bslib::card(
    bslib::card_header(shiny::div(class = "heart-card-title", icone_heart(icone), shiny::span(titre))),
    bslib::card_body(..., fill = FALSE), full_screen = plein_ecran, fill = FALSE
  )
}

entete_heart <- function(numero, titre, texte) shiny::div(class = "page-heading",
  shiny::div(class = "eyebrow", numero), shiny::h1(titre), shiny::p(texte))

theme_application_heart <- function() bslib::bs_theme(version = 5,
  bg = palette_heart$bg, fg = palette_heart$text, primary = palette_heart$red,
  secondary = palette_heart$muted, success = palette_heart$teal,
  info = palette_heart$blue, warning = palette_heart$amber, danger = palette_heart$red,
  base_font = c("Inter", "Segoe UI", "Arial", "sans-serif"),
  heading_font = c("Inter", "Segoe UI", "Arial", "sans-serif"),
  "body-secondary-color" = palette_heart$muted,
  "border-color" = palette_heart$line, "card-bg" = palette_heart$card,
  "card-border-color" = palette_heart$line, "border-radius" = ".75rem")
