# Outils graphiques communs : thème, palettes, libellés des modalités et passage en plotly.
library(ggplot2)
library(plotly)

couleurs_diagnostic <- c("Absence" = "#165DDE", "Présence" = "#E88432")
# Ordre fixe : une couleur reste attachée au même centre dans tous les graphiques.
couleurs_centres <- c("Cleveland" = "#165DDE", "Hongrie" = "#E88432",
                      "Suisse" = "#1E9E8F", "VA Long Beach" = "#8A5CD1")
couleur_encre <- "#143052"
couleur_discrete <- "#60758B"

# Codes UCI (heart-disease.names) traduits pour la lecture des graphiques.
modalites_uci <- list(
  sexe = c("0" = "Femme", "1" = "Homme"),
  type_doul_thor = c("1" = "Angine typique", "2" = "Angine atypique",
                     "3" = "Douleur non angineuse", "4" = "Asymptomatique"),
  glyc_jeun_elevee = c("0" = "≤ 120 mg/dL", "1" = "> 120 mg/dL"),
  ecg_repos = c("0" = "Normal", "1" = "Anomalie ST-T", "2" = "Hypertrophie VG"),
  angine_effort = c("0" = "Non", "1" = "Oui"),
  pente_st = c("1" = "Ascendante", "2" = "Plate", "3" = "Descendante"),
  nb_vaisseaux = c("0" = "0 vaisseau", "1" = "1 vaisseau", "2" = "2 vaisseaux", "3" = "3 vaisseaux"),
  test_thallium = c("3" = "Normal", "6" = "Défaut fixe", "7" = "Défaut réversible"),
  diagnostic = c("0" = "Absence", "1" = "Présence")
)

# Facteur lisible ; un code inconnu garde sa valeur brute plutôt que de devenir NA.
libeller_modalites <- function(x, variable) {
  codes <- modalites_uci[[variable]]
  x <- as.character(x)
  if (is.null(codes)) return(factor(x))
  presents <- names(codes)[names(codes) %in% x]
  autres <- sort(setdiff(unique(x[!is.na(x)]), names(codes)))
  factor(ifelse(x %in% names(codes), codes[x], x), levels = c(unname(codes[presents]), autres))
}

theme_app <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#EDF2F8", linewidth = .4),
          axis.text = element_text(colour = couleur_discrete),
          axis.title = element_text(colour = couleur_encre),
          strip.text = element_text(colour = couleur_encre, face = "bold", hjust = 0),
          legend.position = "top", legend.title = element_blank())
}

pct_fr <- function(x, chiffres = 1) paste0(formatC(x, format = "f", digits = chiffres, decimal.mark = ","), " %")

# Conversion ggplot -> plotly avec une barre d'outils réduite et des infobulles homogènes.
interactif <- function(p, tooltip = "text", legende = TRUE) {
  habiller(ggplotly(p, tooltip = tooltip), legende)
}

habiller <- function(p, legende = TRUE) {
  p |>
    config(displaylogo = FALSE, locale = "fr",
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d",
             "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")) |>
    layout(font = list(family = "Inter, system-ui, -apple-system, 'Segoe UI', sans-serif"),
           hoverlabel = list(bgcolor = "white", bordercolor = "#B8D4FA",
                             font = list(size = 13, color = couleur_encre)),
           showlegend = legende,
           # Marge haute réservée à la légende : aucun titre n'est placé dans les graphiques.
           margin = list(t = if (legende) 60 else 20),
           legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02,
                         yanchor = "bottom", title = list(text = "")))
}
