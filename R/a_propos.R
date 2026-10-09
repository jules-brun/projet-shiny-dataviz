# Onglet "À propos" : présentation du projet et de l'équipe.

carte_membre <- function(nom, role) shiny::div(class = "callout",
  shiny::strong(nom), shiny::p(class = "note", role))

a_propos_ui <- function() {
  shiny::tagList(
    entete_heart("06 / À PROPOS", "Projet Data visualisation & Machine Learning",
      "M2 Science des données · Institut Agro · Année universitaire 2026–2027"),
    carte_heart("Le projet",
      shiny::p("L'objectif est d'explorer les facteurs associés aux maladies cardiaques à partir de quatre bases hospitalières, puis de construire un modèle de prédiction du diagnostic."),
      shiny::tags$ol(
        shiny::tags$li(shiny::strong("Données : "), "import des quatre centres UCI, dédoublonnage et recodage des zéros impossibles."),
        shiny::tags$li(shiny::strong("Valeurs manquantes : "), "description des absences, tests d'association et comparaison des méthodes d'imputation."),
        shiny::tags$li(shiny::strong("Visualisation : "), "premières descriptions de la cohorte et relations entre variables."),
        shiny::tags$li(shiny::strong("Machine learning : "), "comparaison de modèles de prédiction (à venir).")),
      icone = "clipboard2-pulse", plein_ecran = FALSE),
    carte_heart("L'équipe",
      bslib::layout_columns(
        carte_membre("Antonin Rivron", "Import, nettoyage et gestion des valeurs manquantes"),
        carte_membre("Jules Brun", "Machine learning"),
        carte_membre("Youri Michalowski-Skarbek", "Visualisations et interface"),
        col_widths = bslib::breakpoints(xs = 12, lg = 4), fill = FALSE),
      icone = "people", plein_ecran = FALSE),
    carte_heart("Données et outils",
      shiny::p("Janosi A., Steinbrunn W., Pfisterer M., Detrano R. (1989). Heart Disease. UCI Machine Learning Repository. ",
        shiny::tags$a("DOI 10.24432/C52P4X", href = "https://doi.org/10.24432/C52P4X",
          target = "_blank", rel = "noopener noreferrer"), " — licence CC BY 4.0."),
      shiny::p(class = "note", "Application réalisée avec R et Shiny (bslib, ggplot2, FactoMineR, missMDA, VIM)."),
      shiny::p(class = "note", "Les résultats sont exploratoires : une association observée n'est pas une causalité et l'échantillon est hospitalier."),
      icone = "journal-medical", plein_ecran = FALSE)
  )
}
