# Fenêtre "À propos", ouverte depuis la barre de navigation
ouvrir_a_propos <- function() shiny::showModal(shiny::modalDialog(
  title = "À propos", easyClose = TRUE, footer = shiny::modalButton("Fermer"),
  shiny::div(style = "background:#FFFFFF; border-radius:10px; padding:10px 16px; margin-bottom:18px; text-align:center;",
    shiny::tags$img(src = "logo_institut_agro_rennes_angers.png", alt = "L'Institut Agro Rennes-Angers",
      style = "max-width:100%; height:80px;")),
  shiny::p("Projet Data visualisation et Machine Learning"),
  shiny::p(class = "note", "M2 Science des données · Institut Agro · Année universitaire 2026–2027"),
  shiny::tags$ul(
    shiny::tags$li("Antonin Rivron"),
    shiny::tags$li("Jules Brun"),
    shiny::tags$li("Youri Michalowski-Skarbek")),
  shiny::p(class = "note", "Données : Heart Disease, UCI Machine Learning Repository (Janosi et al., 1989). ",
    shiny::tags$a("DOI 10.24432/C52P4X", href = "https://doi.org/10.24432/C52P4X",
      target = "_blank", rel = "noopener noreferrer"))))
