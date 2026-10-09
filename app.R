# Lanceur de l'application UCI assemblee dans ui.R et server.R.
ui <- source("ui.R", local = TRUE, encoding = "UTF-8")$value
server <- source("server.R", local = TRUE, encoding = "UTF-8")$value

shiny::shinyApp(ui = ui, server = server)
