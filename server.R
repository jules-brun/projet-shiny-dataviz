# Serveur Shiny — dépend de R/import.R et des quatre fichiers processed.
# Packages à installer une fois : install.packages(c("shiny", "tidyverse", "VIM", "FactoMineR", "missMDA", "ggrepel"))
library(shiny)
library(ggplot2)
library(tidyverse)
source("R/design.R", local = TRUE)
source("R/overview.R", local = TRUE)

# Chargement une fois au démarrage, dans un environnement séparé.
# import.R peut imprimer un graphique : un périphérique temporaire sans fichier
# évite de créer Rplots.pdf. Les graphiques affichés dans Shiny sont rendus plus bas.
charger_import <- function() {
  if (!file.exists("R/import.R")) stop("R/import.R introuvable à la racine de l'application.")
  env <- new.env(parent = globalenv())
  grDevices::pdf(file = NULL)
  appareil <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(appareil), add = TRUE)
  sys.source("R/import.R", envir = env)
  env
}
import_uci <- charger_import()

# Copie pour l'application : préserver les objets produits par import.R.
donnees_uci <- import_uci$heart
if (!"diagnostic_initial" %in% names(donnees_uci)) {
  donnees_uci$diagnostic_initial <- donnees_uci$diagnostic
}
stopifnot(all(is.na(donnees_uci$diagnostic_initial) |
                donnees_uci$diagnostic_initial %in% 0:4))
donnees_uci$diagnostic <- ifelse(is.na(donnees_uci$diagnostic_initial),
                                NA_integer_, as.integer(donnees_uci$diagnostic_initial > 0))

variables_categorielles <- c("sexe", "type_doul_thor", "glyc_jeun_elevee",
  "ecg_repos", "angine_effort", "pente_st", "test_thallium", "nb_vaisseaux")
libelles_sources <- c(cleveland = "Cleveland", hungarian = "Hongrie",
                       switzerland = "Suisse", va = "VA Long Beach")

# Référence propre au quatrième onglet, avec un identifiant de ligne source stable.
source("R/gestion_na.R", local = TRUE)
source("R/visualisation.R", local = TRUE)
dictionnaire_na <- creer_dictionnaire_na(import_uci$dictionnaire, variables_categorielles)
reference_na <- new.env(parent = emptyenv())
reference_na$donnees <- donnees_uci
reference_na$donnees$id_ligne <- with(import_uci$details[!import_uci$retirer, ],
  paste(provenance, ligne_source, sep = ":"))
lockBinding("donnees", reference_na)
source("R/tests_absence.R", local = TRUE)
# Un seul balayage sur la référence nettoyée, partagé entre les sessions.
associations_absence <- analyser_absences(reference_na$donnees, dictionnaire_na)

function(input, output, session) {
  output$bilan_doublons <- renderText({
    b <- import_uci$bilan_nettoyage
    paste(b$n_doublons_retires, "occurrences retirées sur", b$n_brut,
          "lignes initiales ;", b$n_final,
          "observations conservées. Une ligne par paire est gardée. Les fichiers sources sont inchangés.")
  })
  output$recodages_na <- renderUI({
    journal <- import_uci$journal_recodage
    nb_interrogations <- sum(is.na(import_uci$heart_avant_recodage[import_uci$colonnes]))
    nb_zeros <- function(variable)
      sum(journal$n_zeros_recodes[journal$variable == variable])
    fluidRow(
      column(6, div(class = "recodage",
        h4("1 · Marqueurs manquants UCI"),
        p(class = "regle", "? → NA"),
        p("Les points d'interrogation des fichiers sources indiquent une valeur manquante. Ils sont convertis en NA dès l'import [1]."),
        p(strong(nb_interrogations), "valeurs manquantes d'origine dans les observations conservées après dédoublonnage."))),
      column(6, div(class = "recodage",
        h4("2 · Zéros de mesures non exploitables"),
        p(class = "regle", "pa_repos et cholesterol : 0 → NA"),
        p("Le recodage de ces deux mesures nulles repose sur leur plausibilité clinique, éclairée par les références bibliographiques [2, 3]."),
        p(strong(nb_zeros("pa_repos")), "valeur de pression au repos et",
          strong(nb_zeros("cholesterol")), "valeurs de cholestérol recodées en NA.")))
    )
  })
  # Les données sources restent communes en lecture ; les sorties sont par session.
  updateSelectInput(session, "source_apercu", choices = c(
    "Toutes" = "toutes", setNames(names(import_uci$fichiers), names(import_uci$fichiers))
  ))
  apercu <- reactive({
    req(input$source_apercu)
    if (input$source_apercu == "toutes") donnees_uci else
      donnees_uci[donnees_uci$provenance == input$source_apercu, ]
  })
  output$dimensions <- renderText({
    paste(nrow(apercu()), "observations —", ncol(apercu()),
          "variables disponibles (diagnostic original inclus).")
  })
  updateSelectInput(session, "variable_apercu",
    choices = setNames(import_uci$colonnes, import_uci$dictionnaire$libelle),
    selected = "age")
  output$apercu <- renderPlot({
    req(input$variable_apercu %in% import_uci$colonnes)
    variable <- input$variable_apercu
    d <- data.frame(valeur = apercu()[[variable]])
    d <- d[!is.na(d$valeur), , drop = FALSE]
    validate(need(nrow(d) > 0, "Aucune valeur observée pour cette variable dans ce centre."))
    libelle <- import_uci$dictionnaire$libelle[match(variable, import_uci$colonnes)]
    if (variable %in% c(variables_categorielles, "diagnostic")) {
      d$valeur <- factor(d$valeur)
      if (variable == "diagnostic")
        d$valeur <- factor(d$valeur, levels = c("0", "1"),
                          labels = c("Absence", "Présence"))
      ggplot(d, aes(valeur)) + geom_bar(fill = "#78A9DF", width = .65) +
        labs(x = libelle, y = "Observations", caption = "Valeurs manquantes exclues. Les catégories cliniques suivent les codes UCI.") +
        theme_heart(base_size = 12)
    } else {
      ggplot(d, aes(valeur)) + geom_histogram(bins = 25, fill = "#78A9DF", color = "white") +
        labs(x = libelle, y = "Observations", caption = "Valeurs manquantes exclues.") +
        theme_heart(base_size = 12)
    }
  }, res = 110)
  output$effectifs <- renderPlot({
    d <- as.data.frame(table(factor(apercu()$provenance, levels = names(libelles_sources))))
    names(d) <- c("provenance", "n")
    ggplot(d, aes(provenance, n)) + geom_col(fill = "#78A9DF", width = .6) +
      geom_text(aes(label = n), vjust = -.4, colour = palette_heart$text) +
      scale_x_discrete(labels = libelles_sources) +
      scale_y_continuous(expand = expansion(mult = c(0, .15))) +
      labs(x = NULL, y = "Observations après nettoyage") + theme_heart(base_size = 12)
  }, res = 110)
  output$dictionnaire <- renderUI({
    req(input$variable_apercu %in% import_uci$colonnes)
    d <- import_uci$dictionnaire
    i <- match(input$variable_apercu, d$nom_fr)
    div(class = "callout", strong(d$libelle[i]),
      p("Nom de la variable : ", code(d$nom_fr[i]), " · Code UCI : ", code(d$nom_uci[i])),
      if (input$variable_apercu == "diagnostic")
        p("Diagnostic binaire : 0 = absence, 1 = présence. Le code UCI original (0 à 4) est conservé dans les données."))
  })
  output$doublons <- renderUI({
    d <- import_uci$doublons_details
    groupes <- split(d, interaction(d$provenance, d$groupe_profil, drop = TRUE))
    tagList(lapply(groupes, function(g) {
      retires <- g$ligne_source[import_uci$retirer[g$ligne_fusion]]
      gardees <- g$ligne_source[!import_uci$retirer[g$ligne_fusion]]
      div(class = "callout", strong(unname(libelles_sources[g$provenance[1]])),
        p("Ligne source conservée : ", paste(gardees, collapse = ", "),
          " → ligne source retirée : ", paste(retires, collapse = ", ")))
    }))
  })
  output$combinaisons_na <- renderPlot({
    validate(need(requireNamespace("VIM", quietly = TRUE),
      'Installer VIM pour afficher ce graphique : install.packages("VIM")'))
    ancien_par <- par(no.readonly = TRUE)
    on.exit(par(ancien_par))
    par(las = 2, bg = palette_heart$card, fg = palette_heart$text,
        col.axis = palette_heart$muted, col.lab = palette_heart$muted, col.main = palette_heart$text)
    VIM::aggr(donnees_uci[import_uci$colonnes],
      col = c(palette_heart$blue, palette_heart$amber),
      only.miss = TRUE, sortVars = TRUE, sortCombs = TRUE,
      numbers = FALSE, prop = TRUE, cex.axis = .75,
      ylabs = c("Proportion de NA", "Fréquence des combinaisons"))
  }, res = 110)
  output$na_global_ui <- renderPlot({
    d <- data.frame(nb_na = rowSums(is.na(donnees_uci[import_uci$colonnes])))
    ggplot(d, aes(nb_na)) + geom_bar(fill = "#78A9DF", width = .7) +
      scale_x_continuous(breaks = 0:length(import_uci$colonnes)) +
      labs(x = "Nombre de mesures manquantes sur 14", y = "Observations",
           caption = "Toutes les observations nettoyées sont incluses, y compris celles sans NA.") +
      theme_heart(base_size = 12)
  }, res = 110)
  output$na_detail <- renderPlot({ import_uci$graphique_na_provenance() }, res = 110)

  libelles <- setNames(import_uci$dictionnaire$libelle, import_uci$colonnes)
  parcours_serveur("complets", donnees_uci, import_uci$colonnes,
    variables_categorielles, libelles, libelles_sources)
  parcours_serveur("imputes", donnees_uci, import_uci$colonnes,
    variables_categorielles, libelles, libelles_sources, imputation = TRUE)
  serveur_tests_absence(input, output, session, associations_absence,
                        dictionnaire_na, libelles_sources)
  overview_serveur(input, output, session, donnees_uci, import_uci, libelles_sources)

}
