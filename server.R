# Serveur Shiny — dépend de R/import.R et des quatre fichiers processed.
# Packages à installer une fois : install.packages(c("shiny", "tidyverse", "VIM", "FactoMineR", "missMDA", "ggrepel", "plotly"))
library(shiny)
library(ggplot2)
library(tidyverse)

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
source("R/graphiques.R", local = TRUE)
source("R/visualisation.R", local = TRUE)
source("R/premieres_visus.R", local = TRUE)
source("R/acm_imputation.R", local = TRUE)
source("R/presentation.R", local = TRUE)
libelles <- setNames(import_uci$dictionnaire$libelle, import_uci$colonnes)
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
  # Filtres de sexe et d'âge communs à l'onglet ; la provenance ne filtre que les analyses.
  filtre_sexe_age <- reactive({
    d <- donnees_uci
    sexe <- input$sexe_apercu %||% "tous"
    if (sexe != "tous") d <- d[d$sexe %in% as.numeric(sexe), ]
    age <- input$age_apercu %||% range(donnees_uci$age)
    d[d$age >= age[1] & d$age <= age[2], ]
  })
  apercu <- reactive({
    req(input$source_apercu)
    d <- filtre_sexe_age()
    if (input$source_apercu == "toutes") d else d[d$provenance == input$source_apercu, ]
  })
  output$dimensions <- renderText({
    paste(nrow(apercu()), "patients dans la sélection, sur", nrow(donnees_uci), "au total.")
  })
  updateSelectInput(session, "variable_apercu",
    choices = setNames(import_uci$colonnes, import_uci$dictionnaire$libelle),
    selected = "age")
  # Paramètres communs au graphe et au texte de la question choisie (R/premieres_visus.R).
  choix_apercu <- reactive({
    req(input$question_apercu %in% questions_apercu)
    list(question = input$question_apercu,
         var_quanti = input$quanti_apercu %||% "fc_max",
         var_quali = input$quali_apercu %||% "type_doul_thor",
         variable = input$variable_apercu %||% "age")
  })
  output$apercu <- renderPlotly({
    ch <- choix_apercu()
    graphe_apercu(apercu(), ch$question, ch$var_quanti, ch$var_quali, ch$variable,
                  libelles, variables_categorielles, libelles_sources)
  })
  output$interpretation_apercu <- renderText({
    ch <- choix_apercu()
    texte_apercu(apercu(), ch$question, ch$var_quanti, ch$var_quali, ch$variable,
                 libelles, variables_categorielles, libelles_sources)
  })
  # Toujours les quatre centres : le centre filtré est mis en avant, pas isolé.
  output$effectifs <- renderPlotly({
    d <- filtre_sexe_age()
    validate(need(nrow(d) > 0, "Aucun patient dans cette sélection."))
    b <- as.data.frame(table(
      centre = factor(libelles_sources[d$provenance], levels = libelles_sources),
      diagnostic = factor(d$diagnostic, levels = 0:1, labels = names(couleurs_diagnostic))))
    b$total <- ave(b$Freq, b$centre, FUN = sum)
    choisi <- input$source_apercu %||% "toutes"
    b$opacite <- if (choisi == "toutes") 1 else ifelse(b$centre == libelles_sources[[choisi]], 1, .25)
    b$texte <- paste0("<b>", b$centre, "</b> · ", b$total, " patients<br>", b$diagnostic, " : ",
                      b$Freq, " (", pct_fr(100 * b$Freq / pmax(b$total, 1)), ")")
    totaux <- unique(b[c("centre", "total", "opacite")])
    p <- ggplot(b, aes(centre, Freq, fill = diagnostic, alpha = opacite, text = texte)) +
      geom_col(width = .55, colour = "white", linewidth = .4) +
      geom_text(data = totaux, aes(centre, total + max(total) * .06, label = total),
                inherit.aes = FALSE, colour = couleur_encre, size = 4) +
      scale_alpha_identity() + scale_fill_manual(values = couleurs_diagnostic) +
      labs(x = NULL, y = "Observations après nettoyage") + theme_app() +
      theme(panel.grid.major.x = element_blank())
    interactif(p)
  })
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
  # Une ligne par combinaison de NA (les plus fréquentes), une colonne par variable concernée.
  output$combinaisons_na <- renderPlotly({
    x <- is.na(donnees_uci[import_uci$colonnes])
    vars <- colnames(x)[colSums(x) > 0]
    vars <- vars[order(-colSums(x[, vars, drop = FALSE]))]
    motifs <- apply(x[, vars, drop = FALSE], 1, function(r) paste(as.integer(r), collapse = ""))
    motifs <- motifs[grepl("1", motifs)]
    validate(need(length(motifs) > 0, "Aucune valeur manquante."))
    frequences <- sort(table(motifs), decreasing = TRUE)
    haut <- frequences[seq_len(min(12, length(frequences)))]
    n <- nrow(x)
    lignes <- paste0("#", seq_along(haut), " · ", as.integer(haut), " obs. (", pct_fr(100 * haut / n), ")")
    b <- do.call(rbind, lapply(seq_along(haut), function(k) {
      m <- as.integer(strsplit(names(haut)[k], "")[[1]])
      data.frame(ligne = lignes[k], variable = unname(libelles[vars]),
        statut = ifelse(m == 1, "Manquante", "Observée"),
        texte = paste0("<b>Combinaison ", k, "</b> : ", as.integer(haut[k]), " observations (",
          pct_fr(100 * haut[k] / n), ")<br>Manquantes : ", paste(libelles[vars][m == 1], collapse = ", ")))
    }))
    b$ligne <- factor(b$ligne, levels = rev(lignes))
    b$variable <- factor(b$variable, levels = unname(libelles[vars]))
    titre <- paste0(length(haut), " combinaisons les plus fréquentes sur ", length(frequences),
      " : elles couvrent ", pct_fr(100 * sum(haut) / length(motifs), 0), " des observations avec au moins un NA")
    p <- ggplot(b, aes(variable, ligne, fill = statut, text = texte)) +
      geom_tile(colour = "white", linewidth = 1.5) +
      scale_fill_manual(values = c("Manquante" = "#E88432", "Observée" = "#E2EBF6")) +
      labs(x = NULL, y = NULL, title = titre) + theme_app(11) +
      theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1),
            plot.title = element_text(size = 11, colour = couleur_discrete))
    interactif(p)
  })
  output$na_global_ui <- renderPlotly({
    b <- as.data.frame(table(
      nb_na = rowSums(is.na(donnees_uci[import_uci$colonnes])),
      centre = factor(libelles_sources[donnees_uci$provenance], levels = libelles_sources)))
    b$nb_na <- as.integer(as.character(b$nb_na))
    b <- b[b$Freq > 0, ]
    b$total <- ave(b$Freq, b$nb_na, FUN = sum)
    b$texte <- paste0("<b>", b$nb_na, " mesure(s) manquante(s)</b> · ", b$total, " obs.<br>",
                      b$centre, " : ", b$Freq)
    p <- ggplot(b, aes(nb_na, Freq, fill = centre, text = texte)) +
      geom_col(width = .7, colour = "white", linewidth = .3) +
      scale_fill_manual(values = couleurs_centres) +
      scale_x_continuous(breaks = 0:length(import_uci$colonnes)) +
      labs(x = "Nombre de mesures manquantes par observation (sur 14)", y = "Observations") +
      theme_app() + theme(panel.grid.major.x = element_blank())
    interactif(p)
  })
  output$na_detail <- renderPlotly({
    d <- import_uci$na_par_provenance
    d$variable <- factor(libelles[d$variable], levels = rev(unname(libelles[import_uci$colonnes])))
    d$centre <- factor(libelles_sources[d$provenance], levels = libelles_sources)
    d$texte <- paste0("<b>", d$variable, "</b> · ", d$centre, "<br>", d$n_na, " NA sur ",
                      d$n_observations, " (", pct_fr(d$pct_na), ")")
    d$etiquette <- ifelse(d$n_na > 0, pct_fr(d$pct_na, 0), "")
    p <- ggplot(d, aes(centre, variable, fill = pct_na, text = texte)) +
      geom_tile(colour = "white", linewidth = 1.5) +
      geom_text(aes(label = etiquette, colour = pct_na >= 50), size = 3.5) +
      scale_colour_manual(values = c("FALSE" = couleur_encre, "TRUE" = "white"), guide = "none") +
      scale_fill_gradient(low = "#F1F5FB", high = "#08306B", limits = c(0, 100), name = "% de NA") +
      labs(x = NULL, y = NULL) + theme_app() + theme(panel.grid = element_blank())
    interactif(p, legende = FALSE) |> hide_colorbar()
  })

  parcours_serveur("complets", donnees_uci, import_uci$colonnes,
    variables_categorielles, libelles, libelles_sources)
  parcours_serveur("imputes", donnees_uci, import_uci$colonnes,
    variables_categorielles, libelles, libelles_sources, imputation = TRUE)
  serveur_tests_absence(input, output, session, associations_absence,
                        dictionnaire_na, libelles_sources)
  presentation_serveur(input, output, session, donnees_uci, import_uci, libelles_sources)
  acm_serveur(input, output, session, donnees_uci)

}
