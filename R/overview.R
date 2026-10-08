# Accueil : synthèses dérivées des objets existants, sans nouveau traitement.
overview_ui <- function() {
  shiny::tagList(
    shiny::div(class = "heart-hero",
      shiny::div(class = "hero-copy",
        shiny::div(class = "eyebrow", shiny::span(class = "status-dot"), "UCI HEART DISEASE · EXPLORATION CLINIQUE"),
        shiny::h1("Diagnostic cardiaque", shiny::tags$br(), shiny::span("et prédiction.")),
        shiny::p("Des données cliniques aux relations entre variables. Explorer une cohorte de quatre centres, comprendre les absences et examiner l'influence des choix d'analyse."),
        shiny::div(class = "hero-actions",
          shiny::actionButton("overview_donnees", "Explorer les données", icon = icone_heart("arrow-right"), class = "btn-primary"),
          shiny::actionLink("overview_na", "Comprendre les valeurs manquantes", icon = icone_heart("arrow-up-right"))
        ),
        shiny::div(class = "hero-tags", shiny::span("13 mesures cliniques"), shiny::span("4 centres"), shiny::span("Données UCI · 1989"))
      ),
      shiny::div(class = "hero-signal", `aria-hidden` = "true",
        shiny::div(class = "signal-label", icone_heart("heart-pulse", "1.6em"), shiny::span("HEART DISEASE")),
        shiny::HTML('<svg viewBox="0 0 440 190" role="presentation" focusable="false"><defs><pattern id="ecg-grid" width="22" height="22" patternUnits="userSpaceOnUse"><path d="M 22 0 L 0 0 0 22" fill="none" stroke="#26364C" stroke-width=".5"/></pattern></defs><rect width="440" height="190" fill="url(#ecg-grid)"/><path d="M0 106H72L87 90L103 106H139L153 125L173 40L195 156L215 85L230 106H267L286 96L302 106H440" fill="none" stroke="#EF6473" stroke-width="2.7" stroke-linejoin="round" stroke-linecap="round"/></svg>'),
        shiny::div(class = "signal-foot", "Quatre centres. Une lecture critique.")
      )
    ),
    shiny::uiOutput("overview_indicateurs"),
    bslib::layout_columns(
      carte_heart("Une cohorte, quatre provenances", shiny::p(class = "note", "Effectifs nettoyés et disponibilité des cas complets."),
        shiny::plotOutput("overview_centres", height = "300px"), icone = "geo-alt"),
      carte_heart("Le diagnostic dans la cohorte", shiny::p(class = "note", "La cible binaire utilisée dans les analyses."),
        shiny::plotOutput("overview_diagnostic", height = "300px"), icone = "heart-pulse"),
      col_widths = bslib::breakpoints(xs = 12, lg = c(7, 5)), fill = FALSE
    ),
    carte_heart("Des données aux décisions d'analyse", shiny::uiOutput("overview_chemin"),
      icone = "diagram-3", plein_ecran = FALSE),
    shiny::p(class = "overview-source", "Source : Heart Disease, UCI Machine Learning Repository. Cleveland, Hongrie, Suisse et VA Long Beach. ",
      shiny::tags$a("Consulter la documentation UCI", href = "https://doi.org/10.24432/C52P4X", target = "_blank", rel = "noopener noreferrer"))
  )
}

overview_serveur <- function(input, output, session, donnees, import, libelles_sources) {
  colonnes <- import$colonnes
  complets <- complete.cases(donnees[colonnes])
  output$overview_indicateurs <- shiny::renderUI({
    boite <- function(titre, valeur, note, icone, accent = FALSE) bslib::value_box(
      titre, valeur, shiny::p(note), showcase = icone_heart(icone, "1.8em"),
      showcase_layout = bslib::showcase_top_right(width = .22, max_height = "48px"),
      theme = bslib::value_box_theme(bg = palette_heart$card, fg = palette_heart$text),
      class = if (accent) "heart-metric heart-metric-accent" else "heart-metric", fill = FALSE)
    bslib::layout_columns(
      boite("Observations nettoyées", nrow(donnees), "Après dédoublonnage", "database"),
      boite("Centres de provenance", length(import$fichiers), "Une cohorte multicentrique", "geo-alt"),
      boite("Variables explicatives", length(setdiff(colonnes, "diagnostic")), "Mesures et catégories cliniques", "activity"),
      boite("Cas complets", sum(complets), sprintf("%.1f %% de la cohorte, sans imputation", 100 * mean(complets)), "clipboard2-pulse", TRUE),
      col_widths = bslib::breakpoints(xs = 12, sm = 6, lg = 3), gap = "18px", fill = FALSE)
  })
  output$overview_centres <- shiny::renderPlot({
    sources <- names(libelles_sources)
    n <- as.integer(table(factor(donnees$provenance, levels = sources)))
    cc <- as.integer(table(factor(donnees$provenance[complets], levels = sources)))
    b <- rbind(data.frame(centre = sources, statut = "Cas complets", n = cc),
      data.frame(centre = sources, statut = "Au moins un NA", n = n - cc))
    b$centre <- factor(b$centre, levels = rev(sources))
    b$statut <- factor(b$statut, levels = c("Cas complets", "Au moins un NA"))
    total <- data.frame(centre = factor(sources, levels = rev(sources)), n = n)
    ggplot2::ggplot(b, ggplot2::aes(n, centre, fill = statut)) +
      ggplot2::geom_col(width = .46, position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::geom_text(data = total, ggplot2::aes(n, centre, label = n), inherit.aes = FALSE,
        hjust = -.4, colour = palette_heart$text, size = 3.5) +
      ggplot2::scale_fill_manual(values = c("Cas complets" = palette_heart$blue, "Au moins un NA" = "#31465F")) +
      ggplot2::scale_y_discrete(labels = libelles_sources) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .13))) +
      ggplot2::labs(x = "Observations", y = NULL, fill = NULL) + theme_heart(11) +
      ggplot2::theme(legend.position = "bottom", panel.grid.major.y = ggplot2::element_blank())
  }, res = 110)
  output$overview_diagnostic <- shiny::renderPlot({
    b <- as.data.frame(table(factor(donnees$diagnostic, levels = 0:1,
      labels = c("Absence", "Présence"))))
    names(b) <- c("diagnostic", "n")
    b$pct <- 100 * b$n / sum(b$n)
    ggplot2::ggplot(b, ggplot2::aes(diagnostic, n, fill = diagnostic)) +
      ggplot2::geom_col(width = .38, show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%d · %.1f %%", n, pct)), vjust = -.6,
        colour = palette_heart$text, size = 4) +
      ggplot2::scale_fill_manual(values = c("Absence" = palette_heart$blue, "Présence" = palette_heart$red)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .18))) +
      ggplot2::labs(x = NULL, y = "Observations", caption = "0 = absence, 1 = présence selon le critère UCI.\nLe code 0 n'exclut pas tout problème cardiaque.") + theme_heart(11) +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  }, res = 110)
  output$overview_chemin <- shiny::renderUI({
    b <- import$bilan_nettoyage
    etape <- function(numero, titre, texte) shiny::div(class = "workflow-step",
      shiny::span(class = "workflow-number", numero), shiny::div(shiny::strong(titre), shiny::p(texte)))
    bslib::layout_columns(
      etape("01", "Documenter la cohorte", paste(b$n_brut, "lignes sources ;", b$n_doublons_retires, "occurrences dupliquées retirées.")),
      etape("02", "Comprendre les manques", paste(b$n_zeros_recodes, "zéros recodés en NA selon les règles de nettoyage existantes.")),
      etape("03", "Comparer les relations", "Cas complets et imputation factorielle : deux parcours d'exploration à examiner séparément."),
      col_widths = bslib::breakpoints(xs = 12, lg = 4), fill = FALSE)
  })
  shiny::observeEvent(input$overview_donnees, bslib::nav_select("navigation_heart", "donnees", session = session))
  shiny::observeEvent(input$overview_na, bslib::nav_select("navigation_heart", "manquantes", session = session))
}
