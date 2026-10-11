source("R/acm_imputation.R", local = TRUE)
source("R/graphiques.R", local = TRUE)

# Parcours exploratoires communs aux cas complets et aux données imputées.
preparer_parcours <- function(donnees, colonnes, categories, imputation = FALSE,
                              ncp = 2L, methode = "AFDM") {
  predicteurs <- setdiff(colonnes, "diagnostic")
  masque <- is.na(donnees[predicteurs])
  if (!imputation) {
    garder <- complete.cases(donnees[colonnes])
    return(list(donnees = donnees[garder, , drop = FALSE],
      masque = masque[garder, , drop = FALSE], n_exclus = sum(!garder), ncp = NULL))
  }
  if (!requireNamespace("missMDA", quietly = TRUE))
    stop('Installer missMDA : install.packages("missMDA").')
  # Le diagnostic et le centre ne participent pas à l'imputation.
  garder <- !is.na(donnees$diagnostic)
  d <- donnees[garder, , drop = FALSE]
  x <- d[predicteurs]
  masque <- is.na(x)
  for (v in intersect(categories, predicteurs)) x[[v]] <- factor(x[[v]])
  if (any(vapply(x, function(z) all(is.na(z)), logical(1))))
    stop("Une variable entièrement manquante ne peut pas être imputée.")
  avertissements <- character()
  complet <- withCallingHandlers({
    if (methode == "AFDM") {
      missMDA::imputeFAMD(x, ncp = ncp, method = "Regularized")$completeObs
    } else {
      quanti <- setdiff(predicteurs, categories)
      quali <- intersect(predicteurs, categories)
      x[quanti] <- missMDA::imputePCA(x[quanti], ncp = ncp,
        scale = TRUE, method = "Regularized")$completeObs
      x[quali] <- missMDA::imputeMCA(x[quali], ncp = ncp,
        method = "Regularized")$completeObs
      x
    }
  }, warning = function(w) {
    avertissements <<- c(avertissements, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  for (v in predicteurs) {
    # missMDA préfixe parfois les modalités par le nom de la variable.
    valeurs <- if (v %in% categories)
      as.numeric(sub("^.*_", "", as.character(complet[[v]]))) else complet[[v]]
    d[[v]][masque[, v]] <- valeurs[masque[, v]]
  }
  if (anyNA(d[colonnes])) stop("L'imputation laisse des valeurs manquantes.")
  list(donnees = d, masque = masque, n_exclus = sum(!garder), ncp = ncp,
       avertissements = unique(avertissements), methode = methode)
}

parcours_ui <- function(id, imputation = FALSE) {
  ns <- NS(id)
  tagList(
    if (imputation) tagList(
      p("Imputation factorielle régularisée des mesures cliniques. Le diagnostic et la provenance ne servent pas à reconstruire les valeurs manquantes."),
      selectInput(ns("methode"), "Méthode d'imputation",
        choices = c("AFDM : extension de l'ACP aux données mixtes" = "AFDM",
                    "ACP des mesures numériques + ACM des catégories" = "ACP")),
      sliderInput(ns("ncp"), "Dimensions retenues pour l'imputation", min = 1, max = 4, value = 2, step = 1),
      p(class = "note", "Deux dimensions constituent le réglage initial, sans sélection automatique. Comparer plusieurs valeurs permet d'examiner la sensibilité des relations à l'imputation. Les valeurs observées sont conservées ; seules les cellules manquantes sont remplacées.")) else
      p("Suppression des observations comportant au moins un NA sur les 14 variables cliniques. Aucune variable n'est supprimée et aucune valeur n'est imputée."),
    div(class = "callout", textOutput(ns("resume"))),
    h4("1 · Composition de l'échantillon par centre"),
    plotOutput(ns("centres"), height = "300px"),
    if (imputation) tagList(h4("Valeurs reconstruites par variable"),
      plotOutput(ns("imputations"), height = "380px"),
      p(class = "note", "Une forte proportion de valeurs reconstruites rend l'interprétation de la variable plus dépendante de la méthode d'imputation.")),
    h4("2 · Mesures cliniques selon le diagnostic"),
    plotOutput(ns("distributions"), height = "520px"),
    p(class = "note", "La ligne centrale est la médiane, la boîte couvre les 50 % centraux et les points isolés indiquent les valeurs au-delà des moustaches. Chaque mesure possède sa propre échelle."),
    h4("3 · Modalités cliniques selon le diagnostic"),
    radioButtons(ns("categories_vue"), "Lecture des proportions", inline = TRUE,
      choices = c("Diagnostics positifs par modalité" = "positifs",
                  "Modalités dans chaque groupe de diagnostic" = "distribution"), selected = "positifs"),
    conditionalPanel("input.categories_vue == 'distribution'", ns = ns,
      plotOutput(ns("categories"), height = "620px"),
      p(class = "note", "Chaque barre représente 100 % des observations d'un groupe de diagnostic. Les modalités suivent les codes UCI.")),
    conditionalPanel("input.categories_vue == 'positifs'", ns = ns,
      plotly::plotlyOutput(ns("categories_positifs"), height = "1400px"),
      p(class = "note", "Chaque point indique la part de diagnostics positifs parmi les observations de cette modalité. Le trait relie ce taux à la moyenne du parcours (pointillés). Orange : au-dessus de la moyenne ; bleu : en dessous. Survoler un point affiche le nombre de diagnostics positifs et le dénominateur. Cette comparaison est descriptive.")),
    h4("4 · Corrélations entre mesures numériques"),
    plotOutput(ns("correlations"), height = "480px"),
    p(class = "note", "Corrélations de Spearman : −1 indique une relation décroissante, +1 une relation croissante. Une corrélation ne démontre pas de causalité."),
    h4("5 · Relations entre deux mesures"),
    fluidRow(column(4, selectInput(ns("x"), "Mesure horizontale", choices = NULL)),
      column(4, selectInput(ns("y"), "Mesure verticale", choices = NULL)),
      column(4, selectInput(ns("couleur"), "Couleur des observations",
        choices = c("Diagnostic" = "diagnostic", "Centre" = "provenance")))),
    plotly::plotlyOutput(ns("relations"), height = "440px"),
    p(class = "note", "Survoler un point affiche les deux mesures, le diagnostic, le centre et leur statut observé ou imputé. Cliquer dans la légende masque ou réaffiche un groupe."),
    h4("6 · ACM des variables catégorielles"),
    p("Ce bloc autonome repart des données de référence nettoyées. Son mode de gestion des NA est indépendant des autres graphiques du parcours ; son effectif peut donc différer."),
    fluidRow(
      column(5, selectInput(ns("acm_mode"), "Gestion des NA pour cette ACM",
        choices = acm_modes, selected = if (imputation) "simple" else "complets")),
      column(4, selectInput(ns("acm_nboot"), "Nombre d'imputations multiples",
        choices = c("20 · développement rapide" = 20, "50" = 50, "100 · analyse finale" = 100),
        selected = 100)),
      column(3, actionButton(ns("acm_calcul"), "Calculer l’ACM", class = "btn-primary"))),
    checkboxInput(ns("acm_lourdes"), "Inclure le nombre de vaisseaux et le test au thallium", FALSE),
    textOutput(ns("acm_taux_na")),
    p(class = "note", "Modifier les réglages puis cliquer sur Calculer l’ACM. Les résultats déjà calculés sont réutilisés."),
    div(class = "callout", textOutput(ns("acm_resume"))),
    p("Le diagnostic ne participe jamais à l’imputation ni à la construction des axes : il est projeté en supplémentaire. La provenance est également supplémentaire ; elle peut aider à imputer et influence donc indirectement l’analyse via l’imputation."),
    plotOutput(ns("acm_individus"), height = "440px"),
    plotOutput(ns("acm_modalites"), height = "620px"),
    p(class = "note", "Les croix signalent les observations comportant une valeur imputée. Le graphe des individus représente le jeu de référence, pas leur variabilité entre imputations multiples."),
    p(class = "note", "Les ellipses de variabilité liée aux imputations multiples décrivent la sensibilité des positions des modalités : petite ellipse, faible sensibilité ; grande ellipse, forte dépendance aux valeurs imputées. Ce ne sont pas des intervalles de confiance du centre."),
    p(class = "note", "Les points représentent les modalités de l’ACM de référence ; leur taille indique la qualité de représentation sur le plan (cos²). Lire les proximités avec cette qualité et l’inertie affichée. Les plans calculés séparément ne sont pas directement alignés.")
  )
}

parcours_serveur <- function(id, reference, colonnes, categories, libelles, sources,
                             imputation = FALSE) {
  moduleServer(id, function(input, output, session) {
    quanti <- setdiff(colonnes, c(categories, "diagnostic"))
    choix <- setNames(quanti, libelles[quanti])
    updateSelectInput(session, "x", choices = choix, selected = "age")
    updateSelectInput(session, "y", choices = choix, selected = "fc_max")
    parcours <- reactive({
      resultat <- tryCatch(preparer_parcours(reference, colonnes, categories,
        imputation, if (imputation) input$ncp %||% 2L else 2L,
        if (imputation) input$methode %||% "AFDM" else "AFDM"),
        error = function(e) list(erreur = conditionMessage(e)))
      validate(need(is.null(resultat$erreur), resultat$erreur))
      resultat
    })
    donnees <- reactive({
      d <- parcours()$donnees
      validate(need(nrow(d) >= 3, "Effectif insuffisant pour ce parcours."))
      d$groupe <- factor(d$diagnostic, levels = 0:1, labels = c("Absence", "Présence"))
      d
    })
    palette <- couleurs_diagnostic
    output$resume <- renderText({
      p <- parcours()
      paste(nrow(p$donnees), "observations analysées sur", nrow(reference), "—",
        p$n_exclus, "observations exclues.",
        if (imputation) paste(sum(p$masque), "cellules imputées avec", p$ncp,
          "dimensions ; méthode", p$methode, ".") else "Cas complets uniquement.",
        if (length(p$avertissements)) paste("Avertissement de calcul :", paste(p$avertissements, collapse = " ; ")))
    })
    output$centres <- renderPlot({
      d <- donnees()
      b <- as.data.frame(table(factor(d$provenance, levels = names(sources)), d$groupe))
      names(b) <- c("centre", "diagnostic", "n")
      ggplot(b, aes(centre, n, fill = diagnostic)) + geom_col(width = .65) +
        geom_text(aes(label = ifelse(n > 0, n, "")), position = position_stack(vjust = .5), color = "white") +
        scale_fill_manual(values = palette) + scale_x_discrete(labels = sources) +
        labs(x = NULL, y = "Observations", fill = "Diagnostic") +
        theme_minimal(base_size = 12) + theme(legend.position = "top")
    }, res = 110)
    output$imputations <- renderPlot({
      p <- parcours()
      b <- data.frame(variable = colnames(p$masque), pct = 100 * colMeans(p$masque))
      ggplot(b, aes(pct, reorder(variable, pct))) + geom_col(fill = "#E88432") +
        geom_text(aes(label = sprintf("%.1f %%", pct)), hjust = -.1) +
        scale_y_discrete(labels = libelles) + scale_x_continuous(limits = c(0, 110), breaks = seq(0,100,25)) +
        labs(x = "Pourcentage de valeurs imputées", y = NULL) + theme_minimal(base_size = 12)
    }, res = 110)
    output$distributions <- renderPlot({
      d <- donnees()
      b <- do.call(rbind, lapply(quanti, function(v)
        data.frame(variable = unname(libelles[v]), valeur = d[[v]], diagnostic = d$groupe)))
      ggplot(b, aes(diagnostic, valeur, fill = diagnostic)) +
        geom_boxplot(width = .6, outlier.alpha = .3, show.legend = FALSE) +
        facet_wrap(~variable, scales = "free_y", ncol = 3) +
        scale_fill_manual(values = palette) + labs(x = "Diagnostic", y = NULL) + theme_minimal(base_size = 11)
    }, res = 110)
    output$categories <- renderPlot({
      d <- donnees()
      b <- do.call(rbind, lapply(categories, function(v) {
        z <- as.data.frame(table(d$groupe, factor(d[[v]])))
        names(z) <- c("diagnostic", "modalite", "n")
        z$variable <- libelles[v]
        z
      }))
      ggplot(b, aes(diagnostic, n, fill = modalite)) + geom_col(position = "fill", width = .6) +
        facet_wrap(~variable, ncol = 3) +
        scale_y_continuous(labels = function(x) paste0(100*x, " %")) +
        scale_fill_brewer(palette = "Set2") + labs(x = "Diagnostic", y = "Proportion", fill = "Code UCI") +
        theme_minimal(base_size = 11) + theme(legend.position = "bottom")
    }, res = 110)
    output$correlations <- renderPlot({
      d <- donnees()
      m <- cor(d[quanti], method = "spearman")
      b <- as.data.frame(as.table(m)); names(b) <- c("x", "y", "rho")
      ggplot(b, aes(x, y, fill = rho)) + geom_tile(color = "white") +
        geom_text(aes(label = sprintf("%.2f", rho)), size = 4) +
        scale_fill_gradient2(low = "#E88432", mid = "white", high = "#165DDE", limits = c(-1,1)) +
        scale_x_discrete(labels = libelles) + scale_y_discrete(labels = libelles) +
        coord_equal() + labs(x = NULL, y = NULL, fill = "Spearman") +
        theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 25, hjust = 1), panel.grid = element_blank())
    }, res = 110)
    parts_diagnostic <- reactive(calculer_parts_diagnostic(donnees(), categories, libelles))
    output$categories_positifs <- plotly::renderPlotly({
      d <- parts_diagnostic()
      validate(need(nrow(d) > 0, "Aucune modalité avec un diagnostic renseigné."))
      graphe_parts_diagnostic(d, 100 * mean(donnees()$diagnostic == 1, na.rm = TRUE))
    })
    output$relations <- plotly::renderPlotly({
      req(input$x %in% quanti, input$y %in% quanti, input$couleur %in% c("diagnostic", "provenance"))
      d <- donnees()
      masque <- parcours()$masque
      d$statut_x <- ifelse(masque[, input$x], "Imputée", "Observée")
      d$statut_y <- ifelse(masque[, input$y], "Imputée", "Observée")
      d$reconstruit <- factor(ifelse(masque[, input$x] | masque[, input$y],
        "Au moins une imputée", "Observées"), levels = c("Observées", "Au moins une imputée"))
      d$couleur <- if (input$couleur == "diagnostic") d$groupe else
        factor(d$provenance, levels = names(sources), labels = unname(sources))
      d$survol <- paste0("<b>", htmltools::htmlEscape(libelles[input$x]), "</b> : ",
        signif(d[[input$x]], 5), " (", d$statut_x, ")<br><b>",
        htmltools::htmlEscape(libelles[input$y]), "</b> : ", signif(d[[input$y]], 5),
        " (", d$statut_y, ")<br>Diagnostic : ", d$groupe,
        "<br>Centre : ", htmltools::htmlEscape(unname(sources[d$provenance])))
      couleurs <- if (input$couleur == "diagnostic") palette else
        setNames(unname(couleurs_centres[names(sources)]), unname(sources))
      p <- plotly::plot_ly(d, x = d[[input$x]], y = d[[input$y]],
        color = ~couleur, colors = couleurs, symbol = ~reconstruit, symbols = c("circle", "x"),
        type = "scatter", mode = "markers", text = ~survol, hoverinfo = "text",
        marker = list(size = 7, opacity = .55)) |>
        plotly::layout(xaxis = list(title = libelles[[input$x]], gridcolor = "#E2EBF6"),
                       yaxis = list(title = libelles[[input$y]], gridcolor = "#E2EBF6"))
      habiller_interactif(p)
    })
    output$acm_taux_na <- renderText({
      v <- intersect(acm_variables_lourdes, names(reference))
      paste("Taux de NA dans la référence :", paste(sprintf("%s : %.2f %%",
        libelles[v], 100 * vapply(reference[v], function(z) mean(is.na(z)), numeric(1))), collapse = " ; "),
        ". Ces deux variables sont exclues par défaut de cette ACM.")
    })
    reglages_acm <- eventReactive(input$acm_calcul, {
      list(mode = input$acm_mode %||% if (imputation) "simple" else "complets",
           avec_lourdes = isTRUE(input$acm_lourdes),
           nboot = as.integer(input$acm_nboot %||% 100L))
    }, ignoreNULL = FALSE)
    acm <- reactive({
      r <- reglages_acm()
      resultat <- tryCatch(withProgress(message = "Calcul de l’ACM…", {
        acm_resultat_cache(reference, categories, libelles,
                          r$mode, r$avec_lourdes, r$nboot)
      }), error = function(e) list(erreur = conditionMessage(e)))
      validate(need(is.null(resultat$erreur), resultat$erreur))
      resultat
    })
    output$acm_resume <- renderText(acm_texte_resultat(acm(), sources))
    output$acm_individus <- renderPlot({
      acm_graphe_individus(acm(), palette)
    }, res = 110)
    output$acm_modalites <- renderPlot({
      acm_graphe_modalites(acm(), libelles, palette)
    }, res = 110)
  })
}

# Valeurs initiales avant réception des contrôles Shiny.
`%||%` <- function(x, y) if (is.null(x)) y else x
