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

# Présentation par cartes ; les contrôles et leur espace de noms restent identiques.
parcours_ui <- function(id, imputation = FALSE) {
  ns <- NS(id)
  tagList(
    if (imputation) carte_heart("Réglages de l'imputation factorielle",
      p(class = "note", "Imputation factorielle régularisée des mesures cliniques. Le diagnostic et la provenance ne servent pas à reconstruire les valeurs manquantes."),
      bslib::layout_columns(
        selectInput(ns("methode"), "Méthode d'imputation",
          choices = c("AFDM : extension de l'ACP aux données mixtes" = "AFDM",
                      "ACP des mesures numériques + ACM des catégories" = "ACP")),
        sliderInput(ns("ncp"), "Dimensions retenues pour l'imputation", min = 1, max = 4, value = 2, step = 1),
        col_widths = bslib::breakpoints(xs = 12, lg = 6), fill = FALSE),
      p(class = "note", "Deux dimensions constituent le réglage initial, sans sélection automatique. Comparer plusieurs valeurs permet d'examiner la sensibilité des relations à l'imputation. Les valeurs observées sont conservées ; seules les cellules manquantes sont remplacées."),
      icone = "layers", plein_ecran = FALSE) else
      p(class = "note", "Suppression des observations comportant au moins un NA sur les 14 variables cliniques. Aucune variable n'est supprimée et aucune valeur n'est imputée."),
    div(class = "callout", textOutput(ns("resume"))),
    carte_heart("1 · Composition de l'échantillon par centre", plotOutput(ns("centres"), height = "300px"), icone = "geo-alt"),
    if (imputation) carte_heart("Valeurs reconstruites par variable",
      plotOutput(ns("imputations"), height = "380px"),
      p(class = "note", "Une forte proportion de valeurs reconstruites rend l'interprétation de la variable plus dépendante de la méthode d'imputation."), icone = "layers"),
    carte_heart("2 · Mesures cliniques selon le diagnostic", plotOutput(ns("distributions"), height = "520px"),
      p(class = "note", "La ligne centrale est la médiane, la boîte couvre les 50 % centraux et les points isolés indiquent les valeurs au-delà des moustaches. Chaque mesure possède sa propre échelle."), icone = "heart-pulse"),
    carte_heart("3 · Modalités cliniques selon le diagnostic", plotOutput(ns("categories"), height = "620px"),
      p(class = "note", "Chaque barre représente 100 % des observations d'un groupe de diagnostic. Les modalités suivent les codes UCI ; les proportions permettent de comparer des groupes de tailles différentes."), icone = "bar-chart"),
    carte_heart("4 · Corrélations entre mesures numériques", plotOutput(ns("correlations"), height = "480px"),
      p(class = "note", "Corrélations de Spearman : −1 indique une relation décroissante, +1 une relation croissante. Une corrélation ne démontre pas de causalité."), icone = "grid-3x3"),
    carte_heart("5 · Relations entre deux mesures",
      bslib::layout_columns(selectInput(ns("x"), "Mesure horizontale", choices = NULL),
        selectInput(ns("y"), "Mesure verticale", choices = NULL),
        selectInput(ns("couleur"), "Couleur des observations",
          choices = c("Diagnostic" = "diagnostic", "Centre" = "provenance")),
        col_widths = bslib::breakpoints(xs = 12, lg = 4), fill = FALSE),
      plotOutput(ns("relations"), height = "400px"), icone = "graph-up"),
    carte_heart("6 · ACM des variables catégorielles",
      p(class = "note", "L'ACM décrit les profils des variables catégorielles. Le diagnostic et la provenance sont supplémentaires : ils ne construisent pas les axes."),
      plotOutput(ns("acm_individus"), height = "440px"), plotOutput(ns("acm_modalites"), height = "540px"),
      p(class = "note", "Les observations proches ont des profils catégoriels similaires. Les modalités proches de l'origine sont peu discriminantes sur ce plan. Une proximité entre modalités de variables différentes s'interprète avec leur qualité de représentation ; les axes des deux parcours sont calculés séparément."),
      icone = "diagram-3"),
    if (imputation) p(class = "note", "Les résultats décrivent un jeu complété par une imputation unique. Les relations peuvent être renforcées par la reconstruction et ne mesurent pas l'incertitude d'imputation.")
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
    palette <- c("Absence" = "#78A9DF", "Présence" = "#EF6473")
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
        geom_text(aes(label = ifelse(n > 0, n, "")), position = position_stack(vjust = .5), color = palette_heart$bg) +
        scale_fill_manual(values = palette) + scale_x_discrete(labels = sources) +
        labs(x = NULL, y = "Observations", fill = "Diagnostic") +
        theme_heart(base_size = 12) + theme(legend.position = "top")
    }, res = 110)
    output$imputations <- renderPlot({
      p <- parcours()
      b <- data.frame(variable = colnames(p$masque), pct = 100 * colMeans(p$masque))
      ggplot(b, aes(pct, reorder(variable, pct))) + geom_col(fill = palette_heart$amber) +
        geom_text(aes(label = sprintf("%.1f %%", pct)), hjust = -.1, colour = palette_heart$text) +
        scale_y_discrete(labels = libelles) + scale_x_continuous(limits = c(0, 110), breaks = seq(0,100,25)) +
        labs(x = "Pourcentage de valeurs imputées", y = NULL) + theme_heart(base_size = 12)
    }, res = 110)
    output$distributions <- renderPlot({
      d <- donnees()
      b <- do.call(rbind, lapply(quanti, function(v)
        data.frame(variable = unname(libelles[v]), valeur = d[[v]], diagnostic = d$groupe)))
      ggplot(b, aes(diagnostic, valeur, fill = diagnostic)) +
        geom_boxplot(width = .6, outlier.alpha = .3, show.legend = FALSE) +
        facet_wrap(~variable, scales = "free_y", ncol = 3) +
        scale_fill_manual(values = palette) + labs(x = "Diagnostic", y = NULL) + theme_heart(base_size = 11)
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
        scale_fill_manual(values = c("#78A9DF", "#64C4B2", "#E5B36C", "#BC9ACB", "#EF6473", "#9FB6CB", "#6D809A")) + labs(x = "Diagnostic", y = "Proportion", fill = "Code UCI") +
        theme_heart(base_size = 11) + theme(legend.position = "bottom")
    }, res = 110)
    output$correlations <- renderPlot({
      d <- donnees()
      m <- cor(d[quanti], method = "spearman")
      b <- as.data.frame(as.table(m)); names(b) <- c("x", "y", "rho")
      ggplot(b, aes(x, y, fill = rho)) + geom_tile(color = "#26364C") +
        geom_text(aes(label = sprintf("%.2f", rho)), size = 4, colour = palette_heart$text) +
        scale_fill_gradient2(low = "#AB4E5C", mid = "#162438", high = "#5279A5", limits = c(-1,1)) +
        scale_x_discrete(labels = libelles) + scale_y_discrete(labels = libelles) +
        coord_equal() + labs(x = NULL, y = NULL, fill = "Spearman") +
        theme_heart(base_size = 11) + theme(axis.text.x = element_text(angle = 25, hjust = 1), panel.grid = element_blank())
    }, res = 110)
    output$relations <- renderPlot({
      req(input$x %in% quanti, input$y %in% quanti, input$couleur %in% c("diagnostic", "provenance"))
      d <- donnees()
      d$couleur <- if (input$couleur == "diagnostic") d$groupe else factor(sources[d$provenance])
      d$reconstruit <- if (imputation)
        rowSums(parcours()$masque[, c(input$x, input$y), drop = FALSE]) > 0 else FALSE
      ggplot(d, aes(.data[[input$x]], .data[[input$y]], color = couleur, shape = reconstruit)) +
        geom_point(alpha = .55, size = 2) +
        scale_color_manual(values = if (input$couleur == "diagnostic") palette else
          setNames(c("#78A9DF", "#64C4B2", "#E5B36C", "#BC9ACB"), unname(sources))) + scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 4),
          labels = c("FALSE" = "Observées", "TRUE" = "Au moins une imputée")) +
        labs(x = libelles[input$x], y = libelles[input$y], color = if (input$couleur == "diagnostic") "Diagnostic" else "Centre", shape = "Mesures") +
        theme_heart(base_size = 12) + theme(legend.position = "bottom")
    }, res = 110)
    acm <- reactive({
      validate(need(requireNamespace("FactoMineR", quietly = TRUE), 'Installer FactoMineR pour afficher l’ACM.'))
      d <- donnees()
      actives <- categories[vapply(d[categories], function(z) length(unique(z)) > 1L, logical(1))]
      validate(need(length(actives) >= 2, "Au moins deux variables catégorielles variables sont nécessaires."))
      x <- as.data.frame(lapply(actives, function(v) factor(paste0(v, " = ", d[[v]]))))
      names(x) <- actives
      x$diagnostic <- d$groupe
      x$provenance <- factor(d$provenance)
      # Un centre unique n'est pas une variable supplémentaire exploitable.
      sup <- c("diagnostic", "provenance")
      sup <- sup[vapply(x[sup], nlevels, integer(1)) > 1L]
      x <- x[c(actives, sup)]
      FactoMineR::MCA(x, quali.sup = if (length(sup)) match(sup, names(x)) else NULL,
        ncp = 2, graph = FALSE)
    })
    axes_acm <- function(a) list(x = sprintf("Axe 1 (%.1f %% d'inertie)", a$eig[1,2]),
                                 y = sprintf("Axe 2 (%.1f %% d'inertie)", a$eig[2,2]))
    output$acm_individus <- renderPlot({
      a <- acm(); d <- donnees(); axes <- axes_acm(a)
      b <- data.frame(x = a$ind$coord[,1], y = a$ind$coord[,2], diagnostic = d$groupe)
      ggplot(b, aes(x,y,color = diagnostic)) + geom_point(alpha = .4, size = 1.8) +
        geom_hline(yintercept = 0, color = "#40536C") + geom_vline(xintercept = 0, color = "#40536C") +
        scale_color_manual(values = palette) + labs(title = "ACM · Profils des observations", x = axes$x, y = axes$y, color = "Diagnostic") +
        coord_equal() + theme_heart(base_size = 12) + theme(legend.position = "bottom")
    }, res = 110)
    output$acm_modalites <- renderPlot({
      a <- acm(); axes <- axes_acm(a)
      b <- data.frame(x = a$var$coord[,1], y = a$var$coord[,2],
        modalite = rownames(a$var$coord), qualite = rowSums(a$var$cos2[,1:2]))
      b$variable <- sub(" = .*", "", b$modalite)
      ggplot(b, aes(x,y,color = variable)) +
        geom_hline(yintercept = 0, color = "#40536C") + geom_vline(xintercept = 0, color = "#40536C") +
        geom_point(aes(size = qualite)) +
        scale_color_manual(values = c("#78A9DF", "#64C4B2", "#E5B36C", "#BC9ACB", "#EF6473", "#9FB6CB", "#D4CB9E", "#7FAB9C")) +
        ggrepel::geom_text_repel(aes(label = modalite), seed = 42, max.overlaps = Inf, size = 3) +
        labs(title = "ACM · Relations entre modalités", x = axes$x, y = axes$y,
             color = "Variable", size = "Qualité sur le plan") +
        coord_equal() + theme_heart(base_size = 11) + theme(legend.position = "bottom")
    }, res = 110)
  })
}

# Valeurs initiales avant réception des contrôles Shiny.
`%||%` <- function(x, y) if (is.null(x)) y else x
