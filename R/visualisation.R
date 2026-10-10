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
      fluidRow(
        column(6, selectInput(ns("methode"), "Méthode d'imputation",
          choices = c("AFDM : extension de l'ACP aux données mixtes" = "AFDM",
                      "ACP des mesures numériques + ACM des catégories" = "ACP"))),
        column(6, sliderInput(ns("ncp"), "Dimensions retenues pour l'imputation", min = 1, max = 4, value = 2, step = 1))),
      p(class = "note", "Deux dimensions constituent le réglage initial, sans sélection automatique. Comparer plusieurs valeurs permet d'examiner la sensibilité des relations à l'imputation. Les valeurs observées sont conservées ; seules les cellules manquantes sont remplacées.")) else
      p("Suppression des observations comportant au moins un NA sur les 14 variables cliniques. Aucune variable n'est supprimée et aucune valeur n'est imputée."),
    div(class = "callout", textOutput(ns("resume"))),
    h4("1 · Composition de l'échantillon par centre"),
    plotlyOutput(ns("centres"), height = "320px"),
    if (imputation) tagList(h4("Valeurs reconstruites par variable"),
      plotlyOutput(ns("imputations"), height = "400px"),
      p(class = "note", "Une forte proportion de valeurs reconstruites rend l'interprétation de la variable plus dépendante de la méthode d'imputation.")),
    h4("2 · Mesures cliniques selon le diagnostic"),
    plotlyOutput(ns("distributions"), height = "540px"),
    p(class = "note", "La ligne centrale est la médiane, la boîte couvre les 50 % centraux et les points isolés indiquent les valeurs au-delà des moustaches. Chaque mesure possède sa propre échelle ; survoler une boîte affiche ses quartiles."),
    h4("3 · Part de diagnostics positifs selon chaque modalité"),
    plotlyOutput(ns("categories"), height = "720px"),
    p(class = "note", "Chaque point indique la proportion de diagnostics positifs parmi les observations de la modalité ; le trait le relie au taux de l'échantillon (ligne pointillée). Orange : modalité plus souvent associée à la maladie que la moyenne. L'effectif est donné au survol."),
    h4("4 · Corrélations entre mesures numériques"),
    plotlyOutput(ns("correlations"), height = "440px"),
    p(class = "note", "Corrélations de Spearman : −1 indique une relation décroissante, +1 une relation croissante. Une corrélation ne démontre pas de causalité."),
    h4("5 · Relations entre deux mesures"),
    fluidRow(column(4, selectInput(ns("x"), "Mesure horizontale", choices = NULL)),
      column(4, selectInput(ns("y"), "Mesure verticale", choices = NULL)),
      column(4, selectInput(ns("couleur"), "Couleur des observations",
        choices = c("Diagnostic" = "diagnostic", "Centre" = "provenance")))),
    plotlyOutput(ns("relations"), height = "440px"),
    if (imputation) p(class = "note", "Les croix signalent les observations dont au moins une des deux mesures a été imputée. Cliquer sur une entrée de légende la masque."),
    h4("6 · ACM des variables catégorielles"),
    p("L'ACM décrit les profils des variables catégorielles. Le diagnostic et la provenance sont supplémentaires : ils ne construisent pas les axes."),
    fluidRow(
      column(6, plotlyOutput(ns("acm_individus"), height = "520px")),
      column(6, plotlyOutput(ns("acm_modalites"), height = "520px"))),
    p(class = "note", "Les observations proches ont des profils catégoriels similaires. Seules les modalités les mieux représentées sur le plan sont nommées ; les autres le sont au survol. Les modalités proches de l'origine sont peu discriminantes. Les axes des deux parcours sont calculés séparément."),
    if (imputation) p(class = "note", "Les résultats décrivent un jeu complété par une imputation unique. Les relations peuvent être renforcées par la reconstruction et ne mesurent pas l'incertitude d'imputation.")
  )
}

parcours_serveur <- function(id, reference, colonnes, categories, libelles, sources,
                             imputation = FALSE) {
  moduleServer(id, function(input, output, session) {
    quanti <- setdiff(colonnes, c(categories, "diagnostic"))
    choix <- setNames(quanti, libelles[quanti])
    libelles_courts <- sub(" \\(.*\\)$", "", libelles)
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
      d$groupe <- factor(d$diagnostic, levels = 0:1, labels = names(couleurs_diagnostic))
      d$centre <- factor(unname(sources[d$provenance]), levels = unname(sources))
      d
    })
    output$resume <- renderText({
      p <- parcours()
      paste0(nrow(p$donnees), " observations analysées sur ", nrow(reference), " — ",
        p$n_exclus, " observations exclues. ",
        if (imputation) paste0(sum(p$masque), " cellules imputées avec ", p$ncp,
          " dimensions ; méthode ", p$methode, ".") else "Cas complets uniquement.",
        if (length(p$avertissements)) paste(" Avertissement de calcul :", paste(p$avertissements, collapse = " ; ")))
    })
    output$centres <- renderPlotly({
      d <- donnees()
      b <- as.data.frame(table(centre = d$centre, diagnostic = d$groupe))
      b$total <- ave(b$Freq, b$centre, FUN = sum)
      b <- b[b$Freq > 0, ]
      b$texte <- paste0("<b>", b$centre, "</b> · ", b$total, " observations<br>", b$diagnostic,
                        " : ", b$Freq, " (", pct_fr(100 * b$Freq / b$total), ")")
      totaux <- unique(b[c("centre", "total")])
      p <- ggplot(b, aes(centre, Freq, fill = diagnostic, text = texte)) +
        geom_col(width = .55, colour = "white", linewidth = .4) +
        geom_text(data = totaux, aes(centre, total + max(total) * .07, label = total),
                  inherit.aes = FALSE, colour = couleur_encre, size = 4) +
        scale_fill_manual(values = couleurs_diagnostic) + scale_x_discrete(drop = FALSE) +
        labs(x = NULL, y = "Observations") + theme_app() +
        theme(panel.grid.major.x = element_blank())
      interactif(p)
    })
    output$imputations <- renderPlotly({
      p <- parcours()
      b <- data.frame(variable = unname(libelles[colnames(p$masque)]),
                      n = colSums(p$masque), pct = 100 * colMeans(p$masque))
      b$variable <- factor(b$variable, levels = b$variable[order(b$pct)])
      b$texte <- paste0("<b>", b$variable, "</b><br>", b$n, " valeurs imputées sur ",
                        nrow(p$masque), " (", pct_fr(b$pct), ")")
      g <- ggplot(b, aes(variable, pct, text = texte)) +
        geom_col(fill = "#E88432", width = .6) +
        geom_text(aes(y = pct + 6, label = pct_fr(pct)), colour = couleur_encre, size = 3.4) +
        scale_y_continuous(limits = c(0, 108), breaks = seq(0, 100, 25)) +
        coord_flip() + labs(x = NULL, y = "Pourcentage de valeurs imputées") +
        theme_app() + theme(panel.grid.major.y = element_blank())
      interactif(g, legende = FALSE)
    })
    output$distributions <- renderPlotly({
      d <- donnees()
      b <- do.call(rbind, lapply(quanti, function(v)
        data.frame(variable = unname(libelles[v]), valeur = d[[v]], diagnostic = d$groupe)))
      b$variable <- factor(b$variable, levels = unname(libelles[quanti]))
      p <- ggplot(b, aes(diagnostic, valeur, fill = diagnostic)) +
        geom_boxplot(width = .55, outlier.alpha = .35, outlier.size = 1, colour = "#3A4B5E") +
        facet_wrap(~variable, scales = "free_y", ncol = 3) +
        scale_fill_manual(values = couleurs_diagnostic) + labs(x = NULL, y = NULL) +
        theme_app(11) + theme(panel.grid.major.x = element_blank())
      interactif(p, tooltip = "y")
    })
    output$categories <- renderPlotly({
      d <- donnees()
      taux_global <- 100 * mean(d$diagnostic == 1)
      b <- do.call(rbind, lapply(categories, function(v) {
        m <- libeller_modalites(d[[v]], v)
        n <- table(m)
        k <- tapply(d$diagnostic == 1, m, sum)
        data.frame(mesure = unname(libelles_courts[v]), modalite = names(n),
                   n = as.integer(n), positifs = as.integer(k[names(n)]))
      }))
      b <- b[b$n > 0, ]
      b$taux <- 100 * b$positifs / b$n
      b$ecart <- ifelse(b$taux >= taux_global, "Au-dessus du taux moyen", "En dessous du taux moyen")
      b$modalite <- factor(b$modalite, levels = rev(unique(b$modalite)))
      b$mesure <- factor(b$mesure, levels = unique(b$mesure))
      b$texte <- paste0("<b>", b$mesure, " : ", b$modalite, "</b><br>",
        pct_fr(b$taux), " de diagnostics positifs<br>", b$positifs, " sur ", b$n, " observations")
      p <- ggplot(b, aes(y = modalite, colour = ecart)) +
        geom_vline(xintercept = taux_global, linetype = "dashed", colour = "#9AAABB") +
        geom_segment(aes(x = taux_global, xend = taux, yend = modalite), linewidth = .9) +
        geom_point(aes(x = taux, text = texte), size = 3.2) +
        facet_wrap(~mesure, scales = "free_y", ncol = 2) +
        scale_colour_manual(values = c("Au-dessus du taux moyen" = "#E88432",
                                       "En dessous du taux moyen" = "#165DDE")) +
        scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, " %")) +
        labs(x = paste0("Part de diagnostics positifs (moyenne : ", pct_fr(taux_global, 0), ")"), y = NULL) +
        theme_app(11) + theme(panel.grid.major.y = element_blank())
      interactif(p)
    })
    output$correlations <- renderPlotly({
      d <- donnees()
      m <- cor(d[quanti], method = "spearman")
      noms <- unname(libelles_courts[quanti])
      b <- data.frame(x = factor(noms[col(m)], levels = noms), y = factor(noms[row(m)], levels = rev(noms)),
                      rho = as.vector(m), garder = as.vector(lower.tri(m)))
      b <- b[b$garder, ]
      b$texte <- paste0("<b>", b$x, " × ", b$y, "</b><br>ρ de Spearman = ",
                        formatC(b$rho, format = "f", digits = 2, decimal.mark = ","))
      p <- ggplot(b, aes(x, y, fill = rho, text = texte)) +
        geom_tile(colour = "white", linewidth = 1.5) +
        geom_text(aes(label = formatC(rho, format = "f", digits = 2, decimal.mark = ","),
                      colour = abs(rho) > .5), size = 3.8) +
        scale_colour_manual(values = c("FALSE" = couleur_encre, "TRUE" = "white"), guide = "none") +
        scale_fill_gradient2(low = "#E88432", mid = "#F1F3F6", high = "#165DDE", limits = c(-1, 1), name = "ρ") +
        labs(x = NULL, y = NULL) + theme_app() + theme(panel.grid = element_blank())
      interactif(p, legende = FALSE)
    })
    output$relations <- renderPlotly({
      req(input$x %in% quanti, input$y %in% quanti, input$couleur %in% c("diagnostic", "provenance"))
      d <- donnees()
      par_diagnostic <- input$couleur == "diagnostic"
      d$couleur <- if (par_diagnostic) d$groupe else d$centre
      d$reconstruit <- if (imputation) factor(
        ifelse(rowSums(parcours()$masque[, c(input$x, input$y), drop = FALSE]) > 0,
               "Au moins une imputée", "Observées"), levels = c("Observées", "Au moins une imputée")) else NA
      d$texte <- paste0("<b>", libelles_courts[input$x], "</b> : ", d[[input$x]],
        "<br><b>", libelles_courts[input$y], "</b> : ", d[[input$y]],
        "<br>Diagnostic : ", d$groupe, " · ", d$centre,
        if (imputation) paste0("<br>Mesures : ", d$reconstruit) else "")
      p <- plot_ly(d, x = d[[input$x]], y = d[[input$y]], color = ~couleur,
        colors = if (par_diagnostic) couleurs_diagnostic else couleurs_centres,
        symbol = if (imputation) ~reconstruit else NULL,
        symbols = c("circle", "x"), text = ~texte, hoverinfo = "text",
        type = "scatter", mode = "markers",
        marker = list(size = 8, opacity = .6, line = list(color = "white", width = .5))) |>
        layout(xaxis = list(title = libelles[[input$x]], zeroline = FALSE, gridcolor = "#EDF2F8"),
               yaxis = list(title = libelles[[input$y]], zeroline = FALSE, gridcolor = "#EDF2F8"))
      habiller(p)
    })
    acm <- reactive({
      validate(need(requireNamespace("FactoMineR", quietly = TRUE), 'Installer FactoMineR pour afficher l’ACM.'))
      d <- donnees()
      actives <- categories[vapply(d[categories], function(z) length(unique(z)) > 1L, logical(1))]
      validate(need(length(actives) >= 2, "Au moins deux variables catégorielles sont nécessaires."))
      # Préfixe lisible : les noms de modalités doivent rester uniques entre variables.
      x <- as.data.frame(lapply(actives, function(v)
        factor(paste0(libelles_courts[[v]], " : ", libeller_modalites(d[[v]], v)))))
      names(x) <- actives
      x$diagnostic <- factor(paste("Diagnostic :", d$groupe))
      x$provenance <- factor(paste("Centre :", d$centre))
      # Un centre unique n'est pas une variable supplémentaire exploitable.
      sup <- c("diagnostic", "provenance")
      sup <- sup[vapply(x[sup], nlevels, integer(1)) > 1L]
      x <- x[c(actives, sup)]
      FactoMineR::MCA(x, quali.sup = if (length(sup)) match(sup, names(x)) else NULL,
        ncp = 2, graph = FALSE)
    })
    axes_acm <- function(a) list(x = sprintf("Axe 1 (%.1f %% d'inertie)", a$eig[1,2]),
                                 y = sprintf("Axe 2 (%.1f %% d'inertie)", a$eig[2,2]))
    output$acm_individus <- renderPlotly({
      a <- acm(); d <- donnees(); axes <- axes_acm(a)
      b <- data.frame(x = a$ind$coord[,1], y = a$ind$coord[,2], diagnostic = d$groupe,
        texte = paste0("Diagnostic : ", d$groupe, "<br>", d$centre))
      p <- ggplot(b, aes(x, y, colour = diagnostic, text = texte)) +
        geom_hline(yintercept = 0, colour = "grey85") + geom_vline(xintercept = 0, colour = "grey85") +
        geom_point(alpha = .45, size = 1.8) +
        scale_colour_manual(values = couleurs_diagnostic) +
        labs(title = "Profils des observations", x = axes$x, y = axes$y) +
        theme_app(11) + theme(plot.title = element_text(size = 12, colour = couleur_encre))
      interactif(p)
    })
    output$acm_modalites <- renderPlotly({
      a <- acm(); axes <- axes_acm(a)
      b <- data.frame(x = a$var$coord[,1], y = a$var$coord[,2], nom = rownames(a$var$coord),
        qualite = rowSums(a$var$cos2[, 1:2]), type = "Modalité active")
      if (!is.null(a$quali.sup)) b <- rbind(b, data.frame(x = a$quali.sup$coord[,1],
        y = a$quali.sup$coord[,2], nom = rownames(a$quali.sup$coord),
        qualite = rowSums(a$quali.sup$cos2[, 1:2]), type = "Supplémentaire (diagnostic, centre)"))
      b$etiquette <- ifelse(b$type != "Modalité active" | b$qualite >= quantile(b$qualite, .6), b$nom, "")
      b$survol <- paste0("<b>", b$nom, "</b><br>Qualité sur le plan (cos²) : ",
                         formatC(b$qualite, format = "f", digits = 2, decimal.mark = ","))
      p <- plot_ly(b, x = ~x, y = ~y, color = ~type,
          colors = c("Modalité active" = "#165DDE", "Supplémentaire (diagnostic, centre)" = "#E88432"),
          symbol = ~type, symbols = c("circle", "diamond"),
          text = ~etiquette, hovertext = ~survol, hoverinfo = "text",
          type = "scatter", mode = "markers+text", textposition = "top center",
          textfont = list(size = 10, color = couleur_encre),
          marker = list(size = ~6 + 14 * qualite, opacity = .8, line = list(color = "white", width = 1))) |>
        layout(title = list(text = "Relations entre modalités", x = 0, font = list(size = 14, color = couleur_encre)),
               xaxis = list(title = axes$x, zerolinecolor = "#D5DDE7", gridcolor = "#EDF2F8"),
               yaxis = list(title = axes$y, zerolinecolor = "#D5DDE7", gridcolor = "#EDF2F8"))
      habiller(p)
    })
  })
}

# Valeurs initiales avant réception des contrôles Shiny.
`%||%` <- function(x, y) if (is.null(x)) y else x
