library(shiny)
library(ggplot2)
source("R/design.R", local = TRUE)
source("R/benchmark_imputation.R", local = TRUE)
source("R/pipeline_application.R", local = TRUE)
source("R/pedagogie_na.R", local = TRUE)
source("R/ml_evaluation.R", local = TRUE)
source("R/ml_interface.R", local = TRUE)
source("R/a_propos.R", local = TRUE)

charger_import <- function() {
  env <- new.env(parent = globalenv())
  grDevices::pdf(file = NULL)
  appareil <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(appareil), add = TRUE)
  invisible(capture.output(sys.source("R/import.R", envir = env)))
  env
}
import_uci <- charger_import()
donnees_uci <- import_uci$heart
donnees_uci$diagnostic_initial <- donnees_uci$diagnostic
donnees_uci$diagnostic <- ifelse(is.na(donnees_uci$diagnostic), NA_integer_,
                                as.integer(donnees_uci$diagnostic > 0))
# Distribution intermédiaire : les ? sont déjà NA, les zéros sont encore visibles.
donnees_distribution <- import_uci$heart_avant_recodage
donnees_distribution$diagnostic <- ifelse(is.na(donnees_distribution$diagnostic), NA_integer_,
  as.integer(donnees_distribution$diagnostic > 0))
benchmark_app <- charger_benchmark_app()
libelles_app <- setNames(import_uci$dictionnaire$libelle, import_uci$colonnes)
# Périmètre clinique explicite : 13 prédicteurs et diagnostic, sans métadonnées.
bilan_perte_na <- calculer_perte_information(donnees_uci, import_uci$colonnes)

function(input, output, session) {
  observeEvent(input$ouvrir_a_propos, ouvrir_a_propos())
  ml_serveur(output, donnees_uci[c(import_uci$colonnes, "provenance")])
  output$perte_na_indicateurs <- renderUI(cartes_perte_information(bilan_perte_na))
  output$perte_na_graphique <- renderPlot(graphique_perte_information(bilan_perte_na),
    bg = palette_heart$card)
  output$perte_na_phrase <- renderText(phrase_perte_information(bilan_perte_na))
  output$pipeline_benchmark <- renderPlot({
    graphique_pipeline_benchmark(
      names(benchmark_app$configuration$modeles),
      repetitions = max(benchmark_app$resultats$repetition),
      proportion = benchmark_app$configuration$proportion)
  }, bg = palette_heart$card)

  updateSelectInput(session, "variable_distribution", choices = setNames(import_uci$colonnes, libelles_app),
                    selected = "cholesterol")
  updateSelectInput(session, "variable_benchmark", choices = setNames(benchmark_app$cibles,
    libelles_app[benchmark_app$cibles]), selected = "cholesterol")
  updateSelectInput(session, "variable_na", choices = setNames(import_uci$colonnes, libelles_app),
                    selected = "cholesterol")
  output$bilan_doublons <- renderText({
    b <- import_uci$bilan_nettoyage
    paste(b$n_doublons_retires, "doublons supprimés :", b$n_brut, "lignes initiales deviennent",
      b$n_final, "observations. Les 14 valeurs originales sont comparées dans chaque centre ; la première occurrence est conservée.")
  })
  output$effectifs <- renderPlot({
    z <- as.data.frame(table(import_uci$heart_brut$provenance))
    names(z) <- c("provenance", "n")
    ggplot(z, aes(provenance, n)) + geom_col(fill = palette_heart$blue, width = .6) +
      geom_text(aes(label = n), vjust = -.5, colour = palette_heart$text, size = 4.6) +
      scale_x_discrete(labels = centres_app) + scale_y_continuous(expand = expansion(mult = c(0, .12))) +
      labs(x = NULL, y = "Observations avant dédoublonnage",
           subtitle = paste(sum(z$n), "lignes réunies par empilement des quatre tables")) + theme_heart()
  })
  output$distribution <- renderPlot({
    v <- req(input$variable_distribution)
    validate(need(v %in% import_uci$colonnes, "Choisir une variable."))
    z <- donnees_distribution[!is.na(donnees_distribution[[v]]), ]
    g <- ggplot(z, aes(x = .data[[v]]))
    if (v %in% c(variables_acm_app, "diagnostic")) {
      z[[v]] <- factor(z[[v]])
      g <- ggplot(z, aes(x = .data[[v]])) + geom_bar(fill = palette_heart$blue)
    } else if (v %in% c("cholesterol", "pa_repos")) {
      z$valeur_nulle <- ifelse(z[[v]] == 0, "Zéro à recoder ensuite", "Autre valeur")
      g <- ggplot(z, aes(x = .data[[v]], fill = valeur_nulle)) +
        geom_histogram(bins = 25, boundary = 0, colour = palette_heart$card) +
        scale_fill_manual(values = c("Zéro à recoder ensuite" = palette_heart$amber,
                                    "Autre valeur" = palette_heart$blue), name = NULL) +
        theme(legend.position = "bottom")
    } else g <- g + geom_histogram(bins = 25, fill = palette_heart$blue, colour = palette_heart$card)
    g + labs(x = libelles_app[[v]], y = "Effectif observé") + theme_heart() +
      theme(legend.position = "bottom")
  })
  output$distribution_effectif <- renderText({
    v <- req(input$variable_distribution)
    x <- donnees_distribution[[v]]
    n <- sum(!is.na(x))
    zeros <- if (v %in% c("cholesterol", "pa_repos")) paste0(
      " Dont ", sum(x == 0, na.rm = TRUE), " zéros (",
      if (n > 0) sprintf("%.1f", 100 * sum(x == 0, na.rm = TRUE) / n) else "0.0",
      " % des valeurs renseignées), conservés ici avant leur recodage en NA.") else ""
    paste0(n, " valeurs renseignées ; ", sum(is.na(x)),
      " NA issus des ? exclus du graphique. Après dédoublonnage, avant recodage des zéros.", zeros)
  })
  output$na_cumules <- renderPlot({
    v <- req(input$variable_na)
    validate(need(v %in% import_uci$colonnes, "Choisir une variable."))
    z <- compter_disponibilite_app(donnees_uci, v)
    ggplot(z, aes(provenance, n, fill = statut)) + geom_col(width = .65, position = position_stack(reverse = TRUE)) +
      geom_text(aes(label = ifelse(n > 0, n, "")),
        position = position_stack(vjust = .5, reverse = TRUE), colour = "white", size = 4.2) +
      scale_x_discrete(labels = centres_app, drop = FALSE) +
      scale_fill_manual(values = c("Valeurs observées" = "#78A9DF", "NA" = "#244B78"), drop = FALSE) +
      labs(x = NULL, y = "Nombre d'observations", fill = NULL, subtitle = libelles_app[[v]]) +
      theme_heart() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 20, hjust = 1))
  }, bg = palette_heart$card)
  output$na_detail <- renderPlot({
    g <- import_uci$graphique_na_provenance() + theme_heart(13)
    g$layers[[2]]$aes_params$size <- 4.2
    g
  }, bg = palette_heart$card)
  output$benchmark_erreurs <- renderPlot({
    v <- req(input$variable_benchmark)
    metrique <- if (benchmark_app$types[[v]] == "quantitative") "RMSE" else "taux_erreur"
    r <- benchmark_app$resultats
    z <- r[r$variable == v & r$metrique == metrique & r$statut == "ok" & is.finite(r$valeur), ]
    validate(need(nrow(z) > 0, "Aucun résultat disponible."))
    ordre <- names(sort(tapply(z$valeur, z$modele_imputation, mean)))
    z$modele_imputation <- factor(z$modele_imputation, levels = ordre)
    ggplot(z, aes(modele_imputation, valeur)) + geom_boxplot(fill = palette_heart$blue, alpha = .6) +
      geom_jitter(width = .1, height = 0, colour = palette_heart$text, alpha = .4, size = 1) +
      labs(x = "Méthode", y = if (metrique == "RMSE") "RMSE" else "Taux d'erreur",
           subtitle = libelles_app[[v]]) + theme_heart()
  })
  output$choix_modeles <- renderUI({
    controles <- lapply(benchmark_app$cibles, function(v) {
      selectInput(paste0("modele_", v), libelles_app[[v]], choices = benchmark_app$choix[[v]],
                  selected = benchmark_app$defauts[[v]])
    })
    do.call(bslib::layout_columns, c(controles,
      list(col_widths = bslib::breakpoints(xs = 12, md = 6, lg = 4), fill = FALSE)))
  })
  selections <- reactive(setNames(vapply(benchmark_app$cibles, function(v) {
    x <- input[[paste0("modele_", v)]]
    if (is.null(x)) benchmark_app$defauts[[v]] else x
  }, character(1)), benchmark_app$cibles))
  resultat <- reactiveVal(NULL)
  erreur <- reactiveVal(NULL)
  observeEvent(input$calculer_imputation, {
    m <- input$nombre_imputations
    req(m)
    if (!is.finite(m) || m != as.integer(m) || m < 5 || m > 100) {
      erreur("Choisir un nombre entier de jeux entre 5 et 100.")
      return()
    }
    choix <- selections()
    erreur(NULL)
    nouvel <- tryCatch(withProgress(message = "Imputation et ACM", value = 0, {
      imps <- calculer_imputations_app(donnees_uci, choix, benchmark_app, m = as.integer(m),
        progression = function(i, total) setProgress(value = .9 * i / total,
          detail = paste("Jeu", i, "sur", total)))
      setProgress(value = .95, detail = "Projection des jeux sur l'ACM")
      acm <- calculer_acm_app(imps)
      list(imputations = imps, acm = acm)
    }), error = function(e) { erreur(conditionMessage(e)); NULL })
    if (!is.null(nouvel)) {
      resultat(nouvel)
      actif <- if (!is.null(resultat_acm())) resultat_acm()$acm else nouvel$acm
      n <- ncol(actif$individus[[1]])
      dims <- setNames(seq_len(n), paste("Dimension", seq_len(n)))
      updateSelectInput(session, "axe_x", choices = dims, selected = 1)
      updateSelectInput(session, "axe_y", choices = dims, selected = 2)
    }
  })
  output$statut_imputation <- renderText({
    e <- erreur()
    r <- resultat()
    if (!is.null(e)) return(paste("Calcul non terminé :", e,
      if (!is.null(r)) "Les graphiques conservent le dernier calcul réussi." else ""))
    if (is.null(r)) return("Choisir les modèles, puis cliquer sur Calculer l'imputation.")
    perime <- !identical(selections(), r$imputations$selections) ||
      !identical(as.integer(input$nombre_imputations), as.integer(r$imputations$m))
    paste(r$imputations$m, "jeux calculés ;", nrow(donnees_uci), "observations conservées.",
      if (perime) "Les choix ont changé : recalculer pour les appliquer à l'ACM." else "ACM disponible sur la troisième page.")
  })
  # Résultats exploratoires autonomes : aucun réajustement à chaque affichage.
  resultat_acm <- reactiveVal(NULL)
  erreur_acm <- reactiveVal(NULL)
  publier_axes <- function(r) {
    n <- ncol(r$individus[[1]])
    dims <- setNames(seq_len(n), paste("Dimension", seq_len(n)))
    updateSelectInput(session, "axe_x", choices = dims, selected = 1)
    updateSelectInput(session, "axe_y", choices = dims, selected = 2)
  }
  observeEvent(input$calculer_acm, {
    mode <- input$acm_mode
    req(mode)
    erreur_acm(NULL)
    lourdes <- isTRUE(input$acm_lourdes)
    nboot <- as.integer(input$acm_nboot)
    req(length(nboot) == 1L, !is.na(nboot), nboot %in% c(20L, 50L, 100L))
    nouveau <- tryCatch(withProgress(message = "Calcul de l’ACM", {
      if (mode == "modeles") {
        if (is.null(resultat())) stop("Calculer d’abord l’imputation sur Gestion des NA.")
        v <- if (lourdes) variables_acm_app else setdiff(variables_acm_app, acm_variables_lourdes)
        r <- calculer_acm_app(resultat()$imputations, variables = v)
        origine <- resultat()$imputations
      } else {
        r <- acm_resultat_cache(donnees_uci, mode, lourdes, nboot)
        origine <- NULL
      }
      list(acm = r, mode = mode, lourdes = lourdes, nboot = nboot, origine = origine)
    }), error = function(e) { erreur_acm(conditionMessage(e)); NULL })
    if (!is.null(nouveau)) {
      resultat_acm(nouveau)
      publier_axes(nouveau$acm)
    }
  })
  output$acm_taux_na <- renderText({
    v <- acm_variables_lourdes
    paste("Taux de NA dans les données nettoyées :", paste(paste0(libelles_app[v],
      " : ", pct_fr(100 * vapply(donnees_uci[v], function(z) mean(is.na(z)), numeric(1)), 2)), collapse = " ; "))
  })
  output$statut_acm <- renderText({
    if (!is.null(erreur_acm())) return(paste("Calcul non terminé :", erreur_acm(),
      "Le dernier résultat réussi reste affiché s’il existe."))
    r <- resultat_acm()
    if (is.null(r)) return("Les modèles choisis sont disponibles après calcul sur Gestion des NA. Pour les autres stratégies, cliquer sur Calculer l’ACM.")
    perime <- !identical(input$acm_mode, r$mode) || !identical(isTRUE(input$acm_lourdes), r$lourdes) ||
      (r$mode == "multiple" && !identical(as.integer(input$acm_nboot), r$nboot)) ||
      (r$mode == "modeles" && !identical(resultat()$imputations, r$origine))
    paste("Résultat appliqué :", names(acm_modes)[match(r$mode, unname(acm_modes))],
      if (perime) "· Réglages modifiés : cliquer sur Calculer l’ACM pour les appliquer." else "")
  })
  acm_resultat <- reactive({
    if (!is.null(resultat_acm())) return(resultat_acm()$acm)
    validate(need(!is.null(resultat()), "Calculer les imputations sur Gestion des NA, ou choisir une stratégie ACM puis cliquer sur Calculer l’ACM."))
    resultat()$acm
  })
  axes <- reactive({
    a <- as.integer(c(req(input$axe_x), req(input$axe_y)))
    validate(need(length(unique(a)) == 2, "Choisir deux dimensions différentes."),
      need(all(is.finite(a)) && all(a >= 1L) && all(a <= ncol(acm_resultat()$individus[[1]])),
        "Choisir des dimensions disponibles pour l’ACM affichée."))
    a
  })
  output$acm_resume <- renderText(acm_texte_resultat(acm_resultat(), axes()))
  output$statut_ellipses <- renderText({
    if (!isTRUE(input$ellipses_acm)) return("")
    res <- acm_resultat()
    if (res$m < 3L) return("Aucune ellipse : le résultat affiché comporte moins de trois jeux. Calculer des imputations multiples, puis appliquer la stratégie souhaitée avec Calculer l’ACM.")
    limite <- if (is.null(input$limite_ellipses)) 60L else as.integer(input$limite_ellipses)
    req(is.finite(limite), limite > 0L)
    b <- bilan_ellipses_acm(res, axes(), limite)
    paste(if (identical(input$acm_mode, "modeles") && isTRUE(res$individus_reference))
        "Le graphe affiche encore une stratégie de référence : cliquer sur Calculer l’ACM pour appliquer les modèles choisis." else "",
      "Contours de la couleur des points :", b$modalites[["affichees"]], "ellipses de modalités ;",
      if (isTRUE(res$individus_reference)) "vue individus de référence, sans ellipse individuelle." else
        paste(b$individus[["affichees"]], "ellipses individuelles affichées sur",
              b$individus[["disponibles"]], "positions variables (les plus dispersées en priorité)."),
      if (!b$modalites[["disponibles"]] && !b$individus[["disponibles"]])
        "Les positions sont invariantes sur le plan choisi : aucune ellipse non nulle à tracer." else "",
      "Une dispersion sur un seul axe produit un segment ; les positions identiques restent des points.")
  })
  output$acm_valeurs_propres <- renderPlot({
    r <- acm_resultat()
    z <- data.frame(dimension = seq_len(nrow(r$ref$eig)), valeur = r$ref$eig[, 1], pct = r$ref$eig[, 2])
    ggplot(z, aes(factor(dimension), valeur)) + geom_col(fill = palette_heart$blue) +
      geom_text(aes(label = pct_fr(pct)), vjust = -.5, colour = palette_heart$text, size = 3.7) +
      scale_y_continuous(expand = expansion(mult = c(0, .15))) +
      labs(x = "Dimension", y = "Valeur propre", subtitle = "Pourcentages d'inertie brute de l'ACM de référence") + theme_heart()
  }, bg = palette_heart$card)
  output$acm_modalites <- renderPlot({
    graphe_acm_app(acm_resultat(), "modalites", axes(), isTRUE(input$ellipses_acm))
  }, bg = palette_heart$card)
  output$acm_individus <- renderPlot({
    limite <- as.integer(req(input$limite_ellipses))
    validate(need(is.finite(limite) && limite > 0, "Choisir une limite d'ellipses positive."))
    graphe_acm_app(acm_resultat(), "individus", axes(), isTRUE(input$ellipses_acm), limite)
  }, bg = palette_heart$card)
}
