# Serveur Shiny — dépend de R/import.R et des quatre fichiers processed.
# Packages à installer une fois : install.packages(c("shiny", "ggplot2", "DT"))
library(shiny)
library(ggplot2)

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

# Sélection des seules variables cliniques : éviter la fuite du diagnostic
# original et ne pas inclure la provenance dans la formule diagnostic ~ .
variables_modelisation <- import_uci$colonnes
predicteurs <- setdiff(variables_modelisation, "diagnostic")
cas_complets <- complete.cases(donnees_uci[variables_modelisation])
donnees_modelisation <- donnees_uci[cas_complets, variables_modelisation]
variables_categorielles <- c("sexe", "type_doul_thor", "glyc_jeun_elevee",
  "ecg_repos", "angine_effort", "pente_st", "test_thallium", "nb_vaisseaux")
donnees_modelisation[variables_categorielles] <-
  lapply(donnees_modelisation[variables_categorielles], factor)
libelles_sources <- c(cleveland = "Cleveland", hungarian = "Hongrie",
                       switzerland = "Suisse", va = "VA Long Beach")

# Référence propre au quatrième onglet, avec un identifiant de ligne source stable.
source("R/gestion_na.R", local = TRUE)
source("R/modelisation.R", local = TRUE)
dictionnaire_na <- creer_dictionnaire_na(import_uci$dictionnaire, variables_categorielles)
reference_na <- new.env(parent = emptyenv())
reference_na$donnees <- donnees_uci
reference_na$donnees$id_ligne <- with(import_uci$details[!import_uci$retirer, ],
  paste(provenance, ligne_source, sep = ":"))
lockBinding("donnees", reference_na)
source("R/comprendre_na.R", local = TRUE)
source("R/serveur_na.R", local = TRUE)
source("R/tests_absence.R", local = TRUE)
# Un seul balayage sur la référence nettoyée, partagé entre les sessions.
associations_absence <- analyser_absences(reference_na$donnees, dictionnaire_na)

function(input, output, session) {
  output$indicateurs <- renderUI({
    b <- import_uci$bilan_nettoyage
    carte <- function(valeur, libelle) div(class = "metric", strong(valeur), span(libelle))
    div(class = "metric-row",
      carte(b$n_final, "observations après nettoyage"),
      carte(length(import_uci$fichiers), "centres de provenance"),
      carte(b$n_doublons_retires, "occurrences dupliquées retirées"),
      carte(b$n_zeros_recodes, "zéros recodés en NA")
    )
  })
  output$bilan_doublons <- renderText({
    b <- import_uci$bilan_nettoyage
    paste(b$n_doublons_retires, "occurrences retirées sur", b$n_brut,
          "lignes initiales ;", b$n_final,
          "observations conservées. Une ligne par paire est gardée. Les fichiers sources sont inchangés.")
  })
  output$journal_zeros <- DT::renderDT({
    DT::formatRound(DT::datatable(import_uci$journal_recodage, rownames = FALSE,
      options = list(scrollX = TRUE, dom = "t", pageLength = 8)), "pct_na_finaux", 2)
  })
  output$origine_na <- renderPlot({
    d <- import_uci$na_origine
    d$variable <- factor(d$variable, levels = rev(import_uci$colonnes))
    ggplot(d, aes(x = pct, y = variable, fill = origine)) +
      geom_col(width = .68) +
      scale_fill_manual(values = c("NA initiaux" = "#B8D4FA", "Zéros recodés" = "#165DDE")) +
      scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, " %")) +
      scale_y_discrete(labels = setNames(import_uci$dictionnaire$libelle,
                                        import_uci$dictionnaire$nom_fr)) +
      labs(x = "Pourcentage des observations après dédoublonnage", y = NULL, fill = NULL,
           caption = "Les deux contributions s'additionnent. Dénominateur commun : toutes les observations nettoyées.") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top", panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank())
  }, res = 110)
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
          "colonnes affichables (diagnostic original inclus).")
  })
  output$apercu <- DT::renderDT({
    req(input$n_apercu)
    n <- max(1, min(100, as.integer(input$n_apercu)))
    DT::datatable(head(apercu(), n), rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 6))
  })
  output$effectifs <- renderTable({
    setNames(as.data.frame(table(apercu()$provenance)), c("Provenance", "Effectif"))
  })
  output$dictionnaire <- DT::renderDT({
    d <- import_uci$dictionnaire
    d$libelle[d$nom_fr == "diagnostic"] <- "Diagnostic binaire dans l'application ; code UCI conservé dans diagnostic_initial"
    DT::datatable(d, rownames = FALSE, options = list(pageLength = 14, dom = "t", scrollX = TRUE))
  })
  output$doublons <- DT::renderDT({
    DT::datatable(import_uci$doublons_details, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 6))
  })
  output$carte_na <- renderPlot({ import_uci$graphique_na_provenance() }, res = 110)
  output$na_global_ui <- renderTable(import_uci$na_global, digits = 2)
  output$na_sources_ui <- DT::renderDT({
    DT::formatRound(DT::datatable(import_uci$na_sources, rownames = FALSE,
      options = list(scrollX = TRUE, dom = "t")),
      c("pct_cellules_na", "pct_lignes_avec_na"), 2)
  })
  output$na_detail <- DT::renderDT({
    DT::formatRound(DT::datatable(import_uci$na_par_provenance, rownames = FALSE,
      options = list(pageLength = 14, scrollX = TRUE)), "pct_na", 2)
  })

  # Garder un échantillon commun, indépendant de la variable retirée.
  updateSelectInput(session, "variable_modele",
    choices = setNames(predicteurs, import_uci$dictionnaire$libelle[
      match(predicteurs, import_uci$dictionnaire$nom_fr)]),
    selected = "nb_vaisseaux")
  output$bilan_modelisation <- renderText({
    paste(nrow(donnees_modelisation), "lignes complètes retenues sur",
      nrow(donnees_uci), "lignes nettoyées ;", sum(!cas_complets),
      "lignes exclues pour au moins une valeur manquante. Aucune imputation.")
  })
  output$effectifs_modelisation <- renderTable({
    sources <- names(import_uci$fichiers)
    disponibles <- as.integer(table(factor(donnees_uci$provenance, levels = sources)))
    retenues <- as.integer(table(factor(donnees_uci$provenance[cas_complets], levels = sources)))
    data.frame(Provenance = unname(libelles_sources[sources]),
      "Lignes nettoyées" = disponibles, "Lignes retenues" = retenues,
      "Lignes exclues (NA)" = disponibles - retenues, check.names = FALSE)
  })
  formule_reduite <- reactive({
    req(input$variable_modele %in% predicteurs)
    reformulate(setdiff(predicteurs, input$variable_modele), response = "diagnostic")
  })
  output$formules_modeles <- renderText({
    paste("Modèle complet : diagnostic ~ .",
      paste("Modèle réduit :", paste(deparse(formule_reduite()), collapse = " ")),
      sep = "\n")
  })

  # L'ANOVA de modèles emboîtés utilise les mêmes cas complets et la loi binomiale.
  modeles <- reactive({
    req(input$variable_modele %in% predicteurs)
    tryCatch({
      d <- donnees_modelisation
      if (length(unique(d$diagnostic)) != 2)
        stop("Les deux diagnostics doivent être représentés.")
      if (any(vapply(d[variables_categorielles], nlevels, integer(1)) < 2))
        stop("Une variable catégorielle n'a qu'une modalité observée.")
      avertissements <- character()
      resultat <- withCallingHandlers({
        entier <- glm(diagnostic ~ ., family = binomial(), data = d, na.action = na.fail)
        reduit <- glm(formule_reduite(), family = binomial(), data = d, na.action = na.fail)
        if (!reduit$converged || !entier$converged ||
            anyNA(coef(reduit)) || anyNA(coef(entier)))
          stop("Ajustement instable ou coefficients non identifiables : examiner les catégories et les effectifs.")
        if (entier$df.residual <= 0 || reduit$df.residual - entier$df.residual <= 0)
          stop("Effectif ou degrés de liberté insuffisants pour comparer les modèles.")
        comparaison <- anova(reduit, entier, test = "LRT")
        texte <- capture.output({
          cat("Variable évaluée :", input$variable_modele, "\n")
          cat("Cas complets utilisés dans chaque modèle :", nrow(d), "\n")
          cat("Diagnostics absents :", sum(d$diagnostic == 0),
              "— diagnostics présents :", sum(d$diagnostic == 1), "\n\n")
          cat("ANOVA — test du rapport de vraisemblance :\n")
          # Traduire les colonnes affichées du tableau produit par anova().
          tableau <- data.frame(
            Modèle = c("Réduit", "Complet"),
            "DDL résiduels" = comparaison[[1]],
            "Déviance résiduelle" = comparaison[[2]],
            "Écart de DDL" = comparaison[[3]],
            "Écart de déviance" = comparaison[[4]],
            "Valeur p" = comparaison[[5]], check.names = FALSE)
          print(tableau, row.names = FALSE)
          cat("\nAIC (compromis entre ajustement et complexité ; plus faible = meilleur) :\n")
          print(data.frame(Modèle = c("Réduit", "Complet"),
                           AIC = c(AIC(reduit), AIC(entier))), row.names = FALSE)
        })
        # Interprétation de l'apport conditionnel de la variable et du choix par AIC.
        libelle <- import_uci$dictionnaire$libelle[
          match(input$variable_modele, import_uci$dictionnaire$nom_fr)]
        valeur_p <- comparaison[[5]][2]
        ecart_aic <- AIC(reduit) - AIC(entier)
        conclusion_anova <- if (!is.finite(valeur_p)) {
          "L'ANOVA ne permet pas de conclure : la valeur p n'est pas disponible."
        } else if (valeur_p < .05) {
          paste0("L'ANOVA montre que la variable « ", libelle,
            " » apporte une information statistiquement significative au seuil de 5 % pour expliquer la présence de maladie cardiaque, en tenant compte des autres variables (p = ",
            format.pval(valeur_p, digits = 3, eps = .001), ").")
        } else {
          paste0("L'ANOVA ne met pas en évidence d'apport statistiquement significatif de la variable « ",
            libelle, " » au seuil de 5 %, en tenant compte des autres variables (p = ",
            format.pval(valeur_p, digits = 3, eps = .001),
            "). Ce résultat ne prouve pas l'absence d'association.")
        }
        conclusion_aic <- if (abs(ecart_aic) < 1e-8) {
          "Les deux modèles ont le même AIC : ce critère ne les départage pas."
        } else {
          paste0("Parmi ces deux modèles, l'AIC privilégie le modèle ",
            if (ecart_aic > 0) "complet, qui conserve" else "réduit, qui retire",
            " la variable « ", libelle, " » (AIC réduit = ",
            sprintf("%.2f", AIC(reduit)), ", AIC complet = ",
            sprintf("%.2f", AIC(entier)), "; écart = ",
            sprintf("%.2f", abs(ecart_aic)), ").",
            if (abs(ecart_aic) < 2) " L'écart inférieur à 2 indique que les deux modèles restent proches selon ce critère." else "")
        }
        list(texte = texte, conclusion = c(conclusion_anova, conclusion_aic), graphique = data.frame(
          Modèle = factor(c("Réduit", "Complet"), levels = c("Réduit", "Complet")),
          Déviance = c(deviance(reduit), deviance(entier))))
      }, warning = function(w) {
        avertissements <<- c(avertissements, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
      if (length(avertissements)) {
        # Les messages natifs de R peuvent dépendre de la langue de la session.
        resultat$conclusion <- c("L'ajustement a produit un avertissement : les conclusions suivantes doivent être interprétées avec prudence.",
                                  resultat$conclusion)
        resultat$texte <- c("AVERTISSEMENT : R signale un ajustement potentiellement instable (convergence ou probabilités extrêmes). Interprétation à vérifier.",
                            "", resultat$texte)
      }
      resultat$texte <- paste(resultat$texte, collapse = "\n")
      resultat
    }, error = function(e) list(texte = paste("Modélisation non disponible :", conditionMessage(e)),
                                graphique = NULL))
  })
  output$resultat_modeles <- renderText(modeles()$texte)
  output$conclusion_modeles <- renderUI({
    resultat <- modeles()
    if (is.null(resultat$conclusion)) return(p(resultat$texte))
    tagList(lapply(resultat$conclusion, p))
  })
  output$comparaison_modeles <- renderPlot({
    resultat <- modeles()
    validate(need(!is.null(resultat$graphique), "Graphique indisponible : consulter le résultat ci-dessus."))
    ggplot(resultat$graphique, aes(Modèle, Déviance, fill = Modèle)) +
      geom_col(width = .5, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%.2f", Déviance)), vjust = -.5) +
      scale_fill_manual(values = c("Réduit" = "#B8D4FA", "Complet" = "#165DDE")) +
      scale_y_continuous(expand = expansion(mult = c(0, .12))) +
      labs(title = paste("Apport de la variable", input$variable_modele),
           x = NULL, y = "Déviance résiduelle") +
      theme_minimal(base_size = 12)
  }, res = 110)
  serveur_comprendre_na(input, output, session, reference_na$donnees,
                        import_uci, dictionnaire_na, libelles_sources)
  serveur_tests_absence(input, output, session, associations_absence,
                        dictionnaire_na, libelles_sources)

}
