# Parcours narratif : uniquement des descriptions des données nettoyées existantes.
serveur_comprendre_na <- function(input, output, session, reference, import, dictionnaire, libelles_sources) {
  variables <- dictionnaire$nom_fr
  sources <- names(libelles_sources)
  libelles <- setNames(dictionnaire$libelle, variables)
  population <- bilan_population_complete(reference, c(variables, "diagnostic"), sources)
  updateSelectInput(session, "recit_variable", choices = setNames(variables, libelles), selected = "cholesterol")
  variable <- reactive({ req(input$recit_variable %in% variables); input$recit_variable })
  fiche <- reactive(bilan_variable_absence(reference, import$heart_avant_recodage, variable(), sources))
  description <- reactive(description_absence(reference, variable()))
  franciser_bilan <- function(b) data.frame(Provenance = unname(libelles_sources[b$provenance]),
    Observations = b$n, "NA initiaux" = b$na_initiaux, "Zéros recodés" = b$zeros_recodes,
    "NA après nettoyage" = b$n_na, "% de NA" = b$pct_na,
    "Absence totale" = ifelse(b$absence_totale, "Oui", "Non"), check.names = FALSE)
  output$recit_nettoyage <- renderTable({
    data.frame(Étape = c("Observations avant dédoublonnage", "Occurrences dupliquées retirées",
      "Observations nettoyées", "Cellules NA initiales après dédoublonnage", "Cellules créées par recodage", "Cellules NA finales"),
      Effectif = c(import$bilan_nettoyage$n_brut, import$bilan_nettoyage$n_doublons_retires,
        nrow(reference), sum(is.na(import$heart_avant_recodage[import$colonnes])),
        sum(import$journal_recodage$n_zeros_recodes), sum(is.na(reference[import$colonnes]))))
  })
  output$recit_origines <- renderPlot({
    d <- import$na_origine
    d$variable <- factor(d$variable, levels = rev(import$colonnes))
    ggplot(d, aes(nombre, variable, fill = origine)) + geom_col(width = .65) +
      scale_fill_manual(values = c("NA initiaux" = "#B8D4FA", "Zéros recodés" = "#165DDE")) +
      scale_y_discrete(labels = setNames(import$dictionnaire$libelle, import$dictionnaire$nom_fr)) +
      labs(x = "Nombre de cellules manquantes", y = NULL, fill = NULL,
        caption = paste(nrow(reference), "observations après dédoublonnage ; les deux origines s'additionnent.")) +
      theme_minimal(base_size = 10) + theme(legend.position = "top", panel.grid.major.y = element_blank())
  }, res = 110)
  output$recit_carte <- renderPlot(import$graphique_na_provenance(), res = 110)
  output$recit_structure <- renderUI({
    b <- import$na_par_provenance
    decrire <- function(v, src) {
      x <- b[b$variable == v & b$provenance == src, ]
      paste0(libelles[[v]], " à ", libelles_sources[[src]], " : ", x$n_na,
        " NA sur ", x$n_observations, " (", sprintf("%.1f", x$pct_na), " %).")
    }
    # Les exemples sont calculés ; leurs libellés ne classent pas les mécanismes.
    cleveland <- b[b$provenance == "cleveland" & b$variable %in% c("nb_vaisseaux", "test_thallium"), ]
    suisse <- b[b$provenance == "switzerland" & b$variable == "cholesterol", ]
    titre_cleveland <- if (all(cleveland$pct_na < 5)) "Peu de valeurs manquantes à Cleveland. " else "Valeurs manquantes à Cleveland. "
    titre_suisse <- if (nrow(suisse) && suisse$n_na == suisse$n_observations && suisse$n_observations > 0)
      "Absence totale après recodage. " else "Cholestérol en Suisse après nettoyage. "
    tagList(
      p(strong(titre_cleveland), paste(vapply(c("nb_vaisseaux", "test_thallium"), decrire, character(1), src = "cleveland"), collapse = " ")),
      tags$details(tags$summary("Absences dans les autres centres : les effectifs exacts"),
        lapply(setdiff(sources, "cleveland"), function(src) p(paste(vapply(c("nb_vaisseaux", "test_thallium", "pente_st"), decrire, character(1), src = src), collapse = " ")))),
      p(strong(titre_suisse), decrire("cholesterol", "switzerland"),
        " Cette absence analytique résulte du traitement des zéros ; elle ne prouve pas que la mesure n'a jamais été réalisée."),
      p(class = "note", "La concentration par centre est un constat descriptif. Elle ne classe aucune variable comme MCAR, MAR ou MNAR."))
  })
  output$recit_fiche_bilan <- renderTable(franciser_bilan(fiche()), digits = 1)
  output$recit_fiche_constat <- renderText({
    b <- fiche()
    total <- sum(b$n)
    na <- sum(b$n_na)
    phrase <- paste0(sum(b$na_initiaux), " NA initiaux + ", sum(b$zeros_recodes),
      " zéros recodés = ", na, " valeurs manquantes sur ", total,
      if (total) sprintf(" (%.1f %%).", 100 * na / total) else " (aucune observation).")
    if (!na && total) phrase <- paste(phrase, "Aucun manque observé pour cette variable ; cela ne renseigne pas le mécanisme des autres variables.")
    if (any(b$absence_totale)) phrase <- paste(phrase, "Absence totale dans :",
      paste(libelles_sources[b$provenance[b$absence_totale]], collapse = ", "),
      ". Une imputation intercentres exigerait des hypothèses de transfert supplémentaires.")
    phrase
  })
  output$recit_definition <- renderText(definitions_variables_na[[variable()]])
  output$recit_collecte <- renderText({
    paste("La documentation UCI définit cette variable et son codage ; les fichiers processed fournissent les valeurs et la provenance.",
      "Ils ne donnent pas de raison d'absence pour chaque cellule, ni un historique vérifié de sa collecte.",
      if (variable() %in% import$variables_zero_na)
        "Ici, les zéros ont été écartés pour plausibilité : cela ne démontre ni un code officiel de manque ni une non-mesure."
      else "Une cellule NA signale une information indisponible dans ces données, sans en établir la cause.")
  })
  output$recit_proportions <- renderPlot({
    b <- fiche()
    b$centre <- factor(b$provenance, levels = sources)
    b$etiquette <- ifelse(b$n > 0, sprintf("%d/%d\n%.1f %%", b$n_na, b$n, b$pct_na), "aucune observation")
    ggplot(b, aes(centre, pct_na)) + geom_col(fill = "#165DDE", width = .55, na.rm = TRUE) +
      geom_text(aes(y = ifelse(is.na(pct_na), 0, pct_na), label = etiquette), vjust = -.3, size = 3.4) +
      scale_x_discrete(labels = libelles_sources, drop = FALSE) +
      scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 25), labels = function(x) paste0(x, " %")) +
      labs(x = NULL, y = "Valeurs manquantes", caption = "Dénominateur : observations nettoyées de chaque centre. Aucun test statistique.") +
      theme_minimal(base_size = 11) + theme(panel.grid.major.x = element_blank())
  }, res = 110)
  output$recit_age <- renderPlot({
    desc <- description()
    d <- desc$donnees
    b <- desc$groupes
    d <- d[!is.na(d$age), ]
    etiquettes <- setNames(paste0(b$statut, "\n(n = ", b$n, "; âges disponibles : ", b$n_age, ")"), b$statut)
    vides <- b[b$n_age == 0, , drop = FALSE]
    vides$message <- ifelse(vides$n == 0, "aucune observation", "aucun âge disponible")
    ggplot(d, aes(statut, age)) + geom_boxplot(fill = "#B8D4FA", colour = "#165DDE", width = .4) +
      geom_text(data = vides, aes(x = statut, y = 0, label = message), inherit.aes = FALSE, colour = "#60758B", size = 3.3) +
      scale_x_discrete(limits = c("Observée", "Manquante"), labels = etiquettes, drop = FALSE) +
      labs(x = paste("Statut de", libelles[[variable()]]), y = "Âge (ans)",
        caption = paste(sum(b$na_age), "âge(s) manquant(s) exclu(s) du graphique. Distribution descriptive, sans test.")) +
      theme_minimal(base_size = 11)
  }, res = 110)
  output$recit_sexe <- renderTable({
    d <- description()$donnees
    n <- table(d$statut, factor(d$sexe, levels = c(0, 1), labels = c("Féminin (code 0)", "Masculin (code 1)")))
    data.frame(Statut = rownames(n), n, "Sexe manquant" = description()$groupes$na_sexe,
      "Total" = description()$groupes$n, check.names = FALSE)
  })
  output$recit_cas_complets <- renderTable({
    b <- population
    data.frame(Provenance = unname(libelles_sources[b$provenance]),
      "Nettoyées" = b$n_initial, "Cas complets" = b$n_complet, "Exclues" = b$n_exclus,
      "% conservées dans le centre" = b$pct_conserves,
      "% de la population initiale" = b$pct_population_initiale,
      "% de la population complète" = b$pct_population_complete, check.names = FALSE)
  }, digits = 1)
  output$recit_composition <- renderPlot({
    b <- population
    long <- rbind(data.frame(provenance = b$provenance, population = "Données nettoyées", n = b$n_initial, pct = b$pct_population_initiale),
      data.frame(provenance = b$provenance, population = "Cas complets", n = b$n_complet, pct = b$pct_population_complete))
    long$provenance <- factor(long$provenance, levels = sources)
    long$population <- factor(long$population, levels = c("Données nettoyées", "Cas complets"))
    long$texte <- ifelse(is.na(long$pct), "aucune observation", sprintf("%.1f %%\n(n = %d)", long$pct, long$n))
    ggplot(long, aes(provenance, pct, fill = population)) +
      geom_col(width = .55, show.legend = FALSE, na.rm = TRUE) +
      geom_text(aes(y = ifelse(is.na(pct), 0, pct), label = texte), vjust = -.3, size = 3.2) +
      facet_wrap(~ population) + scale_fill_manual(values = c("#B8D4FA", "#165DDE")) +
      scale_x_discrete(labels = libelles_sources, drop = FALSE) +
      scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 25), labels = function(x) paste0(x, " %")) +
      labs(x = NULL, y = "Part de chaque centre dans la population", caption = "Les proportions décrivent la composition ; les effectifs décrivent la perte de lignes.") +
      theme_minimal(base_size = 10) + theme(panel.grid.major.x = element_blank())
  }, res = 110)
  output$recit_selection <- renderText(interpretation_cas_complets(population, libelles_sources))
  output$recit_conclusion <- renderUI({
    na <- sum(is.na(reference[import$colonnes]))
    tagList(
      p(strong("Constats établis. "), paste(nrow(reference), "observations nettoyées et", na,
        "cellules manquantes. Les effectifs par centre et les pertes sur cas complets sont calculés ci-dessus.")),
      p(strong("Hypothèses sur la collecte. "), "Les profils d'absence motivent des pistes, mais le mécanisme reste indéterminé sans informations supplémentaires."),
      p(strong("Décisions déjà prises. "), "Dédoublonnage, recodage ciblé des zéros et diagnostic binaire avec conservation du code original."),
      p(strong("Choix restant à faire. "), "Définir la population, les variables et les hypothèses de traitement pour expliquer les associations ; définir la cible, la validation et une procédure apprise sur l'entraînement pour prédire. Aucune variable supplémentaire n'est retirée ici."))
  })
  output$recit_references <- renderUI(tags$ul(lapply(references_comprendre_na, function(ref) {
    tags$li(tags$a(ref$titre, href = ref$url, target = "_blank", rel = "noopener noreferrer"))
  })))
  invisible(list(fiche = fiche, description = description, population = population))
}
