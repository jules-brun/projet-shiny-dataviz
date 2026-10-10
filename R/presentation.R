# Page de présentation : contexte, provenance des données, dictionnaire des variables,
# préparation et limites. Reprend l'accueil et la page « À propos » de la branche youri-graphes.

# Description clinique des 14 variables (dataset/heart-disease.names).
description_variables <- data.frame(
  nom_fr = c("age", "sexe", "type_doul_thor", "pa_repos", "cholesterol", "glyc_jeun_elevee",
             "ecg_repos", "fc_max", "angine_effort", "depress_st", "pente_st", "nb_vaisseaux",
             "test_thallium", "diagnostic"),
  famille = c("Patient", "Patient", "Symptômes", "Examens au repos", "Examens au repos",
              "Examens au repos", "Examens au repos", "Test d'effort", "Test d'effort",
              "Test d'effort", "Test d'effort", "Imagerie", "Imagerie", "Cible"),
  description = c(
    "Âge du patient au moment de l'examen.",
    "Sexe déclaré du patient.",
    "Nature de la douleur thoracique rapportée. L'angine typique correspond à la douleur classique d'origine cardiaque.",
    "Pression artérielle systolique mesurée au repos, à l'admission.",
    "Cholestérol total dans le sang (cholestérol sérique).",
    "Glycémie à jeun supérieure à 120 mg/dL, indicateur d'un trouble du métabolisme du sucre.",
    "Résultat de l'électrocardiogramme au repos.",
    "Fréquence cardiaque maximale atteinte pendant le test d'effort.",
    "Douleur angineuse déclenchée par l'effort.",
    "Sous-décalage du segment ST provoqué par l'effort par rapport au repos : signe d'un manque d'oxygène du cœur.",
    "Forme du segment ST au pic de l'effort.",
    "Nombre de vaisseaux principaux visibles en fluoroscopie (examen d'imagerie).",
    "Scintigraphie au thallium : perfusion du muscle cardiaque à l'effort.",
    "Diagnostic angiographique : rétrécissement de plus de 50 % du diamètre d'au moins une artère coronaire."),
  mesure = c("Années", "0 = femme, 1 = homme",
             "1 = angine typique, 2 = atypique, 3 = douleur non angineuse, 4 = asymptomatique",
             "mmHg", "mg/dL", "0 = non, 1 = oui",
             "0 = normal, 1 = anomalie ST-T, 2 = hypertrophie ventriculaire gauche",
             "Battements par minute", "0 = non, 1 = oui", "mm",
             "1 = ascendante, 2 = plate, 3 = descendante", "0 à 3",
             "3 = normal, 6 = défaut fixe, 7 = défaut réversible",
             "Code UCI 0 à 4, binarisé : 0 = absence, 1 à 4 = présence"),
  stringsAsFactors = FALSE)

centres_presentation <- data.frame(
  provenance = c("cleveland", "hungarian", "switzerland", "va"),
  institution = c("Cleveland Clinic Foundation (Ohio, États-Unis)",
                  "Hungarian Institute of Cardiology, Budapest (Hongrie)",
                  "Hôpitaux universitaires de Zurich et Bâle (Suisse)",
                  "V.A. Medical Center, Long Beach (Californie, États-Unis)"),
  responsable = c("Robert Detrano", "Andras Janosi", "William Steinbrunn, Matthias Pfisterer", "Robert Detrano"),
  stringsAsFactors = FALSE)

section <- function(titre, ...) div(class = "section-presentation", h4(titre), ...)

presentation_ui <- function() {
  tagList(
    p(class = "lead", "Peut-on repérer une maladie coronarienne à partir d'examens simples, avant une angiographie ? Cette application explore les données cliniques de 918 patients venus de quatre hôpitaux, et prépare la construction d'un modèle de prédiction du diagnostic."),
    uiOutput("presentation_indicateurs"),

    section("Le contexte médical",
      p("La maladie coronarienne correspond au rétrécissement des artères qui irriguent le cœur. Son diagnostic de référence est la ", strong("coronarographie (angiographie)"), ", un examen invasif qui visualise directement les artères. L'idée de ce jeu de données, publié par Detrano et al. (1989), est d'estimer la probabilité de maladie à partir d'informations plus simples à obtenir : profil du patient, symptômes, examens au repos et test d'effort."),
      p("Chaque ligne correspond à un patient adressé pour suspicion de maladie coronarienne. Le diagnostic final est celui de l'angiographie : ", strong("présence"), " si au moins une artère principale est rétrécie de plus de 50 %.")),

    section("D'où viennent les données ?",
      p("Les données proviennent du dépôt UCI Machine Learning Repository (Janosi et al., 1989). Elles réunissent quatre bases hospitalières collectées dans les années 1980, au même format de 14 variables."),
      uiOutput("presentation_centres_texte"),
      fluidRow(
        column(7, plotlyOutput("presentation_centres", height = "300px"),
          p(class = "note", "Cas complets : patients sans aucune valeur manquante sur les 14 variables.")),
        column(5, plotlyOutput("presentation_diagnostic", height = "300px"),
          p(class = "note", "Le diagnostic négatif n'exclut pas tout problème cardiaque : il indique l'absence de rétrécissement de plus de 50 %."))),
      div(class = "callout", strong("À retenir : "), "les centres ne recrutent pas les mêmes patients. La part de malades varie de 36 % (Hongrie) à 94 % (Suisse), et les valeurs manquantes sont presque toutes concentrées hors de Cleveland. La provenance doit être prise en compte dans toutes les analyses.")),

    section("Les 14 variables",
      p("Treize variables explicatives et une variable cible. Le pourcentage de valeurs manquantes est calculé sur les 918 observations nettoyées."),
      uiOutput("presentation_dictionnaire")),

    section("La préparation des données",
      uiOutput("presentation_preparation"),
      p(class = "note", "Les fichiers sources ne sont jamais modifiés : toutes les étapes sont reproduites dans R/import.R. Aucune valeur n'est imputée dans les données de référence ; l'imputation n'intervient que dans les parcours qui la mentionnent explicitement.")),

    section("Comment lire l'application",
      div(class = "guide-onglets",
        lapply(list(
          list("donnees", "1 · Données", "Qui sont les patients et quelles mesures séparent malades et sains, avec les tests statistiques associés."),
          list("manquantes", "2 · Valeurs manquantes", "Où et combien de données manquent, et comment elles ont été repérées."),
          list("relations", "3 · Relations", "Relations entre variables, sur les cas complets ou après imputation."),
          list("absences", "4 · Absences", "L'absence d'une mesure est-elle liée aux autres caractéristiques ?"),
          list("acm", "5 · ACM & imputation", "Effet de la gestion des NA sur l'analyse des correspondances multiples.")),
          function(o) div(class = "guide-carte",
            actionLink(paste0("aller_", o[[1]]), strong(o[[2]])), p(class = "note", o[[3]]))))),

    section("Limites à garder en tête",
      tags$ul(
        tags$li("L'échantillon est hospitalier : ce sont des patients déjà suspects de maladie, pas la population générale."),
        tags$li("Les centres diffèrent fortement (recrutement, proportion de malades, mesures disponibles) : une différence entre malades et sains peut refléter une différence entre centres."),
        tags$li("Certaines variables sont très incomplètes (nombre de vaisseaux : 66 % de NA, thallium : 53 %). Les cas complets se réduisent presque à Cleveland."),
        tags$li("Les analyses sont exploratoires : une association n'est pas une causalité."))),

    section("Le projet et l'équipe",
      p("Projet Data visualisation & Machine Learning · M2 Science des données · Institut Agro · 2026–2027."),
      div(class = "guide-onglets",
        div(class = "guide-carte", strong("Antonin Rivron"), p(class = "note", "Import, nettoyage et gestion des valeurs manquantes")),
        div(class = "guide-carte", strong("Jules Brun"), p(class = "note", "Machine learning")),
        div(class = "guide-carte", strong("Youri Michalowski-Skarbek"), p(class = "note", "Visualisations et interface"))),
      p(class = "note", "Janosi A., Steinbrunn W., Pfisterer M., Detrano R. (1989). Heart Disease. UCI Machine Learning Repository. ",
        tags$a("DOI 10.24432/C52P4X", href = "https://doi.org/10.24432/C52P4X", target = "_blank", rel = "noopener noreferrer"),
        " — licence CC BY 4.0. Detrano R. et al. (1989). International application of a new probability algorithm for the diagnosis of coronary artery disease. American Journal of Cardiology, 64, 304–310."))
  )
}

presentation_serveur <- function(input, output, session, donnees, import, libelles_sources) {
  colonnes <- import$colonnes
  complets <- complete.cases(donnees[colonnes])
  sources <- names(libelles_sources)

  output$presentation_indicateurs <- renderUI({
    boite <- function(valeur, titre, note) div(class = "indicateur",
      div(class = "indicateur-valeur", valeur), div(class = "indicateur-titre", titre), div(class = "note", note))
    div(class = "indicateurs",
      boite(nrow(donnees), "patients", paste(import$bilan_nettoyage$n_doublons_retires, "doublons retirés")),
      boite(length(sources), "hôpitaux", "États-Unis, Hongrie, Suisse"),
      boite(length(setdiff(colonnes, "diagnostic")), "variables explicatives", "+ le diagnostic à prédire"),
      boite(pct_fr(100 * mean(donnees$diagnostic == 1, na.rm = TRUE), 0), "de malades", "selon l'angiographie"),
      boite(sum(complets), "cas complets", paste0(pct_fr(100 * mean(complets), 0), " de la cohorte")))
  })

  output$presentation_centres_texte <- renderUI({
    lignes <- lapply(seq_len(nrow(centres_presentation)), function(i) {
      src <- centres_presentation$provenance[i]
      d <- donnees[donnees$provenance == src, ]
      tags$li(strong(libelles_sources[[src]]), " — ", centres_presentation$institution[i],
        " · responsable : ", centres_presentation$responsable[i], " · ", nrow(d), " patients, ",
        pct_fr(100 * mean(d$diagnostic == 1, na.rm = TRUE), 0), " de malades.")
    })
    tags$ul(lignes)
  })

  output$presentation_centres <- renderPlotly({
    n <- as.integer(table(factor(donnees$provenance, levels = sources)))
    cc <- as.integer(table(factor(donnees$provenance[complets], levels = sources)))
    b <- rbind(data.frame(centre = unname(libelles_sources), statut = "Cas complets", n = cc, total = n),
               data.frame(centre = unname(libelles_sources), statut = "Au moins un NA", n = n - cc, total = n))
    b$centre <- factor(b$centre, levels = rev(unname(libelles_sources)))
    b$statut <- factor(b$statut, levels = c("Au moins un NA", "Cas complets"))
    b$texte <- paste0("<b>", b$centre, "</b> · ", b$total, " patients<br>", b$statut, " : ", b$n,
                      " (", pct_fr(100 * b$n / b$total, 0), ")")
    p <- ggplot(b, aes(centre, n, fill = statut, text = texte)) +
      geom_col(width = .55, colour = "white", linewidth = .4) +
      scale_fill_manual(values = c("Cas complets" = "#165DDE", "Au moins un NA" = "#C9D6E8")) +
      coord_flip() + labs(x = NULL, y = "Patients") + theme_app() +
      theme(panel.grid.major.y = element_blank())
    interactif(p)
  })

  output$presentation_diagnostic <- renderPlotly({
    b <- as.data.frame(table(diagnostic = factor(donnees$diagnostic, levels = 0:1,
                                                 labels = names(couleurs_diagnostic))))
    b$pct <- 100 * b$Freq / sum(b$Freq)
    b$texte <- paste0("<b>", b$diagnostic, "</b> de maladie<br>", b$Freq, " patients (", pct_fr(b$pct), ")")
    p <- ggplot(b, aes(diagnostic, Freq, fill = diagnostic, text = texte)) +
      geom_col(width = .45) +
      geom_text(aes(y = Freq + max(Freq) * .07, label = paste0(Freq, " · ", pct_fr(pct, 0))),
                colour = couleur_encre, size = 4) +
      scale_fill_manual(values = couleurs_diagnostic) +
      labs(x = "Maladie coronarienne", y = "Patients") + theme_app() +
      theme(panel.grid.major.x = element_blank())
    interactif(p, legende = FALSE)
  })

  output$presentation_dictionnaire <- renderUI({
    d <- merge(description_variables, import$dictionnaire, by = "nom_fr", sort = FALSE)
    d <- d[match(colonnes, d$nom_fr), ]
    d$pct_na <- 100 * colMeans(is.na(donnees[d$nom_fr]))
    lignes <- lapply(seq_len(nrow(d)), function(i) tags$tr(
      tags$td(span(class = "famille", d$famille[i])),
      tags$td(strong(d$libelle[i]), br(), code(d$nom_fr[i]), span(class = "note", " · UCI : ", d$nom_uci[i])),
      tags$td(d$description[i]),
      tags$td(d$mesure[i]),
      tags$td(class = "nombre", if (d$pct_na[i] > 0) pct_fr(d$pct_na[i], 0) else "—")))
    div(class = "table-scroll", tags$table(class = "dictionnaire",
      tags$thead(tags$tr(tags$th("Famille"), tags$th("Variable"), tags$th("Description"),
                         tags$th("Unité ou modalités"), tags$th("NA"))),
      tags$tbody(lignes)))
  })

  output$presentation_preparation <- renderUI({
    b <- import$bilan_nettoyage
    etape <- function(numero, titre, texte) div(class = "guide-carte",
      div(class = "eyebrow", numero), strong(titre), p(class = "note", texte))
    div(class = "guide-onglets",
      etape("ÉTAPE 1", "Assembler les 4 centres", paste(b$n_brut, "lignes issues des fichiers « processed » UCI, avec la provenance de chaque patient.")),
      etape("ÉTAPE 2", "Retirer les doublons", paste(b$n_doublons_retires, "profils identiques au sein d'un même centre retirés ;", b$n_final, "patients conservés.")),
      etape("ÉTAPE 3", "Repérer les manques", paste0("Les « ? » des fichiers deviennent des NA ; ", b$n_zeros_recodes, " zéros impossibles (pression, cholestérol) sont recodés en NA.")),
      etape("ÉTAPE 4", "Définir la cible", "Le diagnostic UCI (0 à 4) devient binaire : 0 = absence, 1 à 4 = présence. Le code original est conservé."))
  })

  # Liens du guide vers les onglets correspondants.
  for (onglet in c("donnees", "manquantes", "relations", "absences", "acm")) local({
    cible <- onglet
    observeEvent(input[[paste0("aller_", cible)]],
      updateTabsetPanel(session, "onglets", selected = cible))
  })
}
