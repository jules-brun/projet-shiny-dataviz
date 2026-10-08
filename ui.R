# Interface bslib — les identifiants des contrôles et des sorties sont conservés.
library(shiny)
library(bslib)
source("R/design.R", local = TRUE)
source("R/overview.R", local = TRUE)
source("R/visualisation.R", local = TRUE)

page_navbar(
  title = div(class = "heart-brand", icone_heart("heart-pulse", "1.4em"),
    span("HEART", span(class = "brand-light", " / DISEASE"))),
  id = "navigation_heart", selected = "overview", window_title = "Heart Disease · Analyse cardiovasculaire",
  lang = "fr", theme = theme_application_heart(), fillable = FALSE,
  navbar_options = navbar_options(bg = palette_heart$bg, theme = "dark", underline = FALSE),
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  footer = div(class = "heart-footer", span("HEART / DISEASE"),
    span("UCI Heart Disease · Janosi et al. (1989) · CC BY 4.0"),
    tags$a("DOI 10.24432/C52P4X", href = "https://doi.org/10.24432/C52P4X", target = "_blank", rel = "noopener noreferrer")),
  nav_panel("Overview", icon = icone_heart("grid-1x2"), value = "overview", overview_ui()),
  nav_panel("Données", icon = icone_heart("database"), value = "donnees",
    entete_heart("01 / LA COHORTE", "Explorer les données cliniques",
      "Comprendre les distributions, les provenances et les choix de nettoyage."),
    layout_columns(
      carte_heart("Périmètre de l'exploration",
        selectInput("source_apercu", "Provenance", choices = c("Toutes" = "toutes")),
        helpText("Ce filtre concerne uniquement cette page."),
        textOutput("dimensions"), icone = "funnel", plein_ecran = FALSE),
      carte_heart("Effectifs par provenance", plotOutput("effectifs", height = "280px"), icone = "geo-alt"),
      col_widths = breakpoints(xs = 12, lg = c(4, 8)), fill = FALSE),
    carte_heart("Distribution des variables cliniques",
      selectInput("variable_apercu", "Variable", choices = NULL), uiOutput("dictionnaire"),
      plotOutput("apercu", height = "380px"), icone = "bar-chart"),
    carte_heart("Traçabilité du dédoublonnage",
      div(class = "callout", textOutput("bilan_doublons")),
      p(class = "note", "Comparaison des 14 variables originales au sein de chaque provenance, avant recodage des zéros. La première occurrence est conservée. Les NA aux mêmes positions comptent comme identiques. Ce choix de dédoublonnage ne constitue pas une preuve d'identité du patient."),
      uiOutput("doublons"), icone = "files", plein_ecran = FALSE)
  ),
  nav_panel("Valeurs manquantes", icon = icone_heart("clipboard2-pulse"), value = "manquantes",
    entete_heart("02 / LA QUALITÉ DES DONNÉES", "Lire les absences",
      "Distinguer les marqueurs UCI des recodages et situer les manques dans la cohorte."),
    carte_heart("Deux étapes pour identifier les valeurs manquantes", uiOutput("recodages_na"),
      p(class = "note", "Seuls les zéros de pression au repos et de cholestérol sont recodés ; les autres zéros sont conservés. Les graphiques portent sur les données après dédoublonnage et recodage, sans imputation."),
      tags$details(
        tags$summary("Justification bibliographique du recodage des zéros"),
        tags$ul(
          tags$li(strong("Cholestérol total (cholesterol, mg/dL). "),
                  "Le cholestérol est une substance produite par l'organisme et transportée dans le sang [2]. Une valeur exactement nulle est retenue comme non exploitable dans cette cohorte et recodée en NA."),
          tags$li(strong("Pression artérielle au repos (pa_repos, mmHg). "),
                  "La pression correspond à la force exercée par le sang dans les artères [3]. Une mesure de repos égale à zéro n'est pas retenue comme une pression clinique exploitable ici : elle est recodée en NA."),
          tags$li(strong("Les autres zéros sont conservés. "),
                  "Ils peuvent représenter une modalité valide : sexe, glycémie non élevée, ECG normal, absence d'angine, dépression ST nulle, aucun vaisseau visualisé ou diagnostic négatif [1].")
        ),
        p(class = "note", "Il s'agit d'une décision analytique fondée sur la plausibilité des mesures. Ces références ne prouvent pas que zéro était un code officiel de valeur manquante dans UCI. Aucun seuil bas autre que zéro n'est appliqué ; aucune valeur n'est imputée."),
        h4("Références bibliographiques"),
        tags$ol(
          tags$li("Janosi A., Steinbrunn W., Pfisterer M., Detrano R. (1989). ",
            tags$a("Heart Disease [Dataset]. UCI Machine Learning Repository.", href = "https://doi.org/10.24432/C52P4X", target = "_blank", rel = "noopener noreferrer"), " DOI : 10.24432/C52P4X."),
          tags$li("National Library of Medicine. MedlinePlus. ",
            tags$a("Cholesterol.", href = "https://medlineplus.gov/cholesterol.html", target = "_blank", rel = "noopener noreferrer"), " Consulté le 3 octobre 2026."),
          tags$li("National Heart, Lung, and Blood Institute (NIH). ",
            tags$a("Low Blood Pressure.", href = "https://www.nhlbi.nih.gov/health/low-blood-pressure", target = "_blank", rel = "noopener noreferrer"), " Consulté le 3 octobre 2026.")
        )
      ), icone = "journal-medical", plein_ecran = FALSE),
    carte_heart("Pourcentage de NA par variable et provenance",
      p(class = "note", "Chaque case indique la proportion de valeurs manquantes pour une variable dans un centre, après conversion des ? et recodage des zéros ciblés. Le dénominateur est l'effectif nettoyé de ce centre."),
      plotOutput("na_detail", height = "630px"), icone = "grid-3x3"),
    carte_heart("Combinaisons de valeurs manquantes",
      p(class = "note", "À gauche, les barres indiquent la proportion de NA par variable. À droite, chaque ligne représente une combinaison de valeurs observées (bleu) et manquantes (ambre) ; la barre associée indique sa fréquence."),
      plotOutput("combinaisons_na", height = "760px"),
      p(class = "note", "Les variables sont triées selon leur proportion de NA. Les combinaisons affichées comportent au moins une valeur manquante ; leurs fréquences sont calculées sur toutes les observations nettoyées. La provenance est exclue de ce graphique."), icone = "intersect"),
    carte_heart("Nombre de mesures manquantes par observation", plotOutput("na_global_ui", height = "330px"), icone = "bar-chart")
  ),
  nav_panel("Relations cliniques", icon = icone_heart("diagram-3"), value = "relations",
    entete_heart("03 / LES RELATIONS CLINIQUES", "Comparer les profils",
      "Deux parcours de visualisation : cas complets et imputation par composantes. Les résultats restent exploratoires."),
    navset_card_tab(
      nav_panel("Cas complets", icon = icone_heart("check2-circle"), parcours_ui("complets")),
      nav_panel("Imputation par composantes", icon = icone_heart("layers"), parcours_ui("imputes", imputation = TRUE))
    )
  ),
  nav_panel("Absences & associations", icon = icone_heart("grid-3x3"), value = "associations",
    entete_heart("04 / L'EXPLORATION DES ABSENCES", "Comprendre les données manquantes",
      "Matrice exploratoire des associations entre les indicateurs d'absence et les caractéristiques observées."),
    carte_heart("Associations avec l'absence · p-values ajustées BH",
      div(class = "recit-matrice", plotOutput("absence_matrice", width = "1400px", height = "650px")), icone = "grid-3x3"),
    carte_heart("Conclusion exploratoire", div(class = "callout", textOutput("absence_conclusion")),
      icone = "chat-left-text", plein_ecran = FALSE)
  )
)
