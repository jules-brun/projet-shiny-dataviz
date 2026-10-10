# Interface Shiny — lancer depuis la racine avec shiny::runApp(".")
library(shiny)
library(plotly)
source("R/graphiques.R", local = TRUE)
source("R/visualisation.R", local = TRUE)

fluidPage(
  tags$head(tags$style(HTML("
    :root { --blue:#165DDE; --ink:#143052; --muted:#60758B; --line:#E2EBF6; }
    body { background:#F5F8FD; color:var(--ink); font-family:Inter,ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif; font-size:15px; line-height:1.6; }
    .container-fluid { max-width:1360px; padding:32px; }
    .hero { padding:12px 0 24px; max-width:900px; }
    .eyebrow { color:var(--blue); font-size:12px; letter-spacing:.15em; font-weight:700; text-transform:uppercase; }
    h1 { font-size:38px; font-weight:700; letter-spacing:-1.2px; margin:10px 0; line-height:1.2; }
    h4 { font-size:19px; font-weight:650; margin:30px 0 14px; }
    .hero p,.note { color:var(--muted); }
    .nav-tabs { border:0; gap:6px; display:flex; flex-wrap:wrap; margin-bottom:16px; }
    .nav-tabs>li>a { border:0!important; border-radius:10px; color:var(--muted); padding:12px 22px; font-weight:600; }
    .nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover { background:var(--blue); color:white; }
    .nav-tabs>li>a:hover { background:#E8F0FE; color:var(--blue); }
    .tab-content { background:#FFF; border:1px solid var(--line); border-radius:20px; padding:28px; box-shadow:0 5px 25px #173b6610; }
    .callout { background:#EFF5FF; border-left:3px solid var(--blue); padding:16px 20px; border-radius:0 10px 10px 0; margin:16px 0; }
    .recodage { background:#EFF5FF; border:1px solid #B8D4FA; border-top:4px solid var(--blue); border-radius:12px; padding:20px; margin-bottom:16px; }
    .recodage h4 { margin:0 0 12px; }
    .recodage .regle { color:var(--blue); font-size:24px; font-weight:700; }
    .form-control,.selectize-input { border-color:var(--line); border-radius:9px; box-shadow:none; }
    .btn-primary { background:var(--blue); border:0; border-radius:10px; padding:12px 20px; font-weight:600; }
    a { color:var(--blue); } a:focus,button:focus { outline:2px solid #3987FF; outline-offset:3px; }
    pre { background:#F4F8FE; border:1px solid var(--line); border-radius:12px; padding:18px; }
    details { margin:20px 0; padding:18px; border:1px solid var(--line); border-radius:12px; }
    summary { cursor:pointer; color:var(--blue); font-weight:600; }
    .recit-matrice { overflow-x:auto; margin:16px 0; }
    .footer { color:var(--muted); font-size:12px; padding:24px 0; }
    .shiny-plot-output,.plotly { margin:16px 0; }
    @media(max-width:767px) { .container-fluid { padding:16px; } h1 { font-size:28px; } .tab-content { padding:16px; } .nav-tabs>li>a { padding:10px 12px; } }
  "))),
  div(class = "hero",
    div(class = "eyebrow", "UCI HEART DISEASE / EXPLORATION"),
    h1("Diagnostic cardiaque et prédiction"),
    p("Analyse des variables cliniques associées à la présence d’une maladie cardiaque et de leur apport à la prédiction. Cette étude s’appuie sur le jeu de données Heart Disease du dépôt UCI, issu de quatre centres : Cleveland, la Hongrie, la Suisse et le VA Medical Center de Long Beach.")
  ),
  tabsetPanel(
    tabPanel("1 · Données",
      selectInput("source_apercu", "Provenance", choices = c("Toutes" = "toutes")),
      helpText("Ce filtre concerne uniquement cet onglet ; le centre choisi est mis en avant ci-dessous."),
      h4("Effectifs par provenance et diagnostic"), plotlyOutput("effectifs", height = "320px"),
      h4("Distribution des variables cliniques"),
      selectInput("variable_apercu", "Variable", choices = NULL),
      uiOutput("dictionnaire"), textOutput("dimensions"),
      plotlyOutput("apercu", height = "400px"),
      p(class = "note", "Barres empilées par diagnostic ; les valeurs manquantes sont exclues. Survoler une barre affiche l'effectif et sa part dans la modalité ou la classe."),
      h4("Doublons : une occurrence conservée par paire"),
      div(class = "callout", textOutput("bilan_doublons")),
      p("Comparaison des 14 variables originales au sein de chaque provenance, avant recodage des zéros. La première occurrence est conservée. Les NA aux mêmes positions comptent comme identiques. Ce choix de dédoublonnage ne constitue pas une preuve d'identité du patient."),
      uiOutput("doublons")
    ),
    tabPanel("2 · Valeurs manquantes",
      h4("Deux étapes pour identifier les valeurs manquantes"),
      uiOutput("recodages_na"),
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
      ),
      h4("Pourcentage de NA par variable et provenance"),
      p("Chaque case indique la proportion de valeurs manquantes pour une variable dans un centre, après conversion des ? et recodage des zéros ciblés. Le dénominateur est l'effectif nettoyé de ce centre."),
      plotlyOutput("na_detail", height = "560px"),
      h4("Combinaisons de valeurs manquantes"),
      p("Chaque ligne représente une combinaison de variables manquantes (orange) et observées (gris clair), de la plus fréquente à la moins fréquente. L'effectif et la part de chaque combinaison sont indiqués à gauche et au survol."),
      plotlyOutput("combinaisons_na", height = "560px"),
      p(class = "note", "Les variables sont triées selon leur proportion de NA ; seules celles qui en comportent sont affichées. Les pourcentages sont calculés sur toutes les observations nettoyées. La provenance est exclue de ce graphique."),
      h4("Nombre de mesures manquantes par observation"),
      plotlyOutput("na_global_ui", height = "360px"),
      p(class = "note", "Toutes les observations nettoyées sont incluses, y compris celles sans NA. Les couleurs indiquent le centre d'origine.")
    ),
    tabPanel("3 · Relations entre variables",
      p("Deux parcours de visualisation pour comparer les relations cliniques avec et sans imputation des valeurs manquantes."),
      tabsetPanel(
        tabPanel("Cas complets", parcours_ui("complets")),
        tabPanel("Imputation par composantes", parcours_ui("imputes", imputation = TRUE))
      )
    ),
    tabPanel("4 · Comprendre les données manquantes",
      p("Chaque ligne correspond à une variable dont on étudie l'absence (X), chaque colonne à une caractéristique croisée (Y), mesurée sur les observations où Y est renseignée. Les cellules indiquent la p-value ajustée par Benjamini-Hochberg ; le survol détaille le test, l'effectif et le V de Cramér."),
      div(class = "recit-matrice",
        plotlyOutput("absence_matrice", width = "100%", height = "620px")
      ),
      p(class = "note", "Variables quantitatives découpées en tertiles. P-values Monte-Carlo estimées ; diagonale non testée ; aucune imputation."),
      h4("Conclusion"),
      div(class = "callout", textOutput("absence_conclusion"))
    )
  ),
  div(class = "footer", "UCI Heart Disease · Janosi et al. (1989) · DOI 10.24432/C52P4X · CC BY 4.0")
)
