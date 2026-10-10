# Interface Shiny — lancer depuis la racine avec shiny::runApp(".")
library(shiny)
library(plotly)
source("R/graphiques.R", local = TRUE)
source("R/visualisation.R", local = TRUE)
source("R/premieres_visus.R", local = TRUE)
source("R/acm_imputation.R", local = TRUE)
source("R/presentation.R", local = TRUE)

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
    .lead { font-size:18px; color:var(--ink); max-width:900px; margin-bottom:8px; }
    .section-presentation { margin-top:34px; }
    .section-presentation>h4 { border-bottom:1px solid var(--line); padding-bottom:8px; }
    .indicateurs,.guide-onglets { display:grid; grid-template-columns:repeat(auto-fit,minmax(190px,1fr)); gap:14px; margin:20px 0; }
    .indicateur,.guide-carte { background:#F7FAFE; border:1px solid var(--line); border-radius:14px; padding:16px 18px; }
    .indicateur-valeur { font-size:30px; font-weight:700; color:var(--blue); line-height:1.1; }
    .indicateur-titre { font-weight:600; }
    .guide-carte p { margin:6px 0 0; }
    .table-scroll { overflow-x:auto; }
    table.dictionnaire { width:100%; border-collapse:collapse; font-size:14px; }
    table.dictionnaire th { text-align:left; color:var(--muted); font-weight:600; border-bottom:2px solid var(--line); padding:8px 10px; }
    table.dictionnaire td { border-bottom:1px solid var(--line); padding:10px; vertical-align:top; }
    table.dictionnaire td.nombre { text-align:right; white-space:nowrap; }
    .famille { display:inline-block; background:#E8F0FE; color:var(--blue); border-radius:999px; padding:2px 10px; font-size:12px; font-weight:600; white-space:nowrap; }
    .filtres { background:#F7FAFE; border:1px solid var(--line); border-radius:14px; padding:14px 18px 4px; margin-bottom:8px; }
    @media(max-width:767px) { .container-fluid { padding:16px; } h1 { font-size:28px; } .tab-content { padding:16px; } .nav-tabs>li>a { padding:10px 12px; } }
  "))),
  div(class = "hero",
    div(class = "eyebrow", "UCI HEART DISEASE / EXPLORATION"),
    h1("Diagnostic cardiaque et prédiction"),
    p("Analyse des variables cliniques associées à la présence d’une maladie cardiaque et de leur apport à la prédiction. Cette étude s’appuie sur le jeu de données Heart Disease du dépôt UCI, issu de quatre centres : Cleveland, la Hongrie, la Suisse et le VA Medical Center de Long Beach.")
  ),
  tabsetPanel(id = "onglets",
    tabPanel("Présentation", value = "presentation", presentation_ui()),
    tabPanel("1 · Données", value = "donnees",
      div(class = "filtres", fluidRow(
        column(4, selectInput("source_apercu", "Provenance", choices = c("Toutes" = "toutes"))),
        column(3, radioButtons("sexe_apercu", "Sexe", inline = TRUE,
          choices = c("Tous" = "tous", "Femmes" = "0", "Hommes" = "1"))),
        column(5, sliderInput("age_apercu", "Âge (ans)", min = 28, max = 77, value = c(28, 77), step = 1))),
        helpText("Ces filtres concernent uniquement cet onglet."), textOutput("dimensions")),
      h4("Effectifs par provenance et diagnostic"), plotlyOutput("effectifs", height = "320px"),
      p(class = "note", "Les quatre centres restent affichés, avec les filtres de sexe et d'âge ; le centre choisi est mis en avant."),
      h4("Premières analyses"),
      fluidRow(
        column(6, selectInput("question_apercu", "Question explorée", choices = questions_apercu,
          selected = "age_sexe")),
        column(6,
          conditionalPanel("['quanti_diag', 'tendance'].includes(input.question_apercu)",
            selectInput("quanti_apercu", "Mesure", choices = c(
              "Âge" = "age", "Pression au repos" = "pa_repos", "Cholestérol" = "cholesterol",
              "Fréquence cardiaque max." = "fc_max", "Dépression ST" = "depress_st"),
              selected = "fc_max")),
          conditionalPanel("input.question_apercu == 'quali_diag'",
            selectInput("quali_apercu", "Caractéristique", choices = c(
              "Type de douleur thoracique" = "type_doul_thor", "Sexe" = "sexe",
              "Glycémie à jeun" = "glyc_jeun_elevee", "ECG au repos" = "ecg_repos",
              "Angine à l'effort" = "angine_effort", "Pente du segment ST" = "pente_st",
              "Nombre de vaisseaux" = "nb_vaisseaux", "Test au thallium" = "test_thallium"))),
          conditionalPanel("input.question_apercu == 'variable'",
            selectInput("variable_apercu", "Variable", choices = NULL)))),
      conditionalPanel("input.question_apercu == 'variable'", uiOutput("dictionnaire")),
      plotlyOutput("apercu", height = "440px"),
      div(class = "callout", strong("Ce que montre ce graphe. "), textOutput("interpretation_apercu", inline = TRUE)),
      p(class = "note", "Données nettoyées, sans imputation. Plusieurs tests sont faits sur les mêmes patients : une p-value isolée proche de 0,05 est à lire avec prudence. Une association n'est pas une causalité, l'échantillon est hospitalier."),
      h4("Doublons : une occurrence conservée par paire"),
      div(class = "callout", textOutput("bilan_doublons")),
      p("Comparaison des 14 variables originales au sein de chaque provenance, avant recodage des zéros. La première occurrence est conservée. Les NA aux mêmes positions comptent comme identiques. Ce choix de dédoublonnage ne constitue pas une preuve d'identité du patient."),
      uiOutput("doublons")
    ),
    tabPanel("2 · Valeurs manquantes", value = "manquantes",
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
      p(strong(textOutput("combinaisons_resume", inline = TRUE))),
      plotlyOutput("combinaisons_na", height = "560px"),
      p(class = "note", "Les variables sont triées selon leur proportion de NA ; seules celles qui en comportent sont affichées. Les pourcentages sont calculés sur toutes les observations nettoyées. La provenance est exclue de ce graphique."),
      h4("Nombre de mesures manquantes par observation"),
      plotlyOutput("na_global_ui", height = "360px"),
      p(class = "note", "Toutes les observations nettoyées sont incluses, y compris celles sans NA. Les couleurs indiquent le centre d'origine.")
    ),
    tabPanel("3 · Relations entre variables", value = "relations",
      p("Deux parcours de visualisation pour comparer les relations cliniques avec et sans imputation des valeurs manquantes."),
      tabsetPanel(
        tabPanel("Cas complets", parcours_ui("complets")),
        tabPanel("Imputation par composantes", parcours_ui("imputes", imputation = TRUE))
      )
    ),
    tabPanel("4 · Comprendre les données manquantes", value = "absences",
      p("Chaque ligne correspond à une variable dont on étudie l'absence (X), chaque colonne à une caractéristique croisée (Y), mesurée sur les observations où Y est renseignée. Les cellules indiquent la p-value ajustée par Benjamini-Hochberg ; le survol détaille le test, l'effectif et le V de Cramér."),
      div(class = "recit-matrice",
        plotlyOutput("absence_matrice", width = "100%", height = "620px")
      ),
      p(class = "note", "Variables quantitatives découpées en tertiles. P-values Monte-Carlo estimées ; diagonale non testée ; aucune imputation."),
      h4("Conclusion"),
      div(class = "callout", textOutput("absence_conclusion"))
    ),
    tabPanel("5 · ACM & imputation", value = "acm", acm_ui())
  ),
  div(class = "footer", "UCI Heart Disease · Janosi et al. (1989) · DOI 10.24432/C52P4X · CC BY 4.0")
)
