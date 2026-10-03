# Interface Shiny — lancer depuis la racine avec shiny::runApp(".")
library(shiny)

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
    .metric-row { display:grid; grid-template-columns:repeat(4,1fr); gap:16px; margin-bottom:30px; }
    .metric { padding:20px 24px; border:1px solid var(--line); background:#FFF; border-radius:16px; }
    .metric strong { display:block; font-size:30px; font-weight:650; color:var(--blue); letter-spacing:-.7px; }
    .metric span { color:var(--muted); font-size:13px; }
    .nav-tabs { border:0; gap:6px; display:flex; flex-wrap:wrap; margin-bottom:16px; }
    .nav-tabs>li>a { border:0!important; border-radius:10px; color:var(--muted); padding:12px 22px; font-weight:600; }
    .nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover { background:var(--blue); color:white; }
    .nav-tabs>li>a:hover { background:#E8F0FE; color:var(--blue); }
    .tab-content { background:#FFF; border:1px solid var(--line); border-radius:20px; padding:28px; box-shadow:0 5px 25px #173b6610; }
    .callout { background:#EFF5FF; border-left:3px solid var(--blue); padding:16px 20px; border-radius:0 10px 10px 0; margin:16px 0; }
    .form-control,.selectize-input { border-color:var(--line); border-radius:9px; box-shadow:none; }
    .btn-primary { background:var(--blue); border:0; border-radius:10px; padding:12px 20px; font-weight:600; }
    a { color:var(--blue); } a:focus,button:focus { outline:2px solid #3987FF; outline-offset:3px; }
    .table>thead>tr>th { color:#426383; background:#F3F7FD; border-bottom:1px solid var(--line); }
    .table>tbody>tr>td { border-top:1px solid #EDF2F8; }
    pre { background:#F4F8FE; border:1px solid var(--line); border-radius:12px; padding:18px; }
    details { margin:20px 0; padding:18px; border:1px solid var(--line); border-radius:12px; }
    summary { cursor:pointer; color:var(--blue); font-weight:600; }
    .recit-matrice { overflow-x:auto; margin:16px 0; }
    .footer { color:var(--muted); font-size:12px; padding:24px 0; }
    .shiny-plot-output { margin:20px 0; }
    @media(max-width:767px) { .container-fluid { padding:16px; } h1 { font-size:28px; } .metric-row { grid-template-columns:repeat(2,1fr); gap:10px; } .metric { padding:14px; } .tab-content { padding:16px; } .nav-tabs>li>a { padding:10px 12px; } }
  "))),
  div(class = "hero",
    div(class = "eyebrow", "UCI HEART DISEASE / EXPLORATION"),
    h1("Diagnostic cardiaque et prédiction"),
    p("Quelles variables cliniques permettent d’expliquer la présence d’une maladie cardiaque, et dans quelle mesure peut-on la prédire ? Cette étude s’appuie sur le jeu de données Heart Disease du dépôt UCI, issu de quatre centres : Cleveland, la Hongrie, la Suisse et le VA Medical Center de Long Beach.")
  ),
  tabsetPanel(
    tabPanel("1 · Données",
      uiOutput("indicateurs"),
      fluidRow(
        column(3,
          selectInput("source_apercu", "Provenance", choices = c("Toutes" = "toutes")),
          numericInput("n_apercu", "Nombre de premières lignes", 6, min = 1, max = 100),
          helpText("Ce filtre concerne uniquement cet onglet. Le diagnostic original et sa version binaire sont conservés.")
        ),
        column(9,
          h4("Aperçu du tableau fusionné"), textOutput("dimensions"),
          DT::DTOutput("apercu"),
          h4("Effectifs par provenance"), tableOutput("effectifs")
        )
      ),
      h4("Dictionnaire des variables"), DT::DTOutput("dictionnaire"),
      h4("Doublons : une occurrence conservée par paire"),
      div(class = "callout", textOutput("bilan_doublons")),
      p("Comparaison des 14 variables originales au sein de chaque provenance, avant recodage des zéros. La première occurrence est conservée. Les NA aux mêmes positions comptent comme identiques. Ce choix de dédoublonnage ne constitue pas une preuve d'identité du patient."),
      p(class = "note", "Le tableau ci-dessous conserve la trace des lignes d'origine et indique la décision pour chacune."),
      DT::DTOutput("doublons")
    ),
    tabPanel("2 · Valeurs manquantes",
      div(class = "callout",
        p(strong("Deux recodages ciblés : "), "cholesterol = 0 et pa_repos = 0 deviennent NA."),
        p("La carte et les tableaux incluent ces nouveaux NA après retrait des occurrences dupliquées. Les pourcentages utilisent l'effectif nettoyé de chaque centre.")
      ),
      tags$details(
        tags$summary("Pourquoi ces zéros sont-ils traités comme manquants ?"),
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
      h4("Traçabilité des zéros recodés"), DT::DTOutput("journal_zeros"),
      plotOutput("carte_na", height = "630px"),
      fluidRow(
        column(6, h4("Toutes provenances réunies, après nettoyage"), tableOutput("na_global_ui")),
        column(6, h4("Bilan par provenance"), DT::DTOutput("na_sources_ui"))
      ),
      h4("Origine des valeurs manquantes"),
      plotOutput("origine_na", height = "500px"),
      h4("Détail par variable et provenance"), DT::DTOutput("na_detail")
    ),
    tabPanel("3 · Modélisation",
      p("Évaluer l'apport d'une variable au diagnostic en comparant deux régressions logistiques."),
      p(class = "note", "Diagnostic binaire : 0 = absence et 1 = présence selon le critère UCI. Le code 0 ne signifie pas l'absence de tout problème cardiaque."),
      selectInput("variable_modele", "Variable dont on souhaite évaluer l'apport", choices = NULL),
      div(class = "callout",
        p(strong("Aucune imputation. "), "Les quatre centres sont réunis après dédoublonnage et recodage des zéros ciblés. Seules les lignes sans NA sur le diagnostic et les 13 variables explicatives sont retenues."),
        p("Les deux modèles sont ajustés sur exactement les mêmes lignes, y compris lorsque la variable choisie est retirée du modèle réduit. La provenance et le diagnostic original ne sont pas des variables explicatives.")
      ),
      h4("Composition du jeu de données utilisé"),
      textOutput("bilan_modelisation"),
      tableOutput("effectifs_modelisation"),
      h4("Modèles comparés"),
      p("Modèle complet : diagnostic ~ . sur les 13 variables cliniques. Modèle réduit : toutes ces variables sauf celle choisie."),
      p("Les variables catégorielles, y compris le nombre de vaisseaux, sont traitées comme des facteurs. Les variables continues ont un effet linéaire sur le logit. Aucun terme d'interaction n'est ajouté."),
      verbatimTextOutput("formules_modeles"),
      h4("Comparaison par ANOVA"),
      p("La comparaison est recalculée à chaque changement de variable. Le test du rapport de vraisemblance compare le modèle réduit au modèle complet."),
      verbatimTextOutput("resultat_modeles"),
      plotOutput("comparaison_modeles", height = "330px"),
      h4("Conclusion de la comparaison"),
      div(class = "callout", uiOutput("conclusion_modeles")),
      p(class = "note", "Une faible valeur p indique que la variable améliore l'ajustement du modèle en présence des autres variables. Le graphique montre la déviance résiduelle : plus elle est faible, meilleur est l'ajustement aux données utilisées. Cela ne démontre ni causalité ni gain prédictif hors échantillon. Les modèles restent exploratoires.")
    ),
    tabPanel("4 · Comprendre les données manquantes",
      div(class = "recit-matrice",
        plotOutput("absence_matrice", width = "1400px", height = "650px")
      ),
      h4("Conclusion"),
      div(class = "callout", textOutput("absence_conclusion"))
    )
  ),
  div(class = "footer", "UCI Heart Disease · Janosi et al. (1989) · DOI 10.24432/C52P4X · CC BY 4.0")
)
