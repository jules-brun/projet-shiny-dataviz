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
    .recit-etape { padding:8px 0 26px; border-bottom:1px solid var(--line); margin-bottom:20px; }
    .recit-etape h3 { font-size:24px; letter-spacing:-.4px; margin:12px 0; }
    .recit-question { color:var(--muted); margin-bottom:18px; }
    .recit-carte { border:1px solid var(--line); border-radius:14px; padding:20px; background:#F8FBFF; margin:12px 0; }
    .recit-matrice { overflow-x:auto; margin:16px 0; }
    .recit-carte h4 { margin:0 0 14px; }
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
      p(class = "note", "Un parcours en cinq étapes pour distinguer ce que montrent les données, les hypothèses sur leur collecte et les décisions encore à prendre."),
      div(class = "recit-etape",
        h3("1 · Identifier les valeurs manquantes"),
        p(class = "recit-question", "Quelles informations sont indisponibles, et pourquoi certains zéros deviennent-ils des NA ?"),
        tableOutput("recit_nettoyage"),
        p("Les NA initiaux et ceux issus du recodage ont des origines distinctes. Les fichiers sources restent inchangés."),
        tags$details(tags$summary("Règles de nettoyage et origine des NA"),
          p("Dans les fichiers processed, le marqueur explicite ? est lu comme NA. Les profils identiques sont dédoublonnés au sein de chaque provenance, en conservant la première occurrence avant recodage. Un profil répété ne prouve pas l'identité d'un patient."),
          p("Les zéros de pression artérielle au repos et de cholestérol sont ensuite recodés en NA pour plausibilité clinique. Ce choix analytique n'est pas une convention officielle UCI démontrée. Les autres zéros valides sont conservés."),
          p("Les comptes de cellules portent sur les 13 variables explicatives et le diagnostic. Les NA initiaux sont comptés après dédoublonnage, avant recodage."),
          plotOutput("recit_origines", height = "440px")
        )
      ),
      div(class = "recit-etape",
        h3("2 · Comprendre où les absences se concentrent"),
        p(class = "recit-question", "Les manques sont-ils dispersés, concentrés dans certains centres ou totaux pour une variable ?"),
        plotOutput("recit_carte", height = "580px"),
        uiOutput("recit_structure")
      ),
      div(class = "recit-etape",
        h3("3 · Discuter les mécanismes possibles"),
        p(class = "recit-question", "Que savons-nous pour une variable précise, et quelles explications restent hypothétiques ?"),
        selectInput("recit_variable", "Variable à explorer", choices = NULL),
        h4("A · Constat observé"), textOutput("recit_fiche_constat"),
        fluidRow(
          column(6, plotOutput("recit_proportions", height = "300px")),
          column(6, plotOutput("recit_age", height = "300px"))
        ),
        p(class = "note", "L'indicateur manquante / observée porte sur la variable choisie. Les groupes réunissent tous les centres : leurs compositions peuvent différer. Ces graphiques descriptifs ne prouvent pas le mécanisme de collecte. Les tests exploratoires sont présentés séparément dans la matrice ci-dessous."),
        tags$details(tags$summary("Détail des NA par centre et exploration du sexe"),
          tableOutput("recit_fiche_bilan"),
          h4("Sexe selon le statut de la variable"), tableOutput("recit_sexe"),
          p(class = "note", "Les effectifs du sexe incluent une colonne dédiée aux valeurs manquantes ; les groupes vides sont conservés.")
        ),
        h4("B · Ce que nous savons"), textOutput("recit_definition"),
        tags$details(tags$summary("Ce qui est documenté sur la collecte et ce qui reste inconnu"),
          textOutput("recit_collecte"),
          p(class = "note", "Définitions et codages : documentation UCI et fichier dataset/heart-disease.names. Les unités non explicitement documentées sont signalées.")
        ),
        tags$details(tags$summary("C · Hypothèses possibles, à vérifier"),
          p(strong("Hypothèse envisagée : "), "un incident de collecte indépendant des caractéristiques et valeurs du patient. Cela pourrait être compatible avec MCAR sous les conditions de cette hypothèse."),
          p(strong("Hypothèse envisagée : "), "une disponibilité liée au centre ou à d'autres informations observées. MAR serait compatible si, après conditionnement sur les informations pertinentes, l'absence ne dépendait plus des valeurs inconnues."),
          p(strong("Hypothèse envisagée : "), "une absence liée à la valeur inconnue ou à des facteurs non observés. MNAR reste possible si la dépendance à cette valeur subsiste après conditionnement."),
          p("Ces pistes ne sont pas des explications établies. Aucun protocole médical ni motif historique de non-mesure n'est déduit des fichiers.")
        ),
        tags$details(tags$summary("D · Conséquences et options à discuter, sans les appliquer"),
          tags$ul(
            tags$li(strong("Supprimer des observations : "), "évite d'inventer des valeurs, mais réduit la précision et peut sélectionner une population particulière."),
            tags$li(strong("Restreindre les centres : "), "peut rendre le périmètre de collecte plus cohérent, mais change la population étudiée et limite la généralisation."),
            tags$li(strong("Exclure la variable d'une analyse : "), "permet de conserver davantage de lignes, mais perd une information et modifie l'ajustement aux autres variables."),
            tags$li(strong("Imputer : "), "peut exploiter davantage d'informations sous des hypothèses explicites ; les valeurs estimées et leur incertitude doivent être reconnues. Une absence totale dans un centre exige des hypothèses de transfert supplémentaires.")
          )
        ),
        h4("Les absences sont-elles associées aux caractéristiques observées ?"),
        p("Pour chaque variable incomplète X, on croise son indicateur d'absence avec une autre variable Y, sur les seules lignes où Y est renseignée. Les autres variables peuvent rester incomplètes."),
        textOutput("absence_perimetre"),
        div(class = "recit-matrice", plotOutput("absence_matrice", width = "1400px", height = "650px")),
        p(class = "note", "La couleur et les cellules utilisent les p-values ajustées BH au seuil exploratoire de 5 %. La famille comprend tous les tests valides de cette matrice ; ce balayage ne constitue pas une analyse confirmatoire."),
        tags$details(tags$summary("Conventions de découpage et choix des tests"),
          p("Âge, pression artérielle, cholestérol, fréquence cardiaque maximale et dépression ST sont découpés en trois classes par les tertiles observés (quantiles R de type 7), calculés une seule fois par variable sur ce périmètre. Les coupures dupliquées sont retirées. S'il reste moins de deux classes non vides, le test est non calculable."),
          p("Ce découpage est exploratoire, non clinique ; les résultats peuvent dépendre des coupures. Les données originales ne sont pas modifiées. Les catégories codées numériquement sont traitées comme des facteurs."),
          DT::DTOutput("absence_classes"),
          p("Pearson sans correction de continuité si tous les effectifs attendus sont au moins 1 et qu'au moins 80 % sont au moins 5. Sinon : Fisher exact pour un tableau 2 × 2 ; Fisher Monte-Carlo pour un tableau plus grand, avec 100 000 réplications et une graine reproductible par paire."),
          p("Les p-values simulées sont estimées, avec une résolution minimale de 1 / 100 001 ; elles ne sont pas affichées comme zéro. Le V de Cramér est calculé à partir de Pearson non corrigé, même lorsque la p-value vient de Fisher. Il décrit l'intensité de l'association, sans interprétation causale.")
        ),
        tags$details(tags$summary("Résultats détaillés : effectifs, méthode, p-values et V de Cramér"),
          DT::DTOutput("absence_details")
        ),
        tags$details(tags$summary("Consulter une paire et son tableau de contingence"),
          fluidRow(
            column(6, selectInput("absence_x", "Variable X dont on étudie l'absence", choices = NULL)),
            column(6, selectInput("absence_y", "Variable Y croisée", choices = NULL))
          ),
          textOutput("absence_paire_resume"), tableOutput("absence_contingence"),
          tableOutput("absence_proportions"), plotOutput("absence_barres", height = "320px")
        ),
        tags$details(tags$summary("Synthèse descriptive pour chaque variable incomplète"),
          uiOutput("absence_syntheses")
        ),
        p(class = "note", "Chaque colonne utilise le sous-échantillon où Y est observée : les effectifs diffèrent. Une association détectée dans ce sous-échantillon ne suffit pas à conclure pour toute la population. Un faible nombre d'absences limite la puissance."),
        p(class = "note", "Ces tests sont non ajustés sur les autres caractéristiques : une association avec l'âge peut refléter des différences entre centres. Ils ne distinguent pas MAR de MNAR ; une association avec la provenance ne prouve pas MAR. MAR reste une hypothèse conditionnelle à argumenter."),
        div(class = "callout",
          strong("MCAR, MAR, MNAR : des mécanismes de collecte, pas des étiquettes de variable."),
          p("Ils peuvent varier selon le centre. À ce stade : mécanisme indéterminé."),
          tags$details(tags$summary("Définitions et limites d'interprétation"),
            tags$ul(
              tags$li(strong("MCAR : "), "l'absence est indépendante des valeurs observées et manquantes."),
              tags$li(strong("MAR : "), "conditionnellement aux informations observées retenues, l'absence ne dépend plus de la valeur manquante."),
              tags$li(strong("MNAR : "), "l'absence dépend encore de la valeur manquante après prise en compte des informations observées.")
            ),
            p("Une association entre absence et caractéristiques observées peut rendre MCAR peu plausible. Ne pas détecter d'association ne prouve pas MCAR. Les seules données observées ne permettent généralement pas de distinguer MAR et MNAR."),
            p("Une variable n'est pas intrinsèquement MAR ou MNAR. Une absence totale dans un centre impose des hypothèses supplémentaires pour transférer une imputation depuis les autres centres."),
            tags$a("Référence : Van Buuren, concepts MCAR / MAR / MNAR", href = "https://stefvanbuuren.name/fimd/sec-idconcepts.html", target = "_blank", rel = "noopener noreferrer")
          )
        )
      ),
      div(class = "recit-etape",
        h3("4 · Observer le coût d'une analyse sur cas complets"),
        p(class = "recit-question", "Combien de lignes perd-on, et de quels centres provient désormais l'échantillon ?"),
        tableOutput("recit_cas_complets"),
        plotOutput("recit_composition", height = "330px"),
        div(class = "callout", textOutput("recit_selection")),
        tags$details(tags$summary("Définition de l'échantillon et portée du constat"),
          p("Un cas complet ne comporte aucun NA sur les 13 variables explicatives et le diagnostic binaire, après nettoyage. Ce sont les mêmes lignes que dans l'onglet Modélisation. Cette comparaison est descriptive : elle ne supprime aucune ligne des données de référence."),
          p("Les parts par centre sont calculées séparément dans la population nettoyée et dans les cas complets. Le pourcentage conservé au sein d'un centre répond à une autre question : la perte de ses observations.")
        )
      ),
      div(class = "recit-etape",
        h3("5 · Préparer deux démarches distinctes"),
        p(class = "recit-question", "Veut-on comprendre les associations ou prédire le diagnostic de nouveaux patients ?"),
        fluidRow(
          column(6, div(class = "recit-carte",
            h4("Comprendre les associations"),
            tags$ol(
              tags$li("Définir la population étudiée et le modèle."),
              tags$li("Étudier les associations ajustées, sans les interpréter automatiquement comme causales."),
              tags$li("Comparer les cas complets à d'autres stratégies si pertinent."),
              tags$li("Envisager l'imputation multiple pour représenter l'incertitude sous des hypothèses explicites.")
            ),
            p(class = "note", "L'imputation simple ou kNN suivie de tests usuels ne prend pas automatiquement en compte l'incertitude d'imputation."),
            tags$a("Référence : imputation multiple avec mice", href = "https://doi.org/10.18637/jss.v045.i03", target = "_blank", rel = "noopener noreferrer")
          )),
          column(6, div(class = "recit-carte",
            h4("Prédire de nouveaux patients"),
            tags$ol(
              tags$li("Définir la population cible : mêmes centres ou nouveau centre."),
              tags$li("Réserver un jeu de test pour la démarche prédictive à venir."),
              tags$li("Choisir et apprendre les traitements sur le jeu d'entraînement."),
              tags$li("Réapprendre imputation, normalisation et sélection de variables dans chaque pli de validation croisée."),
              tags$li("Figer la procédure avant l'évaluation finale."),
              tags$li("Ne jamais utiliser le diagnostic du patient à prédire pour imputer ses mesures.")
            ),
            tags$a("Référence : prévention des fuites d'information", href = "https://scikit-learn.org/stable/common_pitfalls.html", target = "_blank", rel = "noopener noreferrer")
          ))
        ),
        p("L'exploration actuelle porte sur l'ensemble des données. Aucun jeu de test n'a été réservé dès le début ; aucune partition ni nouvelle imputation n'est réalisée ici."),
        tags$details(tags$summary("Pourquoi l'utilisation du diagnostic dépend-elle de l'objectif ?"),
          p("Pour une analyse inférentielle, le diagnostic observé peut être pertinent dans le modèle d'imputation afin de préserver les associations étudiées. Cette procédure n'est pas directement utilisable pour prédire le diagnostic inconnu d'un nouveau patient."),
          p("Pour un futur test, les choix ont déjà été informés par l'exploration globale : ce test ne doit pas être décrit comme entièrement isolé de la démarche initiale. Une validation externe ou prospective pourra être envisagée."),
          tags$a("Référence : choix des prédicteurs d'imputation", href = "https://stefvanbuuren.name/fimd/sec-modelform.html", target = "_blank", rel = "noopener noreferrer")
        )
      ),
      h4("Conclusion provisoire"), uiOutput("recit_conclusion"),
      tags$details(tags$summary("Bibliographie et documentation"),
        uiOutput("recit_references")
      )
    )
  ),
  div(class = "footer", "UCI Heart Disease · Janosi et al. (1989) · DOI 10.24432/C52P4X · CC BY 4.0")
)
