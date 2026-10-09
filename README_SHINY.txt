SHINY — HEART DISEASE, VERSION NETTOYÉE

Conserver ui.R, server.R et le dossier R/ ensemble.
Garder dataset/ à la racine avec les quatre fichiers processed fournis.
Depuis une session R ouverte à la racine :
install.packages(c("shiny", "bslib", "bsicons", "tidyverse", "VIM", "FactoMineR", "missMDA", "ggrepel"))
shiny::runApp(".")
Important : missMDA >= 1.23. Avec missMDA 1.21 et FactoMineR 2.18, les imputations
régularisées plantent (svd.triplet ne renvoie plus que ncp valeurs singulières).
Mettre à jour avec install.packages("missMDA").
app.R lance ui.R et server.R ; les définitions de l'interface et du serveur restent dans ces fichiers.

Conserver R/_disable_autoload.R : ce fichier désactive le chargement automatique
des scripts par Shiny. server.R gère les imports dans leurs environnements dédiés.

L'interface bslib présente un accueil Overview, puis les quatre pages d'analyse
existantes : Données, Valeurs manquantes, Relations cliniques et Absences & associations.
L'accueil affiche un tracé ECG décoratif, des indicateurs calculés sur la référence
nettoyée et deux graphiques synthétiques. Les contrôles et sorties d'analyse sont
conservés. Les définitions et décisions de dédoublonnage restent décrites en texte.

PREMIÈRES VISUALISATIONS DE LA COHORTE
Onglet Données : on choisit une question, pas un type de graphe.
- Qui sont les patients ? pyramide des âges par sexe, colorée par diagnostic
- Quelle mesure sépare malades et sains ? violons + Wilcoxon et delta de Cliff
- Le risque augmente-t-il avec une mesure ? % de malades par quintile + IC 95 %,
  Cochran-Armitage et odds ratio logistique
- Quels profils sont les plus à risque ? barres à 100 % + khi-deux (ou Fisher)
  et V de Cramér
- Le diagnostic varie-t-il selon le centre ? / distribution d'une variable
Filtres communs : provenance, sexe, âge. Sous le graphe, « Ce que montre ce
graphe » : une phrase fixe + une phrase recalculée sur la sélection, avec une
alerte si moins de 20 patients. Code dans R/premieres_visus.R.
Données nettoyées sans imputation ; pas d'interprétation causale.

ACM ET IMPUTATION — CINQUIÈME ONGLET
Même ACM des variables qualitatives, avec trois gestions des NA au choix :
- cas complets ;
- imputation simple par ACM régularisée (missMDA::imputeMCA) ;
- imputation multiple (missMDA::MIMCA, 20 jeux) : chaque jeu est projeté sur
  l'ACM, d'où une ellipse à 95 % par modalité.
Nombre de dimensions choisi par validation croisée (estim_ncpMCA).
La provenance sert à imputer, le diagnostic jamais ; les deux sont supplémentaires.
Option pour inclure nb_vaisseaux et test_thallium (plus de 50 % de NA) : en cas
complets il ne reste alors que 299 patients, presque tous de Cleveland.
Le texte sous le graphe change selon la gestion des NA choisie.
Code dans R/acm_imputation.R.

IDENTITÉ VISUELLE ET ORGANISATION
- ui.R assemble page_navbar, nav_panel, card et les grilles responsive.
- R/design.R centralise les couleurs, theme_heart() et les composants de présentation.
- R/overview.R définit l'accueil et ses sorties descriptives, sans modifier la référence.
- R/visualisation.R conserve les deux modules d'analyse existants.
- www/style.css adapte navigation, cartes, formulaires et typographie aux écrans mobiles.
- Graphiques ggplot2 et VIM partagent un fond sombre et des couleurs lisibles.
- Polices système locales ; aucune police distante ni framework JavaScript supplémentaire.
- Les neuf variables d'absence et les méthodes statistiques restent inchangées.
- Les numéros ci-dessous désignent les quatre pages d'analyse, après l'accueil.

NETTOYAGE
- Comparer les 14 variables originales avant tout recodage.
- Conserver la première occurrence de chaque profil dans sa provenance.
- Deux occurrences retirées : une en Hongrie, une à VA Long Beach.
- 920 lignes initiales, 918 après dédoublonnage.
- Zéros de cholesterol et pa_repos transformés en NA : 172 + 1 cellules.
- Les autres zéros ne sont pas transformés. La référence reste sans imputation.
  Seul le second parcours du troisième onglet utilise une copie imputée.
  Le quatrième onglet conserve les absences de la référence nettoyée.
- Les fichiers sources restent inchangés.
- heart_brut, heart_avant_recodage, doublons_supprimes et journal_recodage
  permettent de retrouver les transformations dans R/import.R.
- Les analyses et les pourcentages utilisent les données nettoyées.
- Le diagnostic original est conservé ; le serveur crée la cible binaire.

BIBLIOGRAPHIE
Janosi A., Steinbrunn W., Pfisterer M., Detrano R. (1989).
Heart Disease. UCI Machine Learning Repository. DOI 10.24432/C52P4X.
https://doi.org/10.24432/C52P4X — CC BY 4.0.
National Library of Medicine. MedlinePlus. Cholesterol.
https://medlineplus.gov/cholesterol.html
National Heart, Lung, and Blood Institute. Low Blood Pressure.
https://www.nhlbi.nih.gov/health/low-blood-pressure
Pages consultées le 3 octobre 2026.
Le contexte physiologique motive le choix de recoder zéro ; ces références
ne documentent pas zéro comme code officiel de donnée manquante chez UCI.

MATRICE DES ASSOCIATIONS AVEC L'ABSENCE — QUATRIÈME ONGLET

Le quatrième onglet affiche la matrice croisée, sa légende et une conclusion
calculée selon les associations détectées après correction BH.
Les textes narratifs, fiches, tableaux détaillés, sélecteurs et autres graphiques
ont été retirés de cet onglet.

R/tests_absence.R définit explicitement les neuf variables dont l'absence est testée :
pa_repos, cholesterol, glyc_jeun_elevee, fc_max, angine_effort, depress_st,
pente_st, nb_vaisseaux et test_thallium. Une variable sans NA dans le périmètre
ne constitue pas une ligne. ecg_repos ne constitue plus une ligne d'absence.
Les colonnes restent les 13 variables cliniques, la provenance et le diagnostic
binaire. La diagonale est non applicable ; identifiants et diagnostic_initial
sont exclus. La correction BH est recalculée sur cette famille restreinte.

Périmètre : toutes les données nettoyées des quatre centres, avant toute imputation.
Les tests sont calculés une seule fois au démarrage puis partagés entre sessions.
Chaque paire utilise uniquement les lignes où Y est observée, sans exiger que les
autres variables soient complètes. Les NA de Y ne deviennent pas une catégorie.

Conventions centralisées dans R/tests_absence.R :
- 3 classes quantitatives selon les tertiles observés, quantiles R de type 7 ;
- coupures calculées une fois par variable, doublons retirés ;
- minimum de deux statuts de X et deux classes non vides de Y pour un test ;
- Pearson sans correction si tous les attendus sont >= 1 et au moins 80 % >= 5 ;
- sinon Fisher exact pour 2 × 2, Fisher Monte-Carlo pour les tableaux plus grands ;
- B = 100000, graine de base = 20261003, reproductibilité et restauration du RNG ;
- correction Benjamini-Hochberg sur toutes les p-values valides, seuil 5 %.

Couleur et chiffres : p-values ajustées BH ; bleu clair sous 0,05, bleu nuit sinon,
gris pour non-calculable et diagonale distinguée. Les p-values simulées sont des
estimations avec résolution minimale 1/(B+1), jamais affichées artificiellement zéro.
Les classes sont exploratoires, non cliniques, et peuvent influencer les résultats.
Les tableaux de contingence, bornes, effectifs, raisons de non-calcul et résultats
longs restent accessibles dans associations_absence en R, sans sorties supplémentaires
sur le site. Le V de Cramér est une mesure descriptive, issue du Pearson non corrigé.
Les associations sont non ajustées sur les autres caractéristiques ; ces tests ne
classent pas automatiquement MCAR/MAR/MNAR et ne distinguent pas MAR de MNAR.

Fichiers actifs : ui.R, server.R et R/tests_absence.R. Le dictionnaire explicite de
R/gestion_na.R reste réutilisé. R/comprendre_na.R et R/serveur_na.R conservent les
anciens calculs narratifs mais ne sont plus branchés au serveur de l'application.
Dépendances de présentation : shiny, bslib et bsicons ; ggplot2 pour les graphiques.
Aucune installation automatique au lancement.

VALIDATION
Rscript tests/validation_design.R
Rscript tests/validation_visualisation.R
Rscript tests/validation_tests_absence.R
Ou Rscript tests/validation_na.R
Contrôles : neuf lignes autorisées, exclusion de la ligne ECG, tableaux manuels,
cas limites, petits effectifs, quantiles dupliqués, NA de Y, simulations reproductibles,
correction BH uniquement sur les tests valides, sources inchangées, rendu de la matrice
et cohérence des parcours descriptifs. Les anciens calculs narratifs peuvent être testés
séparément avec Rscript tests/validation_comprendre_na.R.

Documentation des tests :
https://stat.ethz.ch/R-manual/R-devel/library/stats/html/chisq.test.html
https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fisher.test.html
https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html

VISUALISATION DES RELATIONS — TROISIÈME ONGLET
Deux parcours partagent les distributions par diagnostic, les proportions des
modalités, les corrélations de Spearman, un nuage de points paramétrable et une
ACM des catégories avec diagnostic et provenance supplémentaires.
Le premier supprime les lignes incomplètes sur les 14 variables cliniques.
Le second utilise une imputation régularisée missMDA : AFDM pour données mixtes
par défaut, ou ACP standardisée des mesures numériques + ACM des catégories.
Les codes catégoriels ne sont jamais traités comme des mesures continues.
Diagnostic, provenance et identifiants sont exclus de la reconstruction.
Seules les cellules manquantes sont remplacées. Les diagnostics absents sont
exclus plutôt qu'imputés. Deux dimensions par défaut, réglables de 1 à 4 ;
aucun rang optimal n'est revendiqué. L'imputation unique ne quantifie pas son
incertitude. Les deux ACM sont calculées séparément et leurs axes ne sont
pas directement alignés.
Documentation :
https://search.r-project.org/CRAN/refmans/missMDA/html/imputeFAMD.html
https://search.r-project.org/CRAN/refmans/missMDA/html/imputePCA.html
https://search.r-project.org/CRAN/refmans/missMDA/html/imputeMCA.html
Validation : Rscript tests/validation_visualisation.R
