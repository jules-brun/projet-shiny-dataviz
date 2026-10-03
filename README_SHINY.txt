SHINY — HEART DISEASE, VERSION NETTOYÉE

Conserver ui.R, server.R et le dossier R/ ensemble.
Garder dataset/ à la racine avec les quatre fichiers processed fournis.
Depuis une session R ouverte à la racine :
install.packages(c("shiny", "ggplot2", "DT"))
shiny::runApp(".")

NETTOYAGE
- Comparer les 14 variables originales avant tout recodage.
- Conserver la première occurrence de chaque profil dans sa provenance.
- Deux occurrences retirées : une en Hongrie, une à VA Long Beach.
- 920 lignes initiales, 918 après dédoublonnage.
- Zéros de cholesterol et pa_repos transformés en NA : 172 + 1 cellules.
- Les autres zéros ne sont pas transformés. La référence et les trois premiers
  onglets restent sans imputation. Le quatrième est désormais descriptif :
  il n'applique aucun traitement supplémentaire.
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

COMPRENDRE LES DONNÉES MANQUANTES — QUATRIÈME ONGLET

Parcours vertical en cinq étapes : identifier les NA, localiser les absences,
discuter les mécanismes, décrire la sélection sur cas complets et préparer deux
objectifs distincts (associations / prédiction). Un sélecteur de variable pour la
fiche descriptive, complété par la consultation d'une paire dans la matrice.
Les trois premiers onglets sont conservés. Aucune imputation, nouvelle régression,
partition train/test ou suppression supplémentaire n'est réalisée dans le parcours.

FICHIERS ET CALCULS
- ui.R : étapes, questions, fiche A/B/C/D, précisions dépliables, deux cartes
  explicative/prédictive, conclusion provisoire et bibliographie cliquable.
- server.R : référence à diagnostic binaire, diagnostic_initial conservé et
  identifiant stable ; branchement du récit et calcul partagé de la matrice
  exploratoire au démarrage.
- R/comprendre_na.R : calculs purs des bilans, groupes d'absence et populations,
  définitions traduites du fichier UCI et références méthodologiques vérifiées.
- R/serveur_na.R : sorties réactives du récit (tableaux, graphiques, textes calculés).
- R/import.R : importation et nettoyage existants, inchangés.
- R/gestion_na.R et R/modelisation.R : anciennes fonctions conservées ; leur
  moteur de traitement et de sensibilité n'est plus relié au quatrième onglet.
  Le dictionnaire explicite des variables est réutilisé.
- tests/validation_comprendre_na.R : vérifications du récit et des quatre onglets.
- tests/validation_na.R : point d'entrée compatible vers cette nouvelle validation.

Dépendances actives inchangées : shiny, ggplot2, DT, R de base. Aucune installation
automatique au lancement. Le quatrième onglet n'utilise plus d'export ZIP.

Les NA initiaux sont comptés après dédoublonnage, avant recodage ; les nouveaux NA
sont les différences entre les deux états. Les tableaux et graphes réutilisent
na_origine, na_par_provenance, journal_recodage et graphique_na_provenance().
Les comptes de nettoyage portent sur les 13 prédicteurs et le diagnostic.
La fiche porte sur la variable choisie : origine des NA, centre, absence totale,
définition, âge et sexe selon l'indicateur manquante / observée. Les groupes vides
restent explicites. Le nombre d'âges exclus est indiqué. Ces graphiques descriptifs
ne comportent pas de test ; les tests d'association sont présentés séparément.
La dépression ST n'a pas d'unité explicitée dans heart-disease.names : ce point est
signalé, sans inventer une précision. Les codes catégoriels sont documentés.
Les descriptions agrégées d'âge et de sexe peuvent refléter les différences de
composition des centres ; elles ne démontrent pas le mécanisme d'absence.

CAS COMPLETS
Le masque est construit sur les 13 prédicteurs et le diagnostic binaire, comme
dans l'onglet 3. Le tableau donne les effectifs nettoyés/complets/exclus, la part
retenue dans chaque centre et la composition de chacune des deux populations.
Les chiffres de la phrase de synthèse sont calculés : si au moins 95 % des cas
complets viennent d'un centre dans une référence multicentrique, le texte signale
une population presque exclusivement issue de ce centre. Ce seuil descriptif ne
classe pas le mécanisme de manque et ne valide/invalide pas automatiquement l'analyse.
Les proportions sont indéfinies (NA), et non 0 %, lorsqu'une population est vide.

PRUDENCE ET OBJECTIFS
Les faits observés, hypothèses de collecte, décisions de nettoyage déjà prises et
choix futurs sont séparés. MCAR, MAR et MNAR concernent la collecte et peuvent varier
par centre ; aucun badge ne les attribue automatiquement. Les raisons exactes de
non-mesure ne sont pas connues à partir des fichiers processed. L'absence totale
en Suisse du cholestérol après recodage ne prouve pas que la mesure n'a jamais existé.
MAR et MNAR ne sont généralement pas distinguables par les seules données observées.

Comprendre : population et modèle à définir ; associations ajustées sans causalité
automatique ; sensibilité à d'autres stratégies ; imputation multiple possible.
Prédire : population cible, jeu de test à réserver, traitements appris uniquement sur
l'entraînement et réappris dans chaque pli, procédure figée avant le test final.
L'exploration actuelle utilise toutes les données : un test futur ne peut pas être
présenté comme isolé dès l'origine. Le diagnostic observé peut être utile à une
imputation inférentielle ; celui du nouveau patient est inconnu et ne doit pas servir
à imputer ses mesures en prédiction. Aucune procédure de ce type n'est appliquée ici.
L'imputation simple/kNN suivie de tests usuels ignore l'incertitude d'imputation.
Le résultat du cholestérol dans un modèle particulier ne prouve pas un effet nul ;
aucune variable n'est retirée sur cette base. Le récit n'ajoute pas de conclusion GLM.

RÉFÉRENCES MÉTHODOLOGIQUES VÉRIFIÉES
Van Buuren S. (2018). Flexible Imputation of Missing Data, 2e édition.
https://stefvanbuuren.name/fimd/sec-MCAR.html — concepts MCAR/MAR/MNAR.
https://stefvanbuuren.name/fimd/sec-idconcepts.html — limites d'identification.
https://stefvanbuuren.name/fimd/sec-modelform.html — prédicteurs d'imputation.
Van Buuren S., Groothuis-Oudshoorn K. (2011). mice: Multivariate Imputation by
Chained Equations in R. Journal of Statistical Software 45(3), 1–67.
https://doi.org/10.18637/jss.v045.i03
Scikit-learn, documentation officielle :
https://scikit-learn.org/stable/common_pitfalls.html
https://scikit-learn.org/stable/modules/cross_validation.html
Les références UCI et physiologiques du nettoyage sont reprises dans le site.

VALIDATION
Depuis la racine : Rscript tests/validation_comprendre_na.R
Ou : Rscript tests/validation_na.R
Tests R/Shiny : cohérence des comptes entre objets/tableaux/graphes, décomposition
NA initiaux/zéros recodés, zéros valides conservés, toutes les variables, absence
totale ou aucun NA, groupes et populations vides, rendus des quatre onglets,
absence des anciens contrôles, données de référence inchangées et empreintes MD5
identiques pour les quatre fichiers processed avant/après exécution.
Limites : parcours descriptif, mécanisme indéterminé, collecte non reconstituée,
aucune méthode future de traitement ou validation prédictive choisie pour l'utilisateur.

MATRICE EXPLORATOIRE DES ASSOCIATIONS AVEC L'ABSENCE

R/tests_absence.R centralise les fonctions, la boucle et les réglages :
- 3 classes quantitatives, quantiles de type 7 ;
- Fisher simulé : B = 100000 ; graine de base = 20261003 ;
- seuil exploratoire = 0,05 ; ajustement Benjamini-Hochberg (BH).
Les catégories suivent le dictionnaire existant. Lignes : seuls les prédicteurs
avec au moins un NA. Colonnes : les 13 prédicteurs, provenance et diagnostic
binaire. Ni diagnostic_initial, ni identifiants/colonnes techniques ne sont testés.
La diagonale est Non applicable et ne reçoit aucune p-value.

Périmètre : toutes les observations nettoyées des quatre centres, avant toute
imputation. La matrice est calculée une fois au démarrage et partagée entre les
sessions ; choisir une paire ne relance pas les simulations.
Y quantitative : unique(quantile(Y observée, probs = c(0, 1/3, 2/3, 1), type = 7)).
Intervalles fermés à droite ; le premier inclut la borne minimale. Les bornes et
les effectifs, y compris une classe vide éventuelle, sont consultables. Les niveaux
vides sont ensuite retirés pour le test. Moins de deux classes non vides rend le
test impossible ; aucun nouveau seuil ou autre découpage n'est inventé. Les
coupures sont calculées une seule fois pour chaque Y sur le périmètre complet,
partagées entre tous les indicateurs X. Elles ne sont pas recalculées par paire.
Ce découpage est exploratoire, pas clinique ; il peut influencer les résultats.

Chaque tableau croise is.na(X) (0 observée, 1 manquante) avec Y, en excluant seulement
les NA de Y. Aucun cas complet sur les autres variables n'est exigé et les NA de Y
ne sont pas une modalité. Les niveaux inutilisés sont supprimés. Un seul statut
X, moins de deux modalités Y ou aucun Y observé produit une raison de non-calcul.
Les effectifs sont ceux du sous-échantillon où Y est renseignée : la conclusion ne
s'étend pas automatiquement à toute la population.

Effectifs attendus : produit des marges / N. Pearson sans correction de continuité
si minimum attendu >= 1 et proportion d'attendus >= 5 d'au moins 80 %. Sinon Fisher
exact bilatéral pour 2 × 2, Fisher Monte-Carlo pour 2 × K avec K > 2. Toute erreur
reste attachée à sa paire sans interrompre les autres tests. Les avertissements
sont conservés dans le résultat détaillé.
Simulation : graine propre à chaque paire, déterminée par sa position dans les
listes explicites de prédicteurs/colonnes. L'état aléatoire du reste de la session
est restauré après le calcul. La p-value simulée est estimée et sa résolution
minimale est 1/(B+1) ; elle n'est jamais arrondie artificiellement à zéro. Les très
petites p-values non simulées sous la résolution numérique sont affichées <1e-300.
Les valeurs numériques originales restent dans l'objet associations_absence.

BH s'applique uniquement aux p-values valides de TOUTE la matrice courante, pas
séparément par ligne et pas uniquement aux cellules consultées. Les tests en erreur,
non calculables et diagonales en sont exclus. Les couleurs et synthèses utilisent
les p-values ajustées, jamais les valeurs brutes. V de Cramér = sqrt(Pearson non
corrigé / [N × min(r-1,c-1)]), même lorsque la p-value provient de Fisher. C'est une
mesure descriptive d'intensité, pas de causalité ; la statistique et les DDL exposés
sont ceux de Pearson, pas une statistique prétendue de Fisher.

AFFICHAGE ET INTERPRÉTATION
Matrice bleu foncé pour BH < 0,05, bleu clair sinon, gris pour non-calculable ;
diagonales distinguées par la légende et le texte Non applicable. Tableau filtrable
avec méthodes, effectifs, attendus, p-values brutes/ajustées, V et raisons. Consultation
d'une paire : contingence, pourcentages et graphique avec nombres d'observations.
Les synthèses sont descriptives. Une association peut remettre en cause MCAR,
mais son absence ne démontre pas MCAR. Aucune classification MCAR/MAR/MNAR n'est
produite. Ces tests ne distinguent pas MAR de MNAR ; une association avec le centre
ne prouve pas MAR. Les tests ne sont pas ajustés sur les autres caractéristiques ;
une relation avec l'âge peut refléter la composition des centres. Peu d'absences
limite la puissance. Les hypothèses restent à argumenter ; aucun modèle existant
ou traitement de nettoyage n'est modifié.

VÉRIFICATIONS DE LA MATRICE
Rscript tests/validation_tests_absence.R
Contrôles : tableaux manuels, absence totale ou aucun NA, Y constante/entièrement
manquante, quantiles dupliqués, Pearson/Fisher, exclusions propres à Y, simulations
reproductibles à B = 100000, restauration du RNG, erreurs isolées, BH uniquement sur
p-values valides, résultats des cellules cohérents avec les contingences et les
pourcentages, rendus Shiny, sources inchangées. Dépendances inchangées : R/stats,
shiny, ggplot2, DT ; aucune installation supplémentaire.
Références officielles (également accessibles dans la bibliographie du site) :
https://stat.ethz.ch/R-manual/R-devel/library/stats/html/chisq.test.html
https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fisher.test.html
https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html
