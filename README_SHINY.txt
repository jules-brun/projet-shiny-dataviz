SHINY — HEART DISEASE : QUATRE PAGES

Lancer depuis la racine du projet :
install.packages(c("shiny", "bslib", "bsicons", "ggplot2", "FactoMineR", "ggrepel",
                   "ranger", "FNN", "mice", "nnet", "missMDA", "digest"))
shiny::runApp(".")

app.R assemble ui.R et server.R. Conserver R/_disable_autoload.R : les scripts
R/ sont chargés explicitement. Les anciens modules restent disponibles sur
le disque, mais leurs pages et sorties ne sont plus chargées par l'application.
Le notebook exploration_statistique.Rmd reste indépendant.

1. DONNÉES
Titre et tracé cardiaque communs, sans ancien bandeau éditorial ni accueil.
Diagnostic binaire : code original 0 -> 0, codes 1–4 -> 1. Dans les graphiques,
la cible garde les libellés 0 et 1 ; les valeurs manquantes restent NA.
Effectifs des quatre centres avant nettoyage, note sur les deux doublons,
distribution sélectionnable (cholestérol par défaut), justification du recodage,
barres observé/NA par variable et centre ; heatmap des pourcentages de NA.
Les quatre tables sont empilées, sans jointure sur les mesures des patients.

Nettoyage dans R/import.R : 920 lignes initiales, 918 conservées ; une répétition
retirée en Hongrie, une à VA Long Beach. Comparaison des 14 valeurs originales
au sein de chaque centre, avant recodage. Les profils identiques ne prouvent
pas une identité de patient. ? -> NA ; cholesterol=0 et pa_repos=0 -> NA
(172 et 1 cellules). Les autres zéros et les fichiers sources sont conservés.
Les distributions utilisent les données dédoublonnées AVANT recodage des zéros :
les ? sont NA et exclus, les 172 zéros de cholestérol et le zéro de pression
restent visibles et sont distingués en ambre. Leur proportion est indiquée.
Les graphiques NA utilisent ensuite les données avec les zéros recodés.
Pour la variable sélectionnée, chaque barre empile les valeurs observées (bleu
clair) et les NA (bleu foncé), avec les effectifs réels par centre.
Observé + NA égale l’effectif nettoyé du centre, tous diagnostics réunis.

2. GESTION DES NA
Deux blocs pédagogiques en tête de page, définis dans R/pedagogie_na.R :
- bilan de la suppression des cas incomplets sur les 14 variables cliniques
  (provenance, copie du diagnostic original et identifiants exclus) : trois
  value boxes, barre horizontale de toutes les cellules et phrase automatique ;
- schéma ggplot2 du benchmark, avec les méthodes réellement configurées et
  le même masque pour chaque méthode au sein d'une simulation.
Les champs pct_* du bilan sont des proportions [0, 1], converties en % à l'écran.
L'information observée est un comptage de cellules, pas une mesure pondérée
par l'importance des variables. Les ratios sans dénominateur sont NA.
Les pourcentages de la barre concernent toutes les cellules ; la troisième
value box utilise seulement les cellules observées comme dénominateur.
Validation ciblée : Rscript tests/validation_pedagogie_na.R
Boxplots du benchmark sauvegardé dans resultats/benchmark_imputation/ :
RMSE pour les quantitatives ; taux d'erreur pour les catégories, y compris
les variables ordinales et discrètes. Le classement ne mélange pas ces unités.
Sélecteur par variable ayant des NA, présélection du gagnant sauvegardé ;
en cas d'égalité, premier gagnant dans meilleurs_modeles.csv.
Seuls les modèles ayant réussi toutes les répétitions sont proposés.
Bouton Calculer l'imputation : lance le calcul des jeux et de l'ACM.
Toute modification des choix demande un nouveau clic ; une note indique
quand les graphiques correspondent encore à l'ancien calcul. Une erreur
conserve le dernier résultat réussi et affiche son motif.

R/benchmark_imputation.R fournit les modèles ; R/pipeline_application.R réutilise
la configuration et les résultats. Le diagnostic est exclu des prédicteurs ;
la provenance est incluse comme dans le benchmark. Chaque cible est ajustée sur
ses valeurs observées. Les autres NA des prédicteurs sont préparés comme lors
du benchmark, sans actualisation séquentielle des prédicteurs. Aucun benchmark
n'est relancé au démarrage de l'application.

Jeux multiples : réajustement des modèles choisis sur des échantillons bootstrap
(stratifiés sur les classes des cibles catégorielles). Graine fixe 20261010 ;
20 jeux par défaut, de 5 à 100. Si mice est choisi, un seul tirage mice est
utilisé par jeu bootstrap. Les valeurs observées, la cible et la provenance
sont préservées. Cette approche décrit une variabilité de réajustement ; elle
ne garantit pas une imputation multiple probabiliste complète et ne mesure
pas toute l'incertitude prédictive. Les modèles déterministes peuvent produire
des jeux identiques, donc aucune ellipse visible.

3. ACM
Architecture noire conservée : mêmes graphiques des valeurs propres, modalités
et individus ; mêmes sélecteurs des dimensions, sans nouvelle visualisation.
Les huit variables qualitatives cliniques sont actives par défaut : sexe,
type_doul_thor, glyc_jeun_elevee, ecg_repos, angine_effort, pente_st,
nb_vaisseaux et test_thallium. Une case permet d’exclure les deux dernières ;
leurs taux de NA sont calculés sur les données nettoyées actuelles.

Quatre possibilités, appliquées au clic sur Calculer l’ACM :
- modèles choisis sur Gestion des NA (comportement historique, sélection initiale) ;
- cas complets sur les seules variables actives de cette ACM ;
- imputation simple par ACM régularisée avec missMDA::imputeMCA ;
- imputation multiple avec missMDA::MIMCA.
Les stratégies factorielles partent des données nettoyées avant imputation,
indépendamment du résultat de la page Gestion des NA. Leur rang d’imputation
est choisi par estim_ncpMCA (10 simulations, maximum 5 dimensions admissibles).
Diagnostic : exclu de l’imputation et de la construction des axes, supplémentaire.
Provenance : auxiliaire de l’imputation s’il y a plusieurs centres, puis
supplémentaire dans l’ACM. Elle influence indirectement les axes via les cellules
reconstruites, sans participer directement à leur construction. Le diagnostic
manquant n’entraîne pas une exclusion des lignes pour l’imputation exploratoire.
Cette règle ne change pas le Machine learning : provenance jamais prédicteur.

MIMCA : 20 jeux en développement, 50 ou 100 pour l’analyse finale. Réglage initial
modifiable avant le lancement : options(heart.acm.nboot = 100L).
Chaque jeu est projeté sur la même ACM de référence, sur tous les axes disponibles.
Les modalités sont projetées par leurs barycentres divisés par la racine de la
valeur propre. Les points des modalités sont les positions moyennes des jeux
projetés ; les ellipses sont centrées sur ces mêmes points et décrivent la
dispersion des projections multiples, y compris pour MIMCA. Le texte de proximité
au diagnostic utilise ces mêmes positions affichées.
La vue individus de ces stratégies reste celle de référence, sans ellipse
individuelle. Les positions moyennes et ellipses individuelles historiques des
modèles choisis restent disponibles car leurs projections sont réellement calculées.

Ellipses de variabilité liée aux imputations multiples : petite ellipse = faible
sensibilité de la position ; grande ellipse = plus forte dépendance à l’imputation.
Contours descriptifs de covariance avec rayon gaussien théorique à 95 % ; aucune
garantie de couverture empirique et aucun intervalle de confiance du centre.
Chaque contour partage la couleur de son point : variable pour les modalités,
diagnostic pour les individus. Les jeux, le masque de NA et les variables
supplémentaires sont contrôlés pour éviter les désalignements de lignes.
La limite individuelle est appliquée après exclusion
des positions invariantes, avec les dispersions les plus fortes en priorité.
Un compteur indique les contours disponibles et affichés sur le plan choisi.
Une covariance de rang un produit un segment ; les points invariants n’ont pas
d’ellipse. Si une autre stratégie est encore affichée, appliquer les modèles
choisis avec Calculer l’ACM. Les proximités au diagnostic 1 sont
calculées sur le plan affiché, descriptives et ne constituent pas des tests
d’association. Les plans des stratégies sont distincts, sans alignement entre eux.
Les libellés UCI sont centralisés dans R/graphiques.R ; diagnostic reste 0/1.
Le thème de référence est toujours theme_heart() dans R/design.R.

Cache mémoire partagé entre sessions et cache RDS dans resultats/cache_acm/,
identifié par les données, paramètres, code et versions des packages.
Les réglages modifiés demandent un nouveau clic et une erreur conserve le dernier
résultat réussi. Aucun MIMCA n’est lancé lors du rafraîchissement des graphiques.
Pour désactiver le cache disque : options(heart.acm.cache_dir = NULL).
Pour les précalculs, appeler acm_resultat_cache() avec les mêmes données/réglages.
Seed 20261011 ; état aléatoire restauré après les calculs exploratoires.

Validation : Rscript tests/validation_acm_imputation.R
             Rscript tests/validation_design.R
             Rscript tests/validation_pipeline_application.R

4. MACHINE LEARNING
Page d'évaluation chargée depuis resultats/ml/evaluation_finale.rds, sans calcul
lourd dans Shiny. Graphique du protocole, performances CV des cinq familles,
pipeline retenu et seuil, métriques TEST, confusion, ROC et centres.
Imputation et prédiction n'utilisent jamais la provenance. Les résultats
historiques sur toute la cohorte ne choisissent pas les méthodes de cette page.
Calcul hors app : Rscript scripts/entrainer_ml.R (reprise/cache, TEST unique).
Protocole complet et limites de la CV de sélection : resultats/ml/PROTOCOLE.md.
Dépendances supplémentaires : glmnet, ranger, xgboost, e1071, pROC, digest,
FNN, mice, FactoMineR et nnet. Pas de tidymodels requis : fit/predict inductifs
explicites assurent la séparation apprentissage/prédiction.
Validation : Rscript tests/validation_ml_unitaire.R puis
Rscript tests/validation_ml_integration.R.

VALIDATION DE LA NOUVELLE APPLICATION
Rscript tests/validation_pipeline_application.R
Vérifie les trois pages, l'import, les vrais modèles par défaut avec cinq jeux,
les valeurs observées, les sources, la provenance et le diagnostic supplémentaires,
les projections sur plusieurs axes, les ellipses et les sorties réactives.
Les anciennes validations de pages concernent les anciennes interfaces.

BIBLIOGRAPHIE DU NETTOYAGE
Janosi A., Steinbrunn W., Pfisterer M., Detrano R. (1989).
Heart Disease. UCI Machine Learning Repository. DOI 10.24432/C52P4X ; CC BY 4.0.
https://doi.org/10.24432/C52P4X
National Library of Medicine, MedlinePlus : Cholesterol.
https://medlineplus.gov/cholesterol.html
National Heart, Lung, and Blood Institute : Low Blood Pressure.
https://www.nhlbi.nih.gov/health/low-blood-pressure
Références déjà présentes dans le projet, consultées le 3 octobre 2026.
Elles motivent le recodage analytique ; elles ne prouvent pas que zéro était
un code officiel de valeur manquante UCI.
