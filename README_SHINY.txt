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

MATRICE DES ASSOCIATIONS AVEC L'ABSENCE — QUATRIÈME ONGLET

Le quatrième onglet affiche uniquement la matrice croisée et sa légende.
Les textes narratifs, fiches, tableaux détaillés, sélecteurs et autres graphiques
ont été retirés de l'interface. Les trois premiers onglets restent inchangés.

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

Couleur et chiffres : p-values ajustées BH ; bleu foncé sous 0,05, bleu clair sinon,
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
Dépendances inchangées : shiny, ggplot2, DT et R/stats. Aucune installation automatique.

VALIDATION
Rscript tests/validation_tests_absence.R
Ou Rscript tests/validation_na.R
Contrôles : neuf lignes autorisées, exclusion de la ligne ECG, tableaux manuels,
cas limites, petits effectifs, quantiles dupliqués, NA de Y, simulations reproductibles,
correction BH uniquement sur les tests valides, sources inchangées, rendu de la matrice
et maintien des modèles existants. Les anciens calculs narratifs peuvent être testés
séparément avec Rscript tests/validation_comprendre_na.R.

Documentation des tests :
https://stat.ethz.ch/R-manual/R-devel/library/stats/html/chisq.test.html
https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fisher.test.html
https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html
