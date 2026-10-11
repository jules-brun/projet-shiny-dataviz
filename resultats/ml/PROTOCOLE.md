# Expérience prédictive verrouillée

Les sources UCI restent intactes. Les résultats de cette expérience sont indépendants
des choix interactifs d'imputation/ACM des pages 2 et 3.

- Split reproductible 80/20, stratifié diagnostic × provenance, seed 20261010.
  Allocation par plus grands restes avec au moins une observation de chaque côté
  pour toute strate de taille ≥ 2. Une strate singleton reste dans TRAIN.
- 5 folds, affectation par round-robin dans les strates, débutant dans les folds
  les moins remplis ; une petite strate peut manquer dans un fold, mais les
  classes de diagnostic doivent être présentes partout. Le split n'est pas refait.
- 13 prédicteurs cliniques. Diagnostic, diagnostic original, provenance et
  identifiants sont toujours exclus de l'imputation et des matrices ML.
- Les scores et shortlists de l'ancien benchmark sur toute la cohorte ne sont
  jamais utilisés pour sélectionner. Les familles d'imputation et le protocole
  de masquage sont réutilisés, avec une interface inductive fit/predict.
- Dans chaque analysis fold : 5 simulations de masquage de 10 % des valeurs
  connues par cible, mêmes masques entre méthodes. Toutes les familles applicables
  sont évaluées ; les cinq meilleures valides (toutes simulations réussies) sont
  retenues. Parmi elles : minimum de moyenne + écart-type / racine(n simulations).
  RMSE pour les quantitatives ; taux d'erreur pour les catégories. Les autres
  métriques, les avertissements et les masques sont également sauvegardés.
- Ces 5 simulations donnent une estimation de stabilité limitée, qui est affichée
  explicitement. Aucun réglage de ce nombre n'est autorisé après l'évaluation TEST.
- Chaque imputation cible apprend sur les lignes où elle est observée. Les autres
  prédicteurs manquants sont préparés par médiane/mode appris sur ces lignes ; le
  diagnostic n'aide jamais à imputer. Les prédicteurs restent les données brutes
  du fold, sans utilisation des vraies valeurs masquées ni mise à jour séquentielle.
- Les modèles mean/median/mode, RF, KNN, ACP+régression/classification, AFDM,
  EM et mice sont disponibles selon le type. EM conserve les moments appris.
  Pour mice, les paramètres de préparation et les lignes d'apprentissage sont
  figés ; les lignes à prédire sont ignorées dans les ajustements (`ignore`),
  avec élimination automatique de colonnes désactivée. PMM/classification ne
  voient jamais le diagnostic ML ni une valeur vraie masquée.
- Après imputation : niveaux catégoriels, indicateurs NA des prédicteurs
  quantitatifs, colonnes actives, centrage et réduction appris uniquement sur
  analysis, puis appliqués à assessment. Une modalité inconnue est ramenée au
  mode appris. Aucune sélection de variables supplémentaire.
- Grille de neuf configurations fixée avant TEST : logistique ; elastic net
  (alpha 0/0,5, lambda 0,03) ; RF (mtry 3/6, 400 arbres) ; XGBoost
  (profondeur 2/4, eta 0,05, 120 tours) ; SVM radial (C 0,5/2, gamma 1/p).
- Tous les fits sont limités à analysis. Chaque configuration produit exactement
  une prédiction hors-fold par observation TRAIN. Les hyperparamètres et seuils
  sont comparés uniquement sur ces prédictions de validation de TRAIN : il
  s'agit d'une CV de sélection, pas d'une CV imbriquée donnant une estimation
  indépendante après tuning. Cette limite est indiquée à l'écran ; le TEST
  réservé fournit l'évaluation indépendante du pipeline sélectionné.
- Seuils 0,15 à 0,75 par pas de 0,025. Règle préenregistrée : spécificité moyenne
  ≥ 0,60 et balanced accuracy moyenne ≥ 0,70 ; maximiser ensuite sensibilité,
  puis balanced accuracy, AUC, spécificité. Si aucun couple n'est admissible,
  arrêter sans ouvrir TEST. Les neuf scores par fold sont conservés.
- Sur TRAIN complet : nouveau benchmark d'imputation, choix par variable, fit
  des imputations, préparation et modèle sélectionné. Seuil OOF déjà fixé.
  Le pipeline entier est enregistré avant déverrouillage de TEST.
- TEST est marqué consommé avant toute prédiction finale. Une nouvelle exécution
  retourne le résultat sauvegardé sans fit ni prédiction. Un changement de
  dataset/protocole est rejeté ; une erreur après déverrouillage bloque la reprise
  automatique afin de ne pas favoriser un ajustement guidé par TEST.
- Les métriques, confusion, ROC (orientation fixe), calibration (bins fixes),
  Brier et analyses par provenance utilisent les seules prédictions enregistrées.
  Aucun recalibrage, nouveau modèle, nouveau seuil ou comparaison d'imputation
  n'est effectué sur TEST. Les métriques par centre sont supprimées lorsque
  les classes/effectifs sont trop faibles ; Wilson indique la précision des
  sensibilités/spécificités affichées.

## Fichiers et commandes

`Rscript scripts/entrainer_ml.R` lance/reprend le calcul hors Shiny.
`Rscript tests/validation_ml_unitaire.R` vérifie les moteurs sur données simulées.
`Rscript tests/validation_ml_integration.R` audite les résultats et l'affichage,
sans réévaluer le classifieur sur TEST.

`manifeste.rds` : configuration, seeds, versions, empreinte de données/code,
strates, folds, verrou logique. `partition.rds` : TRAIN et TEST réservés.
`benchmark_fold_*/` : masques, apprentissages, erreurs, avertissements par simulation.
`fold_*.rds` : audit et probabilités assessment. `validation_train.rds` : OOF,
performances par fold, moyennes/écarts-types, recherche de seuils.
`pipeline_fige.rds` : imputations, paramètres de préparation et classifieur entraînés.
`evaluation_finale.rds` : cache utilisé par Shiny. `registre.csv` : ordre et date
UTC de chaque étape. Ne pas supprimer ces fichiers pour essayer un autre modèle
sur le même TEST déjà vu.
