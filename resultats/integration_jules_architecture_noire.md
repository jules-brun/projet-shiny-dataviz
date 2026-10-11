# Intégration sélective de Jules — 11 octobre 2026

## Référence et conservation

Travail sur la branche `Antonin`, sans checkout, merge, cherry-pick, commit ou push.
L'utilisateur a confirmé que l'architecture noire se trouve sur `main` et doit
être prioritaire. Sa référence locale est `2341b66` ; après fetch, aucun changement
des fichiers d'application n'a été trouvé entre cette référence et `origin/main`
(`9ca02d2`). Les fichiers noirs actifs et leurs dépendances ont donc été remis dans
le working tree d'Antonin, qui contenait encore l'ancienne interface claire.

Les modifications locales antérieures et les fichiers remplacés sont sauvegardés
sous `/tmp/heart-avant-integration-noire-20261011/`. Le dossier inclut l'état Git
initial et la liste des fichiers de référence repris. Les modules anciens hors
navigation restent sur disque. Le notebook indépendant et le script d'entraînement
hors Shiny de la référence noire sont conservés.

Les quatre pages restent, dans l'ordre : Données, Gestion des NA, ACM,
Machine learning. Les graphiques existants conservent leurs identifiants.
Aucun nouveau graphique, aucune nouvelle page et aucune conversion Plotly.
`R/design.R`, `www/style.css`, les modules ML et leurs résultats sauvegardés,
le benchmark et le module pédagogique sont identiques à la référence noire.

## Apports examinés

Les trois derniers commits locaux de `origin/Jules` :
- `8a1f6c1` : helpers et graphiques interactifs ;
- `b04f631` : présentation, analyses préliminaires et ACM ;
- `8db3e73` : titres Plotly séparés des légendes.

Lecture sélective de `R/acm_imputation.R` et `R/graphiques.R`, sans copie de leurs
fichiers en bloc, de leur UI, de leur serveur ou de leur navigation.

## Adaptations à la référence noire

- `R/acm_imputation.R` : `calculer_acm_conditionnelle()`, `acm_resultat_cache()`,
  `acm_avec_graine()` et `acm_texte_resultat()`. Principes de Jules : cas complets,
  imputeMCA régularisée, MIMCA, provenance auxiliaire et projection commune.
  Diagnostic jamais utilisé pour reconstruire les variables ni construire les axes.
  Contrôles des constantes, rang admissible, modalités et alignement des lignes ;
  observations originales conservées ; avertissements visibles et seed restaurée.
- `R/pipeline_application.R` : moteur `calculer_acm_app()` réutilisé par les quatre
  modes ; projection sur tous les axes, tableau à variables sélectionnables, vue
  individuelle de référence pour les stratégies factorielles. Les projections
  individuelles multiples du mode historique restent disponibles.
  `ellipses_app()` et `graphe_acm_app()` réutilisés, sans nouveau graphe.
- `R/graphiques.R` : dictionnaire UCI, `libeller_modalites()`, `pct_fr()` et
  `libeller_points_acm()` ; conservation des NA et codes inconnus. Diagnostic 0/1
  conservé conformément au choix d'affichage de l'application noire.
- `ui.R` et `server.R` : ajouts limités au bloc ACM (stratégie, variables très
  manquantes, taux calculés, nombre MIMCA, bouton, statut et texte descriptif).
- `.gitignore` : cache ACM recalculable exclu du suivi.
- `README_SHINY.txt` et tests ACM/design : documentation et vérifications de la
  nouvelle intégration dans les quatre pages noires.

Les jeux MIMCA sont projetés sur une ACM de référence. Les ellipses représentent
une dispersion descriptive entre imputations, pas un IC du centre. Les individus
MIMCA affichent uniquement la référence. La proximité à diagnostic 1 est calculée
sur les dimensions effectivement affichées et n'est pas un test d'association.
Les plans des différentes stratégies ne sont pas directement alignés entre eux.

Nombre MIMCA : 20/50/100, 20 initialement et option `heart.acm.nboot` pour le final.
Calcul exclusivement au clic, cache partagé en mémoire et RDS optionnel/persistant.
La règle ML sans provenance reste inchangée ; aucun réentraînement ou réaccès
prédictif au TEST final n'a été réalisé.

## Éléments volontairement non repris

Présentation et analyses préliminaires de Jules ; architecture, navigation,
UI/server globaux et theme_app ; conversions Plotly ; nouveaux graphiques ;
textes fixes sur les taux de NA ; diagnostic dans l'imputation ; interprétation
inférentielle des proximités ou des ellipses ; incertitude individuelle MIMCA
non calculée. Le correctif des titres Plotly n'a pas d'application ici, puisque
les vues ACM existantes restent en ggplot2.

## Validation

- Tests ACM : trois stratégies réelles sur un échantillon des quatre centres,
  imputation indépendante du diagnostic, valeurs observées, variables optionnelles,
  axes, projections, absence d'ellipse individuelle de référence, cache et bouton.
- Tests pipeline : modèles gagnants réels, cinq jeux bootstrap, observations
  conservées, graphiques existants et parcours Shiny.
- Tests design : pages/ordre, thème sombre, aucun nouveau plotOutput, objet Shiny.
- Tests pédagogie NA et intégration ML sauvegardée : régressions contrôlées.
- Démarrage Shiny sur localhost vérifié, puis arrêté volontairement après huit
  secondes. L'ouverture du port nécessite une exécution hors sandbox.
- Rendu du graphique MIMCA des modalités inspecté sur fond sombre.
