# Audit sélectif de origin/Jules — 11 octobre 2026

## Références et périmètre

- Branche de travail : `integration-jules`.
- Référence actuelle : `53445c02a0e3c5c5ba46009d7c40d12d954bcbc5`.
- Référence Jules examinée : `origin/Jules`, `8db3e739a357d086a4da78e62adc45107a996317`.
- Référence `origin/main` locale : `6aad9b1ae9903908c0377f91f5a3354be43994ed`.
- Commandes utilisées : `git log --oneline origin/main..origin/Jules`,
  `git diff HEAD...origin/Jules`, comparaison directe `git diff HEAD origin/Jules`
  et lectures ciblées par `git show`.
- Trois commits Jules : `8a1f6c1`, `b04f631`, `8db3e73`.
- Différence depuis l'ancêtre commun : 10 fichiers, 1 158 ajouts, 194 suppressions.
- Les références distantes sont celles déjà disponibles localement ; aucun fetch,
  merge, cherry-pick, commit ou push n'a été exécuté.

L'application actuelle reste la référence : accueil existant, thème CSS,
quatre onglets principaux et modules `parcours_ui` / `parcours_serveur`.
Aucun `theme_heart()` n'a été trouvé dans les fichiers R actuels ; les graphes
emploient notamment `theme_minimal()` et les couleurs bleu/orange existantes.
Aucun fichier de l'application ni donnée brute n'a été modifié pendant l'audit.

## Classement des apports

| Catégorie | Apport de Jules | Décision proposée |
| --- | --- | --- |
| 1. Utile, directement intégrable | Dictionnaire des libellés UCI, `libeller_modalites()`, formatage français des pourcentages | Extraire ces helpers ; conserver codes, NA et codes inconnus ; ne pas changer leur affichage graphique avant validation. |
| 1. Utile, directement intégrable | Palettes diagnostic et centres cohérentes | Réutiliser les couleurs actuelles et une seule constante pour le diagnostic, sans alias `couleurs_diag`. |
| 2. Utile, à adapter | ACM qualitative avec cas complets, imputation simple régularisée et MIMCA | Extraire les calculs dans un module distinct de l'imputation générale des parcours. |
| 2. Utile, à adapter | Diagnostic et provenance supplémentaires ; provenance auxiliaire d'imputation | Préserver la séparation entre variables actives, auxiliaires et supplémentaires. Documenter l'influence indirecte du centre. |
| 2. Utile, à adapter | Choix de `ncp` par validation croisée et cache Shiny | Borner le rang selon les données, contrôler les petits effectifs et conserver les avertissements ; cache identifié par données, variables, seed, paramètres et versions. |
| 2. Utile, à adapter | MIMCA et nuages de positions des modalités | Conserver le plan de référence commun ; valider la projection et distinguer dispersion entre imputations et qualité de représentation. |
| 2. Utile, à adapter | Exclusion par défaut des deux variables très manquantes | Taux calculés dynamiquement ; comparaison avec/sans soumise à validation graphique. |
| 2. Utile, à adapter | Helpers Plotly et tooltips | Conversion sélective seulement pour une interaction utile, après accord explicite. Aucun nouveau thème. |
| 2. Utile, à adapter | Premières analyses et textes calculés sur les sélections | Extraire éventuellement les calculs ; corriger tests fragiles, reproductibilité, vocabulaire de risque et comparaisons multiples. |
| 3. Redondant | Préparation cas complets / AFDM / ACP+ACM dans `R/visualisation.R` | `preparer_parcours()` est inchangé dans Jules ; conserver notre fonction. |
| 3. Redondant | ACM des parcours existants et exclusion du diagnostic des axes | Déjà présentes dans les deux parcours ; ne pas ajouter une page équivalente. |
| 3. Redondant | Calculs des tests d'absence, p-values et correction BH | Les changements de `R/tests_absence.R` concernent le rendu ; conserver notre moteur statistique. |
| 3. Redondant | Import, dédoublonnage, recodage, bilans et explications de préparation | Déjà présents ; extraire au besoin des détails documentaires sans nouvelle page. |
| 4. Non pertinent ou risqué en l'état | Remplacement de `ui.R` / `server.R`, ajout d'une présentation et d'un cinquième onglet ACM | Ne pas reprendre ces fichiers en bloc ni changer la navigation. |
| 4. Non pertinent ou risqué en l'état | Conversion globale ggplot/VIM vers Plotly | Ne pas reprendre automatiquement ; dépendance et comportement graphique supplémentaires. |
| 4. Non pertinent ou risqué en l'état | Remplacement des barres de modalités et du graphique VIM | Change la question statistique ou tronque les combinaisons ; validation séparée indispensable. |
| 4. Non pertinent ou risqué en l'état | Vue individus interprétée comme incertitude multiple | Coordonnées de référence uniquement ; aucune dispersion individuelle calculée. |
| 4. Non pertinent ou risqué en l'état | Textes fixes et formulations trop fortes | Corriger avant toute reprise, voir ci-dessous. |

## Points scientifiques et techniques à traiter

1. **Rôle du diagnostic.** Dans `calculer_acm()`, Jules l'exclut bien des entrées
   de `estim_ncpMCA`, `imputeMCA` et `MIMCA`, et le déclare supplémentaire dans
   l'ACM finale. Cette séparation doit être testée lors de l'intégration.
2. **Rôle de la provenance.** Elle participe à l'imputation, puis est
   supplémentaire dans l'ACM finale. Elle influence donc indirectement les
   données complétées et les axes. Ne pas modifier `preparer_parcours()` pour
   appliquer silencieusement cette règle aux autres analyses.
3. **Modélisation du diagnostic.** L'ACM exploratoire est distincte du Machine
   Learning. Le module `R/modelisation.R` existe mais n'est pas chargé par le
   serveur actuel ; ses prédicteurs doivent rester limités au dictionnaire
   clinique, sans provenance. Les helpers d'ACM ne doivent pas alimenter sa
   liste de prédicteurs. La provenance auxiliaire du benchmark d'imputation du
   notebook ne constitue pas non plus un prédicteur du diagnostic.
4. **Modalités et individus.** MIMCA produit des projections par jeu imputé et
   des barycentres par modalité. En revanche, `individus` est toujours construit
   avec `ref$ind$coord`. Le test isolé confirme leur identité exacte. Les croix
   indiquent seulement la présence de valeurs imputées, pas leur incertitude.
5. **Ellipses.** `stat_ellipse(level = .95)` ajuste des ellipses aux nuages des
   positions ; cette instruction ne justifie ni un IC à 95 % du centre ni une
   couverture empirique exacte de 95 % des positions. Retenir « ellipses de
   variabilité liée aux imputations multiples ». Petite ellipse : faible
   sensibilité de la position à l'imputation ; grande ellipse : forte sensibilité.
   Gérer les nuages dégénérés et les modalités peu représentées sans masquer
   silencieusement les avertissements.
6. **Nombre d'imputations.** Le wrapper Jules utilise 20 jeux par défaut ; prévoir
   20 en développement et 100 en configuration finale, avec paramètre explicite,
   cache des résultats lourds et possibilité de précalcul RDS. Le cache Jules
   utilise seulement le mode et l'inclusion des variables lourdes, insuffisant
   pour un cache persistant partagé entre versions de données/protocole.
7. **Taux de NA.** Sur la référence actuelle après nettoyage : 66,340 % pour
   `nb_vaisseaux`, 52,723 % pour `test_thallium`, sur 918 lignes. Ne pas conserver
   les textes fixes « plus de 50 % », ni les autres effectifs et proportions
   écrits en dur dans la présentation de Jules.
8. **Proximité au diagnostic.** Remplacer « modalités les plus proches des
   malades » par « Modalités graphiquement les plus proches de la modalité
   'Présence' sur le plan factoriel affiché. Cette proximité est descriptive
   et ne constitue pas un test d'association. » Lire cette proximité avec les
   cos² et la part d'inertie représentée.
9. **Robustesse ACM.** Contrôler variables constantes/entièrement NA, diagnostic
   ou centre à modalité unique, effectifs insuffisants, rang admissible,
   alignement des individus et des niveaux entre imputations. Réutiliser
   les identifiants source stables de la référence actuelle.
10. **Helpers.** Jules propose deux fonctions de libellé : `libeller_modalites()`
    conserve les codes inconnus, alors que `etiqueter()` les transforme en NA.
    Un test sur le code 99 confirme cette différence. Conserver une seule
    fonction robuste. Éviter les noms globaux génériques (`nb`, `section`,
    `barycentres`) et utiliser les namespaces Shiny existants.
11. **Analyses préliminaires.** Les Fisher simulés ne fixent pas de seed locale,
    les tests exploratoires ne sont pas corrigés pour multiplicité et les
    coupures en quintiles peuvent dégénérer avec des valeurs répétées. Les
    odds ratios univariés n'estiment pas directement des risques ; remplacer
    « le risque augmente » par une association descriptive. Prévoir des
    contrôles pour groupes uniques, valeurs constantes et séparation logistique.
12. **Interprétations figées.** La phrase sur les patients asymptomatiques et
    « les plus à risque » peut contredire une sélection filtrée ; elle doit
    dépendre des résultats et rester descriptive.
13. **Questions statistiques différentes.** Les barres actuelles représentent
    les modalités dans chaque groupe de diagnostic ; Jules représente la
    proportion de diagnostics positifs dans chaque modalité. Ce changement de
    dénominateur mérite une validation, pas une simple conversion technique.
14. **Combinaisons de NA.** Jules remplace VIM par les 12 motifs les plus fréquents.
    Un résumé précise leur couverture, mais la vue ne montre plus tous les
    motifs. Ne pas remplacer le graphique actuel automatiquement.

## Changements graphiques proposés — aucun n'est encore intégré

| ID | Proposition | Emplacement conservé | Apport et condition |
| --- | --- | --- | --- |
| G1 — priorité | Enrichir l'ACM existante avec modes cas complets/simple/MIMCA, projection du diagnostic et ellipses de variabilité des modalités ; option avec/sans les deux variables très manquantes | Bloc ACM de l'onglet 3, dans les parcours existants ; aucun nouvel onglet | Rend visible la sensibilité à l'imputation. Accord nécessaire sur le graphe et ses contrôles. Vue individus de référence explicitement identifiée, sans prétendre afficher son incertitude. |
| G2 — option | Rendre interactive la matrice des tests d'absence | Onglet 4, même emplacement | Survol : effectif, test, p brute/ajustée, V de Cramér et cause de non-calcul. Conserver les calculs, catégories et thème actuels. |
| G3 — option | Rendre interactif le nuage de deux mesures | Onglet 3, bloc « Relations entre deux mesures » | Survol des valeurs et du statut observé/imputé, activation des groupes par légende ; intérêt réel sur les points superposés. |
| G4 — option, modification analytique | Vue de la proportion de diagnostics positifs par modalité | Onglet 3, bloc des catégories, uniquement si cette évolution est approuvée | Répond à une autre question que les barres actuelles ; conserver leur vue tant que le remplacement ou une vue alternative n'est pas approuvé. |

Non proposés pour l'intégration initiale : nouveau graphe d'incertitude
individuelle, nouvelle page Présentation et ses deux graphes, pyramide âge/sexe,
violons, courbes de tendance, nouveaux graphes diagnostic/centre, nouveaux filtres
âge/sexe, remplacement VIM par les 12 motifs, conversion générale des heatmaps,
boxplots, distributions et ACM vers Plotly. Leur intégration demanderait un
accord distinct ; les fonctions analytiques utiles peuvent être étudiées seules.

## Fichiers proposés pour une intégration validée

- `R/graphiques.R` (nouveau) : helpers de libellé/formatage et palette commune,
  sans import Plotly systématique et sans nouveau thème ; respecter le
  dictionnaire actuel plutôt que créer un second système de codes.
- `R/acm_imputation.R` (nouveau, adapté) : calculs purs, configuration,
  validation des entrées, cache et résultats d'ACM ; séparation entre calculs
  et affichage. Pas de copie brute des globals/UI de Jules.
- `R/visualisation.R` : branchement local du bloc ACM si G1 est accepté ; conserver
  les autres sorties, les IDs des modules et `preparer_parcours()`.
- `server.R` : ajouts ciblés de chargement/initialisation du cache seulement.
- `ui.R` : uniquement si une connexion ciblée est indispensable ; aucun
  remplacement, nouvel onglet ou changement de navigation.
- `R/tests_absence.R` : rendu uniquement si G2 est accepté ; moteur statistique
  préservé.
- Tests ACM nouveaux et adaptation ciblée de `tests/validation_visualisation.R`
  et `tests/validation_comprendre_na.R` après intégration ; documentation des
  dépendances dans `README_SHINY.txt` si nécessaire.
- `R/import.R`, données brutes, `R/benchmark_imputation.R`,
  `exploration_na.Rmd` et `R/modelisation.R` restent hors de cette intégration.
- `R/presentation.R` et `R/premieres_visus.R` ne sont pas copiés en bloc.

## Vérifications effectuées avant intégration

- `Rscript tests/validation_visualisation.R` : réussi (cas complets, deux
  imputations, conservation des valeurs observées/modalités, graphes et ACM).
- `Rscript tests/validation_comprendre_na.R` : réussi (sources conservées,
  cas limites, chargement UI/serveur et quatre onglets).
- Démarrage HTTP local de Shiny et réponse HTML : réussis ; présence des quatre
  onglets principaux et des sous-onglets Cas complets / Imputation par
  composantes. Serveur temporaire arrêté après le test. Pas de test manuel
  dans un navigateur pendant cet audit.
- Code ACM Jules exécuté isolément dans un environnement R, hors application :
  cas complets avec six variables actives, 541 lignes ; simple/multiple,
  918 lignes, rang sélectionné 4. Test multiple réduit à 3 jeux pour l'audit,
  sans valider la stabilité des ellipses. Nuages des modalités produits ;
  coordonnées individuelles identiques à la référence.

Ce rapport est le seul fichier ajouté. L'intégration et les tests de
non-régression correspondants restent à faire après validation du périmètre,
notamment des changements graphiques G1–G4.

## Suite de l'audit : G1 validé et intégré

L'utilisateur a validé G1 seul. Le bloc ACM de l'onglet 3 a été enrichi dans les
modules existants, sans nouvel onglet. Les modes cas complets/simple/MIMCA,
l'option avec/sans les deux variables très manquantes, les taux de NA dynamiques,
les projections du diagnostic et les ellipses de variabilité des modalités sont
intégrés. Les positions et cos² affichés demeurent ceux de l'ACM de référence ;
les ellipses sont calculées à partir des projections des jeux MIMCA sur ce plan.
La vue individus reste explicitement une vue de référence.

Fichiers de cette intégration :
- `R/acm_imputation.R` : nouveau moteur ACM, projection, contours descriptifs,
  fonctions de rendu ggplot et caches mémoire/RDS.
- `R/visualisation.R` : chargement du moteur et adaptation du seul bloc ACM,
  avec IDs des deux graphes conservés ; calcul appliqué par bouton.
- `tests/validation_acm_imputation.R` : nouvelle validation scientifique,
  sauvegarde/reprise du cache, contrôles Shiny et conservation de navigation.
- `README_SHINY.txt` : documentation du protocole et des options de cache.
- Ce rapport : traçabilité de l'audit et du périmètre accepté.

`ui.R`, `server.R`, `preparer_parcours()`, le moteur des tests d'absence,
le Machine Learning, le notebook et les sources de données restent inchangés.
Les apports G2, G3, G4, la présentation Jules et ses autres nouveaux graphes
n'ont pas été repris. Aucun merge, cherry-pick, commit ou push.

Vérifications après intégration :
- Validation spécifique ACM réussie : diagnostic sans influence sur imputation
  et axes, provenance auxiliaire/supplémentaire, conservation des observations,
  modes simple/multiple, inclusion des deux variables, centre unique et cas
  constants, maintien du RNG, cache mémoire/RDS et application des réglages
  uniquement au clic.
- Trois suites existantes réussies : `validation_visualisation.R`,
  `validation_comprendre_na.R`, `validation_tests_absence.R`.
- Rendu MIMCA avec 20 jeux vérifié visuellement : légendes lisibles,
  diagnostic distingué, ellipses et cos² affichés.
- Démarrage HTTP et réponse HTML vérifiés ; quatre onglets et deux parcours
  conservés, contrôles ACM présents. Serveur temporaire arrêté.
- `git diff --check` réussi. Les 100 jeux proposés pour l'analyse finale restent
  une configuration disponible, pas une démonstration de stabilité statistique.

## Suite : G2, G3 et G4 validés et intégrés

L'utilisateur a également validé G2, G3 et G4. Intégration ciblée :
- G2 : `absence_matrice` passe à Plotly au même emplacement dans l'onglet 4.
  Statuts/couleurs, correction BH et moteur statistique conservés. Survol : test,
  p brute/ajustée, V de Cramér, effectifs, exclusions, raison et avertissement.
  La note générale reste visible hors des tooltips.
- G3 : `relations` passe à Plotly dans les deux parcours du troisième onglet.
  Les sélecteurs des deux mesures et de la couleur sont conservés. Les
  infobulles indiquent la valeur et le statut de chaque mesure, le diagnostic
  et le centre. Les groupes peuvent être masqués/réaffichés par la légende.
- G4 : un sélecteur dans le bloc des catégories donne accès à la proportion
  de diagnostics positifs dans chaque modalité, avec moyenne du parcours,
  numérateur/dénominateur et libellés UCI. La vue initiale de distribution des
  modalités dans chaque groupe de diagnostic reste accessible. Aucun test
  supplémentaire ou vocabulaire causal n'est ajouté.

`R/graphiques.R` fournit les palettes, libellés et fonctions Plotly partagés ;
il réutilise le dictionnaire de modalités défini pour l'ACM, conserve les codes
inconnus et ne remplace pas le thème. Seuls les graphes G2/G3 et la nouvelle vue
G4 utilisent Plotly. Les ACM, corrélations, distributions, NA et effectifs
conservent leur rendu actuel. La modification de `server.R` est limitée à la
liste des dépendances dans son commentaire de démarrage.

Les tests `validation_graphiques_interactifs.R`, `validation_visualisation.R`,
`validation_tests_absence.R`, `validation_comprendre_na.R` et
`validation_acm_imputation.R` passent. Les tests vérifient les dénominateurs
contre des données manuelles, les informations au survol, les deux vues,
les rôles scientifiques ACM, la conservation des calculs et les empreintes
sources. Démarrage HTTP vérifié et quatre onglets conservés ; serveur temporaire
arrêté. Pas de validation manuelle des interactions dans un navigateur.

La présentation et les autres nouveaux graphiques de Jules ne sont toujours
pas intégrés. Aucun merge, cherry-pick, commit ou push.
