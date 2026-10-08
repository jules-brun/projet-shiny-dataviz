# Projet DataViz et Machine Learning – M2

**Auteurs :** Antonin, Jules, Youri – Institut Agro, M2 Science des données

L'application actuelle utilise `ui.R` et `server.R`, les quatre fichiers UCI
dans `dataset/`, et une interface sombre avec `bslib` et `bsicons`.
Les instructions à jour, la structure et les validations sont décrites dans
[README_SHINY.txt](README_SHINY.txt). La présentation ci-dessous décrit le projet initial.

## Objectif

Explorer les facteurs associés aux maladies cardiaques et construire une application **Shiny** permettant :
- d'explorer et visualiser les données ;
- d'identifier les principaux facteurs de risque ;
- de prédire la probabilité qu'un patient soit atteint d'une maladie cardiaque.

## Source des données

Jeu de données **Heart Failure Prediction Dataset** (fedesoriano, 2021), disponible sur [Kaggle](https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction).

Il regroupe 5 bases de données hospitalières issues de l'UCI Machine Learning Repository : Cleveland, Hongrie, Suisse, Long Beach (VA) et Statlog. Après suppression des doublons, il contient **918 patients** et **12 variables**.

## Variable réponse

| Variable | Description | Modalités |
|---|---|---|
| **HeartDisease** | Présence d'une maladie cardiaque | `1` : malade, `0` : sain |

La cible est **équilibrée** : 508 malades (55 %) et 410 sains (45 %).

## Variables explicatives

### Variables quantitatives

| Variable | Description | Unité / valeurs |
|---|---|---|
| **Age** | Âge du patient | années (28 à 77) |
| **RestingBP** | Pression artérielle au repos, mesurée au début de l'examen. Une valeur élevée indique une hypertension, facteur de risque cardiovasculaire. | mm Hg |
| **Cholesterol** | Taux de cholestérol sanguin. Un excès favorise le dépôt de graisses dans les artères (athérosclérose). | mg/dl |
| **MaxHR** | Fréquence cardiaque maximale atteinte pendant un test d'effort. Un cœur en mauvaise santé atteint généralement une fréquence maximale plus basse. | battements/min (60 à 202) |
| **Oldpeak** | Dépression du segment ST à l'effort par rapport au repos, lue sur l'électrocardiogramme. Plus la valeur est élevée, plus le cœur manque d'oxygène à l'effort (ischémie). | mm (-2.6 à 6.2) |

### Variables qualitatives

| Variable | Description | Modalités |
|---|---|---|
| **Sex** | Sexe du patient | `M` : homme, `F` : femme |
| **ChestPainType** | Type de douleur thoracique ressentie. L'angine de poitrine est une douleur due à un manque d'oxygène du cœur. | `TA` : angine typique, `ATA` : angine atypique, `NAP` : douleur non angineuse, `ASY` : asymptomatique (pas de douleur) |
| **FastingBS** | Glycémie à jeun. Une glycémie élevée peut signaler un diabète, facteur de risque cardiaque. | `1` : > 120 mg/dl, `0` : ≤ 120 mg/dl |
| **RestingECG** | Résultat de l'électrocardiogramme au repos | `Normal` : normal, `ST` : anomalie de l'onde ST-T (inversion de l'onde T et/ou élévation ou dépression du segment ST > 0.05 mV), `LVH` : hypertrophie probable ou certaine du ventricule gauche (critères d'Estes) |
| **ExerciseAngina** | Angine de poitrine déclenchée par l'effort | `Y` : oui, `N` : non |
| **ST_Slope** | Pente du segment ST au pic de l'effort. Une pente plate ou descendante est souvent associée à une maladie cardiaque. | `Up` : montante, `Flat` : plate, `Down` : descendante |

> **Le segment ST, c'est quoi ?** C'est la portion de l'électrocardiogramme entre la contraction et la récupération des ventricules. Normalement plat, il se déforme (sous-décalage, pente anormale) quand le muscle cardiaque manque d'oxygène.

## Qualité des données et prétraitement

- Aucune valeur manquante codée `NA`, **mais des valeurs impossibles à 0** :
  - `Cholesterol = 0` pour **172 patients (18,7 %)** ;
  - `RestingBP = 0` pour **1 patient**.
- Ces 0 correspondent à des **mesures non réalisées**. Une partie des hôpitaux, notamment l'hôpital suisse, ne mesurait pas le cholestérol. Les valeurs manquantes ne sont donc **pas aléatoires** : elles dépendent du lieu de prélèvement. Ces patients sont surtout des hommes (22 % des hommes contre 6 % des femmes), et 88 % d'entre eux sont malades.
- Les 0 sont recodés en `NA`, puis **deux méthodes d'imputation sont comparées** :
  1. remplacement par la **moyenne** ;
  2. imputation par les **k plus proches voisins (KNN, k = 5)**, sans utiliser la variable réponse, pour éviter la fuite d'information.
- Une variable indicatrice `chol_NA` (cholestérol manquant oui/non) est conservée comme indicateur indirect du lieu de prélèvement.
- La population est **déséquilibrée en sexe** : 79 % d'hommes.

## Structure du projet

```
projet-shiny-dataviz/
├── heart.csv              # données brutes
├── app.R                  # application Shiny (assemble les modules)
├── R/                     # modules Shiny (chargés automatiquement)
...

```

## Lancer l'application

```r
install.packages(c("shiny", "bslib", "ggplot2", "DT", "tidyverse", "VIM"))
shiny::runApp()
```
