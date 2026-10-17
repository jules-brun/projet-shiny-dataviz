# Heart disease : le projet en visualisations

Ce dossier résume notre projet Shiny en quelques graphes statiques destinés à un public de
médecins et de chercheurs en cardiologie. Le document `graphes_visu-heart.Rmd` présente la
démarche, de la version technique de chaque graphe jusqu'à sa version finale.

## Lancer le document en 3 étapes

1. **Décompresser** `Visu.zip`, n'importe où sur l'ordinateur.
2. **Installer les packages**, une seule fois, dans la console R :
   ```r
   install.packages(c("ggplot2", "ggrepel", "rmarkdown", "knitr"))
   ```
3. **Ouvrir** `graphes_visu-heart.Rmd` dans RStudio et cliquer sur **Knit**.

Le document HTML s'ouvre au bout d'environ 20 secondes. Aucun calcul lourd n'est relancé.

### Sans RStudio

Dans une console R lancée **depuis le dossier `Visu`** :

```r
rmarkdown::render("graphes_visu-heart.Rmd")
browseURL("graphes_visu-heart.html")
```

Il faut alors que [pandoc](https://pandoc.org/installing.html) soit installé (il est fourni avec
RStudio).

## Prérequis

| Outil | Rôle | Version testée |
|---|---|---|
| R | | 4.6.1 (R ≥ 4.1 requis par `ggplot2` et `ggrepel`) |
| `ggplot2` | tous les graphes | 4.0.3 |
| `ggrepel` | étiquettes sans chevauchement (AFDM) | 0.9.8 |
| `rmarkdown`, `knitr` | génération du document HTML | 2.32, 1.52 |
| pandoc | conversion en HTML (fourni avec RStudio) | 3.11 |

Aucun autre package n'est nécessaire : l'importation et le prétraitement sont écrits en R de base.

## Naviguer dans le document

- La **table des matières** à gauche suit les quatre parties du projet : données, gestion des
  valeurs manquantes, lien avec le diagnostic, prédiction.
- L'encadré **« Graphes finaux »**, en haut du document, mène directement aux graphes retenus.
  Sous chacun d'eux, un lien permet d'y revenir.

## Contenu du dossier

```
Visu/
├── README.md                       ce fichier
├── graphes_visu-heart.Rmd          le document à ouvrir
├── import_pretraitement.R          importation et prétraitement, lancé par le Rmd
├── dataset/
│   ├── processed.cleveland.data    données brutes UCI, une par hôpital
│   ├── processed.hungarian.data
│   ├── processed.switzerland.data
│   ├── processed.va.data
│   ├── heart-disease.names         documentation UCI des variables
│   ├── heart_imputations.rds       données imputées (précalculées)
│   └── heart_afdm.rds              résultats de l'AFDM (précalculés)
└── resultats/
    └── evaluation_finale.rds       prédictions du modèle final sur le jeu TEST (précalculées)
```

### Prétraitement (`import_pretraitement.R`)

Le Rmd exécute ce script dès son premier bloc de code. Le script :

- importe et fusionne les fichiers des 4 hôpitaux, avec `?` lu comme valeur manquante ;
- retire les lignes en double au sein d'un même hôpital (918 patients au final) ;
- remplace par une valeur manquante les pressions artérielles et cholestérols égaux à 0 ;
- rend le diagnostic binaire : 0 = pas de maladie, 1 = maladie (codes 1 à 4).

### Résultats précalculés

Les imputations, l'AFDM et l'évaluation du modèle ont été produites par notre application Shiny
(imputation par Random Forest, EM et AFDM, avec 20 imputations multiples ; AFDM avec FactoMineR ;
modèle XGBoost). Ces calculs sont longs et demandent des packages supplémentaires. Le document
lit donc directement leurs résultats, enregistrés dans les fichiers `.rds`, pour rester rapide et
ne dépendre que de `ggplot2` et `ggrepel`.

## En cas de problème

| Message | Solution |
|---|---|
| `there is no package called 'ggrepel'` (ou un autre package) | Relancer l'étape 2. |
| `Fichier introuvable : dataset/...` | R n'est pas dans le dossier `Visu`. Utiliser **Knit**, ou faire `setwd()` vers `Visu` avant `rmarkdown::render()`. |
| Caractères accentués mal affichés | Ouvrir le Rmd en UTF-8 : dans RStudio, *File > Reopen with Encoding > UTF-8*. |

## Source des données

UCI Machine Learning Repository, *Heart Disease*. Janosi, Steinbrunn, Pfisterer et Detrano (1989).
DOI : [10.24432/C52P4X](https://doi.org/10.24432/C52P4X). Licence CC BY 4.0.
