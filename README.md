# Rboard

<img src="man/figures/logo.png" align="right" height="139" alt="Logo Rboard" />

> Outil de construction de tableaux de bord statistiques interactifs.

[![R-CMD-check](https://github.com/census-specs/Rboard/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/census-specs/Rboard/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)

**Rboard** est une application R/Shiny conçue pour construire des tableaux de bord de résultats statistiques (p-values, moyennes, tailles d'effet, graphiques, commentaires d'interprétation) et les exporter en HTML, Word ou PDF.

Contrairement aux outils de Business Intelligence classiques, Rboard est **orienté reporting statistique** : chaque indicateur est pensé pour présenter un résultat d'analyse, pas pour explorer des données.

---

## Sommaire

- [Installation](#installation)
- [Fonctionnalités](#fonctionnalités)
- [Démarrage rapide](#démarrage-rapide)
- [Documentation](#documentation)
- [Structure du projet](#structure-du-projet)
- [Contribuer](#contribuer)
- [Auteur](#auteur)
- [Licence](#licence)

---

## Installation

Depuis GitHub avec `devtools` :

```r
install.packages("devtools")
devtools::install_github("census-specs/Rboard")
```

Puis lance l'application :

```r
library(Rboard)
Rboard::run_app()
```

---

## Fonctionnalités

### Import de données

- CSV, Excel (`.xlsx`, `.xls`), RDS
- Import direct depuis un `data.frame` de l'environnement R
- Détection automatique du type des variables (numérique, facteur, date, logique)

### Création de KPI statistiques

- **Statistiques descriptives** : moyenne, médiane, écart-type, variance, quartiles, minimum, maximum, effectif
- **Comparaison de groupes** : test t de Student, Wilcoxon, ANOVA, Kruskal-Wallis
- **Association** : corrélation Pearson/Spearman, Chi-deux, test exact de Fisher
- **Tableaux** : statistiques par groupe, tableaux croisés avec effectifs ou pourcentages
- **Saisie manuelle** : KPI libres avec valeur personnalisée

### Coloration conditionnelle

- Règles multiples (`<`, `<=`, `>`, `>=`, `=`)
- Couleurs personnalisables par règle
- Rendu visuel immédiat

### Graphiques

- Plus de 25 types : nuage de points, ligne, aire, boxplot, violon, barplot, histogramme, densité, mosaïque, heatmap
- Suggestions automatiques selon les types de variables
- Palettes de couleurs (Brasserie, Gris, Manuelle)
- Options avancées : couleur par variable, taille, facette

### Dashboard

- Grille 12 colonnes × 6 unités de hauteur
- Trois formats de tableau : compact (2 unités), long (3 unités, largeur 6 ou 12), étendu (4 unités, largeur 6)
- Deux formats de graphique : standard (3 unités) et grand (4 unités)
- Cartes KPI, tableaux, graphiques, blocs de texte
- Texte avec support Markdown (gras, italique, souligné, listes, code)
- Bibliothèque de textes réutilisables
- Mode présentation plein écran

### Export

- **HTML** autonome avec 6 thèmes (Flatly, Cosmo, Journal, Darkly, Minty, Lux)
- **Word** (`.docx`)
- **PDF** A4 paysage, qualité vectorielle

### Script Runner

- Exécution de scripts R en environnement isolé
- Détection automatique des résultats statistiques (test t, chi², régression, tableaux, vecteurs)
- Détection des objets `ggplot` et des textes markdown
- Transformation en KPI, tableau, texte ou graphique en un clic

---

## Démarrage rapide

```r
library(Rboard)
run_app()
```

Parcours typique :

1. **Import** — charger un jeu de données
2. **KPI** — créer des indicateurs statistiques
3. **Graphiques** — construire des visualisations
4. **Dashboard** — assembler les éléments en pages
5. **Export** — produire un rapport HTML, Word ou PDF

---

## Documentation

Aide générale :

```r
?Rboard
```

Aide sur une fonction :

```r
?creer_kpi
?construire_graphique
?exporter_html
```

---

## Structure du projet

```
Rboard/
├── R/                     # Code source des fonctions metier et modules Shiny
├── inst/app/              # Application Shiny
├── tests/testthat/        # Tests unitaires
├── man/                   # Documentation generee par roxygen2
├── DESCRIPTION            # Metadonnees du package
├── NAMESPACE              # Exports du package
└── README.md
```

---

## Contribuer

Les contributions sont bienvenues. Pour proposer une amélioration :

1. Ouvrir une *issue* pour discuter du changement
2. Créer une branche depuis `main`
3. Ajouter des tests si nécessaire
4. Soumettre une *pull request*

---

## Auteur

**Pierre Valdeze MBOM MBOM**

- Email : pierrembom@outlook.com
- GitHub : [@census-specs](https://github.com/census-specs)
- Téléphone : +237 98389030 / +237 650989019

---

## Licence

Ce projet est distribué sous licence MIT. Voir [LICENSE.md](LICENSE.md) pour plus de détails.
