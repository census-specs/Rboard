# Rboard 0.1.0

## Version initiale

Première version publique de Rboard, outil de construction de tableaux de
bord statistiques pour R et Shiny.

### Fonctionnalités principales

#### Import de données
- Support des formats CSV, Excel (`.xlsx`, `.xls`) et RDS.
- Import direct depuis un `data.frame` de l'environnement R.
- Détection automatique du type des variables (numérique, facteur,
  date, logique).

#### Bibliothèque de KPI statistiques
- Statistiques descriptives : moyenne, médiane, écart-type, variance,
  quartiles, minimum, maximum, effectif.
- Comparaison de groupes : test t de Student, Wilcoxon, ANOVA,
  Kruskal-Wallis.
- Association : corrélation Pearson/Spearman, Chi-deux, test exact de
  Fisher.
- Tableaux : statistiques par groupe, tableaux croisés avec effectifs
  ou pourcentages.
- Saisie manuelle de KPI personnalisés.

#### Graphiques
- Plus de 25 types de graphiques basés sur `ggplot2`.
- Suggestions automatiques selon les types de variables.
- Palettes de couleurs (Brasserie, Gris, Manuelle).
- Options avancées : couleur par variable, taille, facette.

#### Dashboard
- Grille 12 colonnes × 6 unités de hauteur.
- Trois formats de tableau : compact (2 unités), long (3 unités,
  largeur 6 ou 12), étendu (4 unités, largeur 6).
- Deux formats de graphique : standard (3 unités) et grand (4 unités).
- Support Markdown pour les blocs de texte.
- Bibliothèque de textes réutilisables.
- Multi-pages (1 page = 1 A4 paysage).
- Mode présentation plein écran.

#### Export
- HTML autonome avec 6 thèmes (Flatly, Cosmo, Journal, Darkly, Minty,
  Lux).
- Word (`.docx`).
- PDF A4 paysage vectoriel.

#### Script Runner
- Exécution de scripts R dans un environnement isolé.
- Détection automatique des résultats statistiques (tests, régressions,
  tables, vecteurs).
- Détection des tableaux, textes markdown et objets `ggplot`.
- Transformation en KPI, tableau, texte ou graphique en un clic.

### Notes de version

- Migration automatique des anciens projets vers la grille 12 × 6.
- Nettoyage automatique des commentaires générés par le Script Runner.
- Style uniforme des tableaux (bordures horizontales et verticales).
- Police Corbel pour l'interface, JetBrains Mono pour les chiffres.
