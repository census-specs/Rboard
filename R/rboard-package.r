#' Rboard : outil de construction de tableaux de bord statistiques
#'
#' Rboard est une application Shiny distribuee sous forme de package R.
#' Elle permet d'importer des donnees, de creer des indicateurs
#' statistiques (KPI), de construire des graphiques et d'assembler
#' des tableaux de bord exportables en HTML, Word ou PDF.
#'
#' @section Demarrage rapide :
#' \preformatted{
#'   library(Rboard)
#'   Rboard::run_app()
#' }
#'
#' @keywords internal
"_PACKAGE"

# Imports globaux
#' @importFrom stats sd median quantile t.test chisq.test cor.test
#'   wilcox.test kruskal.test aov shapiro.test fisher.test pf
#' @importFrom utils head tail read.csv
#' @importFrom grDevices pdf dev.off
#' @importFrom tools file_ext
NULL