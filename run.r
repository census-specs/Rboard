# run.R - Lance Rboard
# ==============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(readxl)
library(shinyFiles)
library(rmarkdown)
library(officer)
library(flextable)
library(gridExtra)
library(grid)
library(commonmark)

# --- 1. Utilitaires de base ---
source("R/utils.R")
source("R/projet.R")
source("R/import.R")
source("R/validators.R")

# --- 2. Design system ---
source("R/theme.R")

# --- 3. Moteurs metier ---
source("R/kpi.R")
source("R/stats.R")
source("R/graphiques.R")
source("R/dashboard.R")
source("R/script_runner.R")
source("R/export.R")
source("R/export_pdf.R")

# --- 4. Modules Shiny ---
source("R/mod_file_browser.R")
source("R/mod_bandeau.R")
source("R/mod_projet.R")
source("R/mod_import.R")
source("R/mod_donnees.R")          # NOUVEAU
source("R/mod_kpi.R")
source("R/mod_kpi_analyses.R")
source("R/mod_kpi_liste.R")
source("R/mod_texte_liste.R")
source("R/mod_graphique.R")
source("R/mod_script.R")
source("R/mod_export.R")
source("R/mod_dash_page.R")
source("R/mod_dash_ligne.R")
source("R/mod_dash_element.R")
source("R/mod_dash_element_edit.R")
source("R/mod_dash_apercu.R")
source("R/mod_dash_present.R")

# --- 5. Redefinition finale de securite ---
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 ||
      (is.character(a) && length(a) == 1 && is.na(a))) {
    b
  } else {
    a
  }
}

# --- 6. Lancement ---
shiny::runApp("inst/app")