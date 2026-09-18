# run_tests.R - Lance tous les tests unitaires de Rboard
# ==============================================================================

cat("\n============================================\n")
cat("Rboard - Suite de tests unitaires\n")
cat("============================================\n\n")

# --- Verification de testthat ---
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Le package 'testthat' est requis. Installez-le avec : install.packages('testthat')",
       call. = FALSE)
}
library(testthat)

# --- Chargement des packages externes ---
suppressMessages({
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
})

# --- Chargement des sources Rboard ---
cat("Chargement des sources Rboard...\n")
suppressWarnings(suppressMessages({
  source("R/utils.R")
  source("R/projet.R")
  source("R/import.R")
  source("R/validators.R")
  source("R/theme.R")
  source("R/kpi.R")
  source("R/stats.R")
  source("R/graphiques.R")
  source("R/dashboard.R")
  source("R/script_runner.R")
  source("R/export.R")
  source("R/export_pdf.R")
  source("R/mod_file_browser.R")
  source("R/mod_bandeau.R")
  source("R/mod_projet.R")
  source("R/mod_import.R")
  source("R/mod_kpi.R")
  source("R/mod_kpi_analyses.R")
  source("R/mod_kpi_liste.R")
  source("R/mod_graphique.R")
  source("R/mod_script.R")
  source("R/mod_export.R")
  source("R/mod_dash_page.R")
  source("R/mod_dash_ligne.R")
  source("R/mod_dash_element.R")
  source("R/mod_dash_element_edit.R")
  source("R/mod_dash_apercu.R")
  source("R/mod_dash_present.R")
}))
cat("Sources chargees.\n\n")

# --- Execution des tests ---
cat("Execution des tests...\n\n")

resultats <- testthat::test_dir(
  "tests/testthat",
  reporter = testthat::ProgressReporter$new(show_praise = FALSE, update_interval = 1),
  stop_on_failure = FALSE,
  stop_on_warning = FALSE
)

# --- Resume final ---
df <- as.data.frame(resultats)
nb_total <- nrow(df)
nb_ok <- sum(df$passed)
nb_fail <- sum(df$failed)
nb_warn <- sum(df$warning)
nb_skip <- sum(df$skipped)

cat("\n============================================\n")
cat("RESUME\n")
cat("============================================\n")
cat(sprintf("Tests reussis  : %d\n", nb_ok))
cat(sprintf("Tests echoues  : %d\n", nb_fail))
cat(sprintf("Avertissements : %d\n", nb_warn))
cat(sprintf("Tests ignores  : %d\n", nb_skip))
cat(sprintf("Total          : %d\n", nb_ok + nb_fail))
cat("============================================\n\n")

if (nb_fail > 0) {
  cat("Certains tests ont echoue. Verifiez la sortie ci-dessus.\n\n")
  quit(status = 1)
} else {
  cat("Tous les tests sont passes.\n\n")
}