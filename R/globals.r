# ==============================================================================
# Rboard - Declarations des variables globales
# ==============================================================================
# Ce fichier declare les variables et fonctions utilisees mais importees
# depuis d'autres packages (shiny, ggplot2, etc.). Cela evite les centaines
# d'avertissements "no visible binding for global variable" lors de
# R CMD check.
# ==============================================================================

# --- Variables globales ---
utils::globalVariables(c(
  # ggplot2
  ".data",

  # Shiny (fonctions utilisees via UI)
  "tags", "HTML", "NS",

  # Utils / Stats de base
  "head", "tail", "na.omit", "setNames", "capture.output",
  "read.csv", "plot.new", "par", "text",

  # Variables internes Rboard qui apparaissent dans des expressions NSE
  "id", "nom", "valeur", "label", "type_kpi", "classe",
  "apercu", "n_lignes", "n_colonnes", "n_caracteres",
  "style_suggere", "nb_couches",
  "variable", "nb_na", "nb_uniques", "exemple",
  "Statistique", "Valeur",
  "n", "n_col", "n_row", "base_size", "tg"
))

# --- Import des fonctions Shiny non utilisees directement mais necessaires ---
#' @importFrom shiny NS tagList
#' @importFrom shiny tags HTML
#' @importFrom shiny actionButton checkboxInput checkboxGroupInput
#' @importFrom shiny column conditionalPanel downloadHandler fluidRow
#' @importFrom shiny isolate modalButton modalDialog moduleServer
#' @importFrom shiny numericInput observe observeEvent radioButtons
#' @importFrom shiny reactive reactiveVal removeModal renderPlot renderUI
#' @importFrom shiny req selectInput showModal showNotification
#' @importFrom shiny textAreaInput textInput uiOutput updateSelectInput
#' @importFrom shiny updateTextInput
#' @importFrom stats na.omit setNames
#' @importFrom utils capture.output head read.csv
#' @importFrom graphics par plot.new text
NULL