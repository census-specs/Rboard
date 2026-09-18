#' ==============================================================================
#' Rboard - Module du bandeau global
#' Affiche en haut de chaque page un rappel compact de l'etat du projet
#' ==============================================================================

#' Interface utilisateur du bandeau
#'
#' @param id Identifiant du module Shiny.
#'
#' @return Un tagList d'elements d'interface Shiny.
#' @export
mod_bandeau_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("bandeau"))
}

#' Serveur du bandeau
#'
#' @param id Identifiant du module Shiny.
#' @param projet_r ReactiveVal contenant le projet Rboard.
#'
#' @return NULL
#' @export
mod_bandeau_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {

    output$bandeau <- renderUI({
      p <- projet_r()
      if (is.null(p)) return(NULL)

      nom <- if (!is.null(p$nom) && nzchar(p$nom)) p$nom else "Sans titre"

      info_donnees <- if (!is.null(p$donnees) && is.data.frame(p$donnees)) {
        sprintf("%d x %d", nrow(p$donnees), ncol(p$donnees))
      } else {
        "aucune"
      }

      nb_kpi   <- if (!is.null(p$kpi)) length(p$kpi) else 0
      nb_graph <- if (!is.null(p$graphiques)) length(p$graphiques) else 0
      nb_pages <- if (!is.null(p$dashboard) && !is.null(p$dashboard$pages)) {
        length(p$dashboard$pages)
      } else {
        0
      }

      tags$div(
        style = paste0(
          "max-width: 1000px; margin: 0 auto 20px auto; ",
          "padding: 10px 16px; background: #f8fafc; ",
          "border: 1px solid #e2e8f0; border-radius: 8px; ",
          "display: flex; align-items: center; justify-content: space-between; ",
          "gap: 16px; font-size: 0.85rem; color: #475569;"
        ),
        tags$div(
          style = "display: flex; align-items: center; gap: 8px; overflow: hidden;",
          bsicons::bs_icon("folder-fill", class = "text-primary"),
          tags$strong(
            style = "color: #0f172a; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
            nom
          )
        ),
        tags$div(
          style = "display: flex; gap: 18px; color: #64748b; flex-wrap: wrap;",
          tags$span(bsicons::bs_icon("database"), " ", info_donnees),
          tags$span(bsicons::bs_icon("calculator-fill"), " ", nb_kpi, " KPI"),
          tags$span(bsicons::bs_icon("bar-chart-fill"), " ", nb_graph, " graphiques"),
          tags$span(bsicons::bs_icon("grid-1x2-fill"), " ", nb_pages, " pages")
        )
      )
    })
  })
}