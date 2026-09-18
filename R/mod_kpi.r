#' ==============================================================================
#' Rboard - Helpers KPI (fonctions utilitaires partagees)
#' ==============================================================================

rboard_or <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.character(a) && length(a) == 1 && is.na(a))) b else a
}

rboard_nom_couleur <- function(cle) {
  switch(as.character(cle),
    "success" = "Vert", "danger" = "Rouge", "warning" = "Orange",
    "primary" = "Bleu", "info" = "Cyan", "secondary" = "Gris",
    "dark" = "Noir", "light" = "Clair", as.character(cle))
}

rboard_choix_couleurs <- function() {
  c("Vert" = "success", "Rouge" = "danger", "Orange" = "warning",
    "Bleu" = "primary", "Cyan" = "info", "Gris" = "secondary",
    "Noir" = "dark", "Clair" = "light")
}

#' Choix de variables avec icone de type
#' @keywords internal
rboard_choix_variables <- function(donnees, filtre = NULL) {
  if (is.null(donnees) || !is.data.frame(donnees)) return(c("(Aucune donnee)" = ""))
  noms <- names(donnees)
  types <- sapply(noms, function(v) detecter_type_variable(donnees[[v]]))
  if (!is.null(filtre)) {
    garder <- types %in% filtre
    noms <- noms[garder]; types <- types[garder]
  }
  if (length(noms) == 0) return(c("(Aucune variable)" = ""))
  setNames(noms, sprintf("%s (%s)", noms, types))
}

#' Tableau HTML statique du jeu de donnees (apercu limite)
#' @keywords internal
rboard_table_apercu_data <- function(donnees, max_rows = 10) {
  if (is.null(donnees) || !is.data.frame(donnees) || ncol(donnees) == 0) {
    return(tags$div(style = "padding: 20px; text-align: center; color: #94a3b8;",
                    "Aucune donnee"))
  }
  df <- head(donnees, max_rows)
  cols <- names(df)
  types <- sapply(cols, function(c) detecter_type_variable(df[[c]]))

  cell_td <- "border: 1px solid #e2e8f0; padding: 6px 12px; color: #334155; vertical-align: middle; font-size: 0.85rem; white-space: nowrap;"
  cell_th <- "border: 1px solid #cbd5e1; padding: 8px 12px; background: #f1f5f9; font-weight: 600; color: #0f172a; text-align: left; white-space: nowrap; font-size: 0.85rem;"

  header <- tags$thead(tags$tr(lapply(seq_along(cols), function(j) {
    tags$th(style = cell_th,
            tags$div(style = "display: flex; align-items: center; gap: 6px;",
                     icone_type_variable(types[j]),
                     tags$span(cols[j])))
  })))

  body <- tags$tbody(lapply(seq_len(nrow(df)), function(i) {
    tags$tr(lapply(cols, function(c) {
      v <- df[[c]][i]
      txt <- if (is.na(v)) "NA" else as.character(v)
      tags$td(style = cell_td, txt)
    }))
  }))

  tags$div(
    style = "border: 1px solid #cbd5e1; border-radius: 6px; max-height: 380px; overflow: auto;",
    tags$table(style = "border-collapse: collapse; width: 100%;", header, body)
  )
}

#' Tableau HTML du jeu de donnees complet (modale)
#' @keywords internal
rboard_table_data_full <- function(donnees) {
  if (is.null(donnees)) return(NULL)
  df <- donnees; cols <- names(df)

  cell_td <- "border: 1px solid #e2e8f0; padding: 4px 10px; color: #334155; font-size: 0.82rem; white-space: nowrap;"
  cell_th <- "border: 1px solid #cbd5e1; padding: 6px 10px; background: #f1f5f9; font-weight: 600; color: #0f172a; text-align: left; white-space: nowrap; font-size: 0.82rem; position: sticky; top: 0; z-index: 2;"

  header <- tags$thead(tags$tr(lapply(cols, function(c) tags$th(style = cell_th, c))))
  body <- tags$tbody(lapply(seq_len(nrow(df)), function(i) {
    tags$tr(lapply(cols, function(c) {
      v <- df[[c]][i]
      txt <- if (is.na(v)) "NA" else as.character(v)
      tags$td(style = cell_td, txt)
    }))
  }))

  tags$div(
    style = "border: 1px solid #cbd5e1; border-radius: 6px; max-height: 70vh; overflow: auto;",
    tags$table(style = "border-collapse: collapse; width: 100%;", header, body)
  )
}

#' Ligne de regle de couleur (utilisee dans la modale de modification)
#' @keywords internal
rboard_regle_ui <- function(ns, id_regle, signe = "<", valeur = 0.05, couleur = "success") {
  fluidRow(
    style = "margin-bottom: 8px; align-items: center;",
    column(2, selectInput(ns(paste0("mregle_signe_", id_regle)), NULL,
                          choices = c("<", "<=", ">", ">=", "="),
                          selected = signe, width = "100%")),
    column(3, numericInput(ns(paste0("mregle_valeur_", id_regle)), NULL,
                            value = valeur, width = "100%")),
    column(5, selectInput(ns(paste0("mregle_couleur_", id_regle)), NULL,
                          choices = rboard_choix_couleurs(),
                          selected = couleur, width = "100%")),
    column(2, actionButton(ns(paste0("mregle_del_", id_regle)), NULL,
                            icon = bsicons::bs_icon("x-lg"),
                            class = "btn-sm btn-outline-danger",
                            style = "width: 100%;"))
  )
}