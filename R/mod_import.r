#' ==============================================================================
#' Rboard - Module d'importation
#' ==============================================================================

rboard_etape_titre <- function(numero, titre) {
  tags$div(
    style = "display: flex; align-items: center; gap: 10px; margin-bottom: 14px;",
    tags$span(style = paste0("display: inline-flex; align-items: center; justify-content: center; ",
                              "width: 26px; height: 26px; border-radius: 50%; ",
                              "background: #2563eb; color: #ffffff; font-weight: 700; font-size: 0.85rem;"),
              numero),
    tags$span(style = "font-weight: 600; font-size: 1.05rem; color: #0f172a;", titre))
}

rboard_table_preview <- function(donnees, max_rows = 10) {
  df <- head(donnees, max_rows)
  cols <- names(df)
  n <- nrow(df)
  td <- "border: 1px solid #e2e8f0; padding: 6px 12px; color: #334155; white-space: nowrap; vertical-align: middle;"
  th <- "border: 1px solid #cbd5e1; padding: 8px 12px; background: #f1f5f9; font-weight: 600; color: #0f172a; text-align: left; white-space: nowrap;"

  tags$div(
    style = "overflow-x: auto; border-radius: 6px; border: 1px solid #cbd5e1; max-width: 100%;",
    tags$table(
      style = "border-collapse: collapse; width: 100%; font-size: 0.85rem;",
      tags$thead(tags$tr(lapply(cols, function(c) tags$th(style = th, c)))),
      tags$tbody(lapply(seq_len(n), function(i) {
        tags$tr(lapply(cols, function(c) {
          v <- df[[c]][i]
          txt <- if (is.na(v)) "NA" else as.character(v)
          tags$td(style = td, txt)
        }))
      }))))
}

rboard_table_resume <- function(donnees) {
  resume <- resumer_variables(donnees)
  n <- nrow(resume)
  td <- "border: 1px solid #e2e8f0; padding: 6px 12px; color: #334155; white-space: nowrap; vertical-align: middle;"
  tdr <- "border: 1px solid #e2e8f0; padding: 6px 12px; color: #334155; white-space: nowrap; vertical-align: middle; text-align: right;"
  th <- "border: 1px solid #cbd5e1; padding: 8px 12px; background: #f1f5f9; font-weight: 600; color: #0f172a; text-align: left; white-space: nowrap;"

  tags$div(
    style = "overflow-x: auto; border-radius: 6px; border: 1px solid #cbd5e1; max-width: 100%;",
    tags$table(
      style = "border-collapse: collapse; width: 100%; font-size: 0.85rem;",
      tags$thead(tags$tr(
        tags$th(style = th, "Variable"), tags$th(style = th, "Type"),
        tags$th(style = paste0(th, " text-align: right;"), "NA"),
        tags$th(style = paste0(th, " text-align: right;"), "Uniques"),
        tags$th(style = th, "Exemple"))),
      tags$tbody(lapply(seq_len(n), function(i) {
        tags$tr(
          tags$td(style = td, resume$variable[i]),
          tags$td(style = td, tags$span(style = "display: inline-flex; align-items: center; gap: 6px;",
                                        icone_type_variable(resume$type[i]), resume$type[i])),
          tags$td(style = tdr, as.character(resume$nb_na[i])),
          tags$td(style = tdr, as.character(resume$nb_uniques[i])),
          tags$td(style = td, resume$exemple[i]))
      }))))
}

#' Interface
#' @export
mod_import_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 1000px; margin: 0 auto; padding: 8px 0;",
    uiOutput(ns("contenu"))
  )
}

#' Serveur
#' @export
mod_import_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    donnees_internes_r <- reactiveVal(NULL)
    chemin_fichier_r   <- reactiveVal(NULL)
    message_charge_r   <- reactiveVal(NULL)
    message_global_r   <- reactiveVal(NULL)
    show_browser_r     <- reactiveVal(FALSE)
    selected_path_r    <- reactiveVal(NULL)

    mod_file_browser_server("browser", show_browser_r, selected_path_r,
                             mode = "open",
                             filetypes = c("csv", "xlsx", "xls", "rds"))

    observeEvent(selected_path_r(), {
      p <- selected_path_r()
      if (!is.null(p) && nzchar(p)) {
        chemin_fichier_r(p)
        message_charge_r(NULL)
      }
      selected_path_r(NULL)
    })

    output$contenu <- renderUI({
      if (isTRUE(show_browser_r())) {
        return(mod_file_browser_ui(ns("browser")))
      }

      tagList(
        bslib::card(
          bslib::card_body(
            style = "padding: 20px 24px;",
            rboard_etape_titre(1, "Choisir la source des donnees"),

            radioButtons(ns("source_type"), NULL,
                         choices = c("Fichier (.csv, .xlsx, .xls, .rds)" = "fichier",
                                     "Data.frame de l'environnement R" = "environnement"),
                         selected = "fichier", inline = TRUE),

            tags$div(style = "margin-top: 12px;",

              conditionalPanel(
                condition = sprintf("input['%s'] == 'fichier'", ns("source_type")),
                tags$div(
                  style = "display: flex; gap: 12px; align-items: center; flex-wrap: wrap;",
                  actionButton(ns("btn_parcourir"), "Parcourir...",
                               icon = bsicons::bs_icon("folder-fill"),
                               class = "btn-primary"),
                  uiOutput(ns("nom_fichier_choisi"), inline = TRUE))),

              conditionalPanel(
                condition = sprintf("input['%s'] == 'environnement'", ns("source_type")),
                tags$div(
                  style = "max-width: 520px;",
                  selectInput(ns("df_environnement"), "Data.frame disponibles dans .GlobalEnv :",
                              choices = NULL, width = "100%"),
                  tags$p(style = "font-size: 0.82rem; color: #64748b; margin-top: 4px;",
                         bsicons::bs_icon("info-circle"),
                         " Seuls les objets de type data.frame sont listes."),
                  actionButton(ns("btn_rafraichir_env"), "Rafraichir la liste",
                               icon = bsicons::bs_icon("arrow-clockwise"),
                               class = "btn-sm btn-outline-secondary")))),

            tags$hr(style = "margin: 18px 0 14px 0;"),

            actionButton(ns("btn_charger"), "Charger les donnees",
                         icon = bsicons::bs_icon("download"), class = "btn-primary"),
            uiOutput(ns("message_chargement"), style = "margin-top: 14px;"))),

        uiOutput(ns("ui_etape_apercu")),
        uiOutput(ns("ui_etape_confirmation")),
        uiOutput(ns("zone_message"))
      )
    })

    observeEvent(input$btn_parcourir, { show_browser_r(TRUE) })

    lister_df_env <- function() {
      noms <- ls(envir = .GlobalEnv)
      if (length(noms) == 0) return(character(0))
      est_df <- sapply(noms, function(n) tryCatch(is.data.frame(get(n, envir = .GlobalEnv)),
                                                    error = function(e) FALSE))
      noms[est_df]
    }
    rafraichir_env <- function() {
      noms_df <- lister_df_env()
      if (length(noms_df) == 0) {
        updateSelectInput(session, "df_environnement",
                          choices = c("(Aucun data.frame dans l'environnement)" = ""))
      } else {
        updateSelectInput(session, "df_environnement", choices = noms_df, selected = noms_df[1])
      }
    }
    observe({ rafraichir_env() })
    observeEvent(input$btn_rafraichir_env, { rafraichir_env() })

    output$nom_fichier_choisi <- renderUI({
      c <- chemin_fichier_r()
      if (is.null(c)) {
        return(tags$span(style = "color: #94a3b8; font-size: 0.85rem; font-style: italic;",
                         "Aucun fichier selectionne"))
      }
      tags$span(style = "color: #2563eb; font-size: 0.88rem; font-weight: 500;",
                bsicons::bs_icon("file-earmark-check-fill"), " ", basename(c))
    })

    observeEvent(input$btn_charger, {
      message_charge_r(NULL); message_global_r(NULL); donnees_internes_r(NULL)
      res <- tryCatch({
        if (identical(input$source_type, "fichier")) {
          chemin <- chemin_fichier_r()
          if (is.null(chemin) || !nzchar(chemin)) stop("Veuillez d'abord selectionner un fichier.", call. = FALSE)
          df <- importer_fichier(chemin); label_source <- basename(chemin)
        } else {
          nom_df <- input$df_environnement
          if (is.null(nom_df) || !nzchar(nom_df)) stop("Aucun data.frame selectionne.", call. = FALSE)
          df <- get(nom_df, envir = .GlobalEnv); label_source <- nom_df
        }
        valider_donnees(df); donnees_internes_r(df)
        list(type = "success", texte = sprintf(
          "Donnees chargees : %s (%d lignes, %d colonnes).",
          label_source, nrow(df), ncol(df)))
      }, error = function(e) list(type = "danger", texte = sprintf("Erreur : %s", conditionMessage(e))))
      message_charge_r(res)
    })

    output$message_chargement <- renderUI({
      msg <- message_charge_r(); if (is.null(msg)) return(NULL)
      rboard_alerte(msg$type, msg$texte)
    })

    output$ui_etape_apercu <- renderUI({
      df <- donnees_internes_r(); if (is.null(df)) return(NULL)
      bslib::card(
        style = "margin-top: 16px;",
        bslib::card_body(
          style = "padding: 20px 24px;",
          rboard_etape_titre(2, "Apercu des donnees"),
          tags$h5(style = "font-size: 0.95rem; font-weight: 600; color: #334155; margin-bottom: 10px;",
                  tags$span(bsicons::bs_icon("table"), " Apercu (10 premieres lignes)")),
          rboard_table_preview(df, max_rows = 10),
          tags$div(style = "margin-top: 16px;",
            bslib::accordion(open = FALSE,
              bslib::accordion_panel(
                title = tags$span(bsicons::bs_icon("list-columns-reverse"),
                                  " Resume des variables (cliquez pour afficher)"),
                value = "resume_vars",
                rboard_table_resume(df))))))
    })

    output$ui_etape_confirmation <- renderUI({
      df <- donnees_internes_r(); if (is.null(df)) return(NULL)
      bslib::card(
        style = "margin-top: 16px;",
        bslib::card_body(
          style = "padding: 20px 24px;",
          rboard_etape_titre(3, "Confirmer l'import"),
          tags$p(style = "color: #64748b; margin-bottom: 16px;",
                 "Cette action enregistre ces donnees dans le projet."),
          actionButton(ns("btn_confirmer"), "Confirmer l'import",
                       icon = bsicons::bs_icon("check-lg"), class = "btn-primary")))
    })

    observeEvent(input$btn_confirmer, {
      df <- donnees_internes_r()
      if (is.null(df)) {
        message_global_r(list(type = "warning", texte = "Aucune donnee a confirmer.")); return()
      }
      p <- projet_r(); p$donnees <- df; p$date_modification <- Sys.Date(); projet_r(p)
      donnees_internes_r(NULL); chemin_fichier_r(NULL); message_charge_r(NULL)
      message_global_r(list(type = "success",
        texte = sprintf("Import confirme : %d lignes et %d colonnes.", nrow(df), ncol(df))))
    })

    output$zone_message <- renderUI({
      msg <- message_global_r(); if (is.null(msg)) return(NULL)
      tags$div(style = "margin-top: 16px;", rboard_alerte(msg$type, msg$texte))
    })
  })
}