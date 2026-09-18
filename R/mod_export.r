#' ==============================================================================
#' Rboard - Module Export (UI compacte + theme + metadonnees)
#' ==============================================================================

mod_export_ui <- function(id) {
  ns <- NS(id)
  tags$div(style = "max-width: 1100px; margin: 0 auto; padding: 8px 0;",
    uiOutput(ns("contenu")))
}

mod_export_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_browser_r <- reactiveVal(FALSE)
    selected_r     <- reactiveVal(NULL)
    chemin_r       <- reactiveVal(NULL)
    message_r      <- reactiveVal(NULL)
    format_attente_r <- reactiveVal("html")

    mod_file_browser_server("browser", show_browser_r, selected_r,
                             mode = "save", filetypes = c("html", "docx"),
                             default_filename = "rapport_rboard.html")

    observeEvent(selected_r(), {
      chemin <- selected_r()
      if (!is.null(chemin) && nzchar(chemin)) {
        fmt <- format_attente_r()
        ext <- if (fmt == "docx") ".docx" else ".html"
        if (!grepl(paste0("\\", ext, "$"), chemin, ignore.case = TRUE))
          chemin <- paste0(chemin, ext)
        chemin_r(chemin)
      }
      selected_r(NULL)
    })

    output$contenu <- renderUI({
      if (isTRUE(show_browser_r())) {
        return(mod_file_browser_ui(ns("browser")))
      }

      p <- projet_r()
      tagList(
        bslib::card(
          bslib::card_body(
            style = "padding: 20px 24px;",

            # ---- Ligne 1 : Titre + Auteur + Institution ----
            fluidRow(
              column(6,
                textInput(ns("titre"), "Titre du rapport :",
                          value = "", width = "100%")),
              column(3,
                textInput(ns("auteur"), "Auteur :",
                          value = "", width = "100%")),
              column(3,
                textInput(ns("institution"), "Institution :",
                          value = "", width = "100%"))),

            # ---- Ligne 2 : Description ----
            textAreaInput(ns("description"), "Description (optionnel) :",
                          value = "", rows = 2, width = "100%",
                          placeholder = "Ex : Analyse statistique de l'essai clinique 2026"),

            tags$hr(style = "margin: 10px 0 16px 0;"),

            # ---- Ligne 3 : Format + Theme ----
            fluidRow(
              column(4,
                radioButtons(ns("format"), "Format :",
                             choices = c("HTML" = "html", "Word (.docx)" = "docx"),
                             selected = "html", inline = TRUE)),
              column(4,
                selectInput(ns("theme"), "Theme (HTML) :",
                            choices = rboard_themes_html(),
                            selected = "flatly", width = "100%")),
              column(4,
                tags$div(style = "padding-top: 30px;",
                  checkboxInput(ns("inclure_date"), "Afficher la date",
                                value = TRUE)))),

            # ---- Ligne 4 : Pages ----
            tags$div(style = "margin-top: 4px;",
              radioButtons(ns("mode_pages"), "Pages a exporter :",
                           choices = c("Toutes les pages" = "toutes",
                                       "Selection specifique" = "selection"),
                           selected = "toutes", inline = TRUE),
              uiOutput(ns("ui_selection_pages"))),

            tags$hr(style = "margin: 14px 0 14px 0;"),

            # ---- Ligne 5 : Emplacement + bouton ----
            fluidRow(
              column(8,
                tags$div(
                  style = "display: flex; gap: 12px; align-items: center;",
                  actionButton(ns("btn_parcourir"), "Choisir l'emplacement",
                               icon = bsicons::bs_icon("folder-fill"),
                               class = "btn-outline-primary"),
                  uiOutput(ns("chemin_affiche"), inline = TRUE))),
              column(4,
                tags$div(style = "text-align: right;",
                  actionButton(ns("btn_exporter"), "Exporter",
                               icon = bsicons::bs_icon("download"),
                               class = "btn-success",
                               style = "padding: 10px 28px; font-weight: 600;"))))
          )),

        uiOutput(ns("zone_message"))
      )
    })

    # Titre par defaut
    observe({
      p <- projet_r()
      if (!is.null(p$nom) && nzchar(p$nom)) {
        t <- isolate(input$titre)
        if (is.null(t) || !nzchar(t)) {
          updateTextInput(session, "titre", value = sprintf("Rapport - %s", p$nom))
        }
      }
    })

    # Afficher le theme uniquement si HTML
    observeEvent(input$format, {
      if (identical(input$format, "docx")) {
        # Desactiver le theme visuellement (facultatif)
      }
    }, ignoreInit = TRUE)

    output$ui_selection_pages <- renderUI({
      if (!identical(input$mode_pages, "selection")) return(NULL)
      p <- projet_r()
      pages <- if (!is.null(p$dashboard) && !is.null(p$dashboard$pages)) p$dashboard$pages else list()
      if (length(pages) == 0) return(tags$p(style = "color: #94a3b8; font-style: italic;", "Aucune page."))
      choix <- setNames(sapply(pages, function(x) x$id), sapply(pages, function(x) x$titre))
      tags$div(
        style = "margin-top: 8px; padding: 10px 12px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px;",
        checkboxGroupInput(ns("pages_sel"), NULL, choices = choix, selected = unname(choix),
                           inline = TRUE))
    })

    observeEvent(input$btn_parcourir, {
      format_attente_r(input$format %||% "html")
      show_browser_r(TRUE)
    })

    output$chemin_affiche <- renderUI({
      c <- chemin_r()
      if (is.null(c)) return(tags$span(style = "color: #94a3b8; font-size: 0.85rem; font-style: italic;",
                                       "Aucun emplacement selectionne"))
      tags$span(style = "font-size: 0.88rem; color: #2563eb; font-weight: 500;",
                bsicons::bs_icon("check-circle-fill"), " ", basename(c))
    })

    observeEvent(input$btn_exporter, {
      message_r(NULL)
      chemin <- chemin_r()
      if (is.null(chemin) || !nzchar(chemin)) {
        message_r(list(type = "warning", texte = "Veuillez choisir un emplacement."))
        return()
      }
      p <- projet_r()
      if (is.null(p$dashboard) || length(p$dashboard$pages) == 0) {
        message_r(list(type = "warning", texte = "Le projet ne contient aucune page."))
        return()
      }

      pages_a_exporter <- NULL
      if (identical(input$mode_pages, "selection")) {
        sel <- input$pages_sel
        if (is.null(sel) || length(sel) == 0) {
          message_r(list(type = "warning", texte = "Selectionnez au moins une page."))
          return()
        }
        pages_a_exporter <- sel
      }

      fmt <- input$format
      ext <- if (fmt == "docx") ".docx" else ".html"
      if (!grepl(paste0("\\", ext, "$"), chemin, ignore.case = TRUE)) {
        chemin <- paste0(chemin, ext); chemin_r(chemin)
      }
      titre <- trimws(input$titre %||% "")
      if (!nzchar(titre)) titre <- if (!is.null(p$nom) && nzchar(p$nom)) p$nom else "Rapport Rboard"

      tryCatch({
        if (fmt == "docx") {
          exporter_word(p, chemin, titre = titre, pages_a_exporter = pages_a_exporter)
        } else {
          exporter_html(p, chemin, titre = titre, pages_a_exporter = pages_a_exporter,
                        theme = input$theme %||% "flatly",
                        auteur = input$auteur,
                        institution = input$institution,
                        description = input$description,
                        inclure_date = isTRUE(input$inclure_date))
        }
        message_r(list(type = "success", texte = chemin))
      }, error = function(e) {
        message_r(list(type = "danger",
          texte = sprintf("Erreur : %s", conditionMessage(e))))
      })
    })

    observeEvent(input$btn_ouvrir_dossier, {
      m <- message_r()
      if (!is.null(m) && identical(m$type, "success")) rboard_ouvrir_dossier(m$texte)
    })

    output$zone_message <- renderUI({
      m <- message_r(); if (is.null(m)) return(NULL)
      if (identical(m$type, "success")) {
        return(tags$div(
          style = "margin-top: 16px; display: flex; align-items: center; gap: 12px; padding: 14px 18px; background: #dcfce7; border: 1px solid #86efac; border-radius: 10px;",
          bsicons::bs_icon("check-circle-fill", class = "fs-5"),
          tags$div(style = "flex: 1;",
            tags$strong("Export reussi dans : "),
            tags$code(style = "font-size: 0.85rem;", m$texte)),
          actionButton(ns("btn_ouvrir_dossier"), "Ouvrir le dossier",
                       icon = bsicons::bs_icon("folder2-open"),
                       class = "btn-sm btn-outline-success")))
      }
      tags$div(style = "margin-top: 16px;", rboard_alerte(m$type, m$texte))
    })
  })
}

