#' ==============================================================================
#' Rboard - Barre de navigation des pages du dashboard
#' ==============================================================================

#' Interface de la barre de navigation des pages
#' @export
mod_dash_page_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_principal"))
}

#' Serveur de la barre de navigation des pages
#' @export
mod_dash_page_server <- function(id, projet_r, page_courante_r, present_visible_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    msg_r <- reactiveVal(NULL)

    # Navigateur de fichier pour le PDF
    show_browser_r <- reactiveVal(FALSE)
    selected_pdf_r <- reactiveVal(NULL)
    msg_pdf_r <- reactiveVal(NULL)

    mod_file_browser_server("pdf_browser", show_browser_r, selected_pdf_r,
                             mode = "save", filetypes = "pdf",
                             default_filename = "dashboard_rboard.pdf")

    observeEvent(selected_pdf_r(), {
      chemin <- selected_pdf_r()
      if (!is.null(chemin) && nzchar(chemin)) {
        if (!grepl("\\.pdf$", chemin, ignore.case = TRUE)) {
          chemin <- paste0(chemin, ".pdf")
        }
        p <- projet_r()
        res <- tryCatch({
          exporter_pdf_dashboard(p, chemin)
          list(type = "success",
               texte = sprintf("PDF genere : %s", basename(chemin)))
        }, error = function(e) {
          list(type = "danger",
               texte = sprintf("Erreur export PDF : %s", conditionMessage(e)))
        })
        msg_pdf_r(res)
      }
      selected_pdf_r(NULL)
    })

    # --- UI principal (carte OU navigateur) ---
    output$ui_principal <- renderUI({
      if (isTRUE(show_browser_r())) {
        return(mod_file_browser_ui(ns("pdf_browser")))
      }

      bslib::card(
        bslib::card_body(
          style = "padding: 14px 20px;",
          tags$div(
            style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
            tags$div(
              style = "flex: 1; min-width: 220px;",
              uiOutput(ns("ui_selecteur_page"))),
            tags$div(
              style = "display: flex; gap: 6px;",
              actionButton(ns("btn_nouvelle_page"), NULL,
                           icon = bsicons::bs_icon("plus-lg"),
                           class = "btn-sm btn-outline-primary",
                           title = "Nouvelle page"),
              actionButton(ns("btn_renommer_page"), NULL,
                           icon = bsicons::bs_icon("pencil-square"),
                           class = "btn-sm btn-outline-secondary",
                           title = "Renommer la page"),
              actionButton(ns("btn_supprimer_page"), NULL,
                           icon = bsicons::bs_icon("trash"),
                           class = "btn-sm btn-outline-danger",
                           title = "Supprimer la page"),
              tags$span(style = "width: 1px; background: #e2e8f0; margin: 0 4px;"),
              actionButton(ns("btn_page_gauche"), NULL,
                           icon = bsicons::bs_icon("arrow-left"),
                           class = "btn-sm btn-outline-secondary",
                           title = "Deplacer a gauche"),
              actionButton(ns("btn_page_droite"), NULL,
                           icon = bsicons::bs_icon("arrow-right"),
                           class = "btn-sm btn-outline-secondary",
                           title = "Deplacer a droite")),
            tags$div(
              style = "display: flex; gap: 6px;",
              actionButton(
                ns("btn_imprimer"),
                label = "Imprimer (PDF)",
                icon = bsicons::bs_icon("printer-fill"),
                class = "btn-outline-primary",
                style = "font-weight: 600;"
              ),
              actionButton(
                ns("btn_presentation"),
                label = "Mode presentation",
                icon = bsicons::bs_icon("play-circle-fill"),
                class = "btn-primary",
                style = "font-weight: 600;"
              ))),
          uiOutput(ns("message_page"), style = "margin-top: 10px;"),
          uiOutput(ns("message_pdf"), style = "margin-top: 8px;")
        )
      )
    })

    output$ui_selecteur_page <- renderUI({
      p <- projet_r()
      pages <- if (!is.null(p$dashboard) && !is.null(p$dashboard$pages)) p$dashboard$pages else list()

      if (length(pages) == 0) {
        return(tags$em(style = "color: #94a3b8;", "Aucune page"))
      }

      choix <- setNames(
        sapply(pages, function(x) x$id),
        sapply(pages, function(x) x$titre)
      )
      selectInput(ns("page_sel"), NULL, choices = choix,
                  selected = page_courante_r(), width = "100%")
    })

    observeEvent(input$page_sel, {
      if (!is.null(input$page_sel) && nzchar(input$page_sel)) {
        page_courante_r(input$page_sel)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$btn_nouvelle_page, {
      p <- projet_r()
      if (is.null(p$dashboard)) p$dashboard <- creer_dashboard()
      nouvelle <- creer_page(sprintf("Page %d", length(p$dashboard$pages) + 1))
      p$dashboard <- ajouter_page(p$dashboard, nouvelle)
      p$date_modification <- Sys.Date()
      projet_r(p)
      page_courante_r(nouvelle$id)
      msg_r(list(type = "success", texte = "Page creee."))
    })

    observeEvent(input$btn_renommer_page, {
      id_page <- page_courante_r(); req(id_page)
      p <- projet_r()
      page <- obtenir_page(p$dashboard, id_page)
      req(page)

      showModal(modalDialog(
        title = "Renommer la page",
        textInput(ns("nouveau_titre"), "Nouveau titre :",
                  value = page$titre, width = "100%"),
        footer = tagList(
          modalButton("Annuler"),
          actionButton(ns("btn_valider_renommer"), "Valider", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_valider_renommer, {
      id_page <- page_courante_r(); req(id_page)
      titre <- trimws(input$nouveau_titre %||% "")
      if (!nzchar(titre)) return()
      p <- projet_r()
      p$dashboard <- renommer_page(p$dashboard, id_page, titre)
      p$date_modification <- Sys.Date()
      projet_r(p)
      removeModal()
      msg_r(list(type = "success", texte = "Page renommee."))
    })

    observeEvent(input$btn_supprimer_page, {
      id_page <- page_courante_r(); req(id_page)
      p <- projet_r()
      if (length(p$dashboard$pages) <= 1) {
        msg_r(list(type = "warning", texte = "Impossible de supprimer la derniere page."))
        return()
      }
      showModal(modalDialog(
        title = tags$div(
          style = "display: flex; align-items: center; gap: 8px; color: #dc2626;",
          bsicons::bs_icon("exclamation-triangle-fill"), " Confirmer la suppression"
        ),
        tags$p("Supprimer cette page et tous ses elements ?"),
        footer = tagList(
          modalButton("Annuler"),
          actionButton(ns("btn_valider_suppr"), "Supprimer", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_valider_suppr, {
      id_page <- page_courante_r(); req(id_page)
      p <- projet_r()
      p$dashboard <- supprimer_page(p$dashboard, id_page)
      p$date_modification <- Sys.Date()
      projet_r(p)
      removeModal()
      if (length(p$dashboard$pages) > 0) {
        page_courante_r(p$dashboard$pages[[1]]$id)
      } else {
        page_courante_r(NULL)
      }
      msg_r(list(type = "success", texte = "Page supprimee."))
    })

    observeEvent(input$btn_page_gauche, {
      id_page <- page_courante_r(); req(id_page)
      p <- projet_r()
      p$dashboard <- deplacer_page(p$dashboard, id_page, "gauche")
      p$date_modification <- Sys.Date()
      projet_r(p)
    })

    observeEvent(input$btn_page_droite, {
      id_page <- page_courante_r(); req(id_page)
      p <- projet_r()
      p$dashboard <- deplacer_page(p$dashboard, id_page, "droite")
      p$date_modification <- Sys.Date()
      projet_r(p)
    })

    observeEvent(input$btn_presentation, {
      id_page <- page_courante_r()
      if (is.null(id_page)) {
        msg_r(list(type = "warning", texte = "Aucune page a presenter."))
        return()
      }
      present_visible_r(TRUE)
    })

    observeEvent(input$btn_imprimer, {
      msg_pdf_r(NULL)
      p <- projet_r()
      if (is.null(p$dashboard) || length(p$dashboard$pages) == 0) {
        msg_pdf_r(list(type = "warning", texte = "Aucune page a imprimer."))
        return()
      }
      show_browser_r(TRUE)
    })

    output$message_page <- renderUI({
      m <- msg_r(); if (is.null(m)) return(NULL)
      rboard_alerte(m$type, m$texte)
    })

    output$message_pdf <- renderUI({
      m <- msg_pdf_r(); if (is.null(m)) return(NULL)
      rboard_alerte(m$type, m$texte)
    })
  })
}

