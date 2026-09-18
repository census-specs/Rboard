#' ==============================================================================
#' Rboard - Mode presentation plein ecran
#' ==============================================================================

#' Interface du mode presentation
#' @export
mod_dash_present_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("overlay"))
}

#' Serveur du mode presentation
#' @export
mod_dash_present_server <- function(id, projet_r, page_courante_r, present_visible_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    index_r <- reactiveVal(1L)

    observeEvent(present_visible_r(), {
      if (isTRUE(present_visible_r())) {
        p <- projet_r()
        pages <- p$dashboard$pages %||% list()
        if (length(pages) > 0) {
          idx <- which(sapply(pages, function(x) identical(x$id, page_courante_r())))
          index_r(if (length(idx) > 0) idx[1] else 1L)
        } else {
          index_r(1L)
        }
      }
    }, ignoreInit = TRUE)

    pages_r <- reactive({
      p <- projet_r()
      p$dashboard$pages %||% list()
    })

    observeEvent(input$present_prev, {
      n <- length(pages_r()); if (n == 0) return()
      i <- index_r(); if (i > 1) index_r(i - 1)
    })
    observeEvent(input$present_next, {
      n <- length(pages_r()); if (n == 0) return()
      i <- index_r(); if (i < n) index_r(i + 1)
    })
    observeEvent(input$present_quit, {
      present_visible_r(FALSE)
    })

    observe({
      if (!isTRUE(present_visible_r())) return()
      req(projet_r())
      rboard_enregistrer_plots(projet_r, output, prefixe = "present")
    })

    output$overlay <- renderUI({
      if (!isTRUE(present_visible_r())) return(NULL)

      p <- projet_r()
      pages <- pages_r()
      n <- length(pages)

      if (n == 0) {
        return(tags$div(
          style = "position: fixed; inset: 0; background: #ffffff; z-index: 100000; display: flex; align-items: center; justify-content: center;",
          tags$div(style = "text-align: center;",
                   tags$h3("Aucune page a presenter"),
                   actionButton(ns("present_quit"), "Fermer", class = "btn-primary"))
        ))
      }

      idx <- min(max(index_r(), 1L), n)
      page <- pages[[idx]]

      # ======================================================================
      # JS : clavier
      # ======================================================================
      js <- tags$script(HTML(sprintf("
        (function() {
          if (window.rboard_present_kb_installed) return;
          window.rboard_present_kb_installed = true;
          document.addEventListener('keydown', function(e) {
            var root = document.getElementById('%s');
            if (!root) return;
            if (e.key === 'Escape') {
              Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
            } else if (e.key === 'ArrowLeft') {
              Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
            } else if (e.key === 'ArrowRight') {
              Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
            }
          });
        })();
      ",
        ns("overlay_root"),
        ns("present_quit"),
        ns("present_prev"),
        ns("present_next")
      )))

      # ======================================================================
      # CSS : plein ecran avec --rboard-unit calcule sur la hauteur du viewport
      # ======================================================================
      css <- tags$style(HTML(sprintf("
        #%s {
          position: fixed;
          inset: 0;
          background: #ffffff;
          z-index: 100000;
          overflow: hidden;
        }

        #%s .rboard-present-stage {
          position: absolute;
          top: 0; left: 0; right: 0; bottom: 0;
          background: #ffffff;
          padding: 20px 24px 80px 24px;
          box-sizing: border-box;
          overflow-y: auto;
          --rboard-gap: 16px;
          --rboard-unit: calc((100vh - 220px) / 6);
        }

        #%s .rboard-present-stage > div {
          max-width: none !important;
          margin: 0 !important;
          padding: 20px 24px !important;
          width: 100%% !important;
          box-sizing: border-box;
        }

        #%s .rboard-present-stage h3 {
          font-size: 1.7rem !important;
          margin-bottom: 16px !important;
        }

        #%s .rboard-present-nav {
          position: fixed;
          bottom: 16px;
          left: 50%%;
          transform: translateX(-50%%);
          display: flex;
          align-items: center;
          gap: 12px;
          background: #ffffff;
          border: 1px solid #cbd5e1;
          border-radius: 40px;
          padding: 6px 16px;
          box-shadow: 0 4px 20px rgba(0,0,0,0.15);
          z-index: 100001;
        }

        #%s .rboard-present-nav button {
          border: none;
          background: transparent;
          padding: 6px 12px;
          font-weight: 500;
          cursor: pointer;
          border-radius: 20px;
          color: #334155;
          font-size: 0.9rem;
        }

        #%s .rboard-present-nav button:hover:not(:disabled) {
          background: #f1f5f9;
        }

        #%s .rboard-present-nav button:disabled {
          opacity: 0.4;
          cursor: not-allowed;
        }

        #%s .rboard-present-nav .compteur {
          font-size: 0.9rem;
          color: #475569;
          font-weight: 600;
          min-width: 50px;
          text-align: center;
        }

        #%s .rboard-present-close {
          position: fixed;
          top: 16px;
          right: 20px;
          z-index: 100002;
          background: #ffffff;
          border: 1px solid #cbd5e1;
          border-radius: 24px;
          padding: 8px 14px;
          font-weight: 500;
          cursor: pointer;
          box-shadow: 0 4px 14px rgba(0,0,0,0.15);
          color: #334155;
          font-size: 0.88rem;
        }

        #%s .rboard-present-close:hover {
          background: #f8fafc;
        }
      ",
        ns("overlay_root"),
        ns("overlay_root"),
        ns("overlay_root"),
        ns("overlay_root"),
        ns("overlay_root"),
        ns("overlay_root"),
        ns("overlay_root"),
        ns("overlay_root"),
        ns("overlay_root"),
        ns("overlay_root"),
        ns("overlay_root")
      )))

      tags$div(
        id = ns("overlay_root"),
        js,
        css,

        tags$button(
          class = "rboard-present-close",
          onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'});",
                            ns("present_quit")),
          bsicons::bs_icon("x-lg"), " Quitter (Echap)"
        ),

        tags$div(
          class = "rboard-present-stage",
          rboard_rendre_page(page, p, prefixe = "present",
                              fond = "#ffffff", ns = ns)
        ),

        tags$div(
          class = "rboard-present-nav",
          tags$button(
            disabled = if (idx <= 1) "disabled" else NULL,
            onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'});",
                              ns("present_prev")),
            bsicons::bs_icon("chevron-left"), " Precedente"
          ),
          tags$span(class = "compteur", sprintf("%d / %d", idx, n)),
          tags$button(
            disabled = if (idx >= n) "disabled" else NULL,
            onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'});",
                              ns("present_next")),
            "Suivante ", bsicons::bs_icon("chevron-right")
          )
        )
      )
    })
  })
}