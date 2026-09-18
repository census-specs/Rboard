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
          style = "position: fixed; inset: 0; background: #f8fafc; z-index: 100000; display: flex; align-items: center; justify-content: center;",
          tags$div(style = "text-align: center;",
                   tags$h3("Aucune page a presenter"),
                   actionButton(ns("present_quit"), "Fermer", class = "btn-primary"))
        ))
      }

      idx <- min(max(index_r(), 1L), n)
      page <- pages[[idx]]

      # ======================================================================
      # Script clavier : Echap pour quitter, fleches pour naviguer
      # ======================================================================
      js <- tags$script(HTML(sprintf("
        if (!window.rboard_present_installed) {
          window.rboard_present_installed = true;
          document.addEventListener('keydown', function(e) {
            var root = document.getElementById('%s');
            if (!root) return;
            if (e.key === 'Escape') {
              Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
            }
            if (e.key === 'ArrowLeft') {
              Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
            }
            if (e.key === 'ArrowRight') {
              Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
            }
          });
        }
      ",
        ns("overlay_root"),
        ns("present_quit"),
        ns("present_prev"),
        ns("present_next"))))

      # ======================================================================
      # CSS injecte : force le rendu pleine largeur / pleine hauteur
      # ======================================================================
      css <- tags$style(HTML(sprintf("
        #%s {
          position: fixed;
          inset: 0;
          background: #0f172a;
          z-index: 100000;
          overflow: hidden;
          display: flex;
          flex-direction: column;
        }

        /* Conteneur de la page A4 */
        #%s .rboard-present-stage {
          flex: 1;
          min-height: 0;
          width: 100%%;
          padding: 24px 32px 80px 32px;
          box-sizing: border-box;
          overflow-y: auto;
          overflow-x: hidden;
        }

        /* Neutralise le max-width herite */
        #%s .rboard-present-stage > div {
          max-width: none !important;
          margin: 0 !important;
        }

        /* La page A4 s'etire sur toute la largeur */
        #%s .rboard-present-page {
          width: 100%%;
          max-width: none;
          background: #ffffff;
          border-radius: 10px;
          padding: 28px 32px !important;
          box-sizing: border-box;
          box-shadow: 0 6px 30px rgba(0,0,0,0.35);
        }

        /* La grille interne occupe tout */
        #%s .rboard-present-page > div {
          width: 100%%;
        }

        /* Titre de page plus grand en mode presentation */
        #%s .rboard-present-page h3 {
          font-size: 1.8rem !important;
          margin-bottom: 20px !important;
        }

        /* Barre de navigation flottante */
        #%s .rboard-present-nav {
          position: fixed;
          bottom: 20px;
          left: 50%%;
          transform: translateX(-50%%);
          display: flex;
          align-items: center;
          gap: 14px;
          background: rgba(255,255,255,0.98);
          border: 1px solid #cbd5e1;
          border-radius: 40px;
          padding: 8px 16px;
          box-shadow: 0 8px 24px rgba(0,0,0,0.28);
          z-index: 100001;
        }

        #%s .rboard-present-nav button {
          border: none;
          background: transparent;
          font-weight: 500;
          padding: 6px 12px;
        }

        #%s .rboard-present-nav button:hover:not(:disabled) {
          background: #f1f5f9;
          border-radius: 20px;
        }

        #%s .rboard-present-nav .compteur {
          font-size: 0.9rem;
          color: #475569;
          font-weight: 600;
          min-width: 60px;
          text-align: center;
        }

        #%s .rboard-present-close {
          position: fixed;
          top: 20px;
          right: 24px;
          z-index: 100002;
          background: rgba(255,255,255,0.95);
          border: 1px solid #cbd5e1;
          border-radius: 24px;
          padding: 8px 14px;
          font-weight: 500;
          box-shadow: 0 4px 14px rgba(0,0,0,0.2);
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
        ns("overlay_root"))))

      # ======================================================================
      # Rendu final
      # ======================================================================
      tags$div(
        id = ns("overlay_root"),
        js,
        css,

        # Bouton de fermeture haut-droit
        tags$button(
          class = "rboard-present-close",
          onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'});",
                            ns("present_quit")),
          bsicons::bs_icon("x-lg"), " Quitter (Echap)"
        ),

        # Zone de la page
        tags$div(
          class = "rboard-present-stage",
          tags$div(
            class = "rboard-present-page",
            rboard_rendre_page(page, p, prefixe = "present",
                                fond = "transparent", ns = ns)
          )
        ),

        # Barre de navigation basse
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