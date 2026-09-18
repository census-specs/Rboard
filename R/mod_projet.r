#' ==============================================================================
#' Rboard - Module Projet (4 sections independantes)
#' ==============================================================================

rboard_value_box_stat <- function(icone, titre, valeur, couleur_bg, couleur_icone) {
  shiny::tags$div(
    style = paste0("padding: 20px; box-sizing: border-box; background: #ffffff; ",
                   "border: 1px solid #e2e8f0; border-radius: 10px; ",
                   "display: flex; align-items: center; gap: 16px;"),
    shiny::tags$div(
      style = paste0("width: 52px; height: 52px; min-width: 52px; border-radius: 10px; ",
                     "display: flex; align-items: center; justify-content: center; ",
                     "font-size: 1.5rem; background: ", couleur_bg, "; color: ", couleur_icone, ";"),
      bsicons::bs_icon(icone)),
    shiny::tags$div(
      style = "flex: 1; min-width: 0;",
      shiny::tags$div(style = paste0("font-size: 0.75rem; text-transform: uppercase; ",
                                     "letter-spacing: 0.06em; color: #64748b; font-weight: 600; ",
                                     "margin-bottom: 4px;"), titre),
      shiny::tags$div(style = paste0("font-size: 1.4rem; font-weight: 700; color: #0f172a; ",
                                     "overflow: hidden; text-overflow: ellipsis; white-space: nowrap;"),
                      valeur))
  )
}

# ==============================================================================
# SECTION 1 : Vue d'ensemble
# ==============================================================================

mod_projet_vue_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 1000px; margin: 0 auto;",
    uiOutput(ns("entete")),
    tags$div(style = "margin-top: 24px;", uiOutput(ns("stats")))
  )
}

mod_projet_vue_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mode_edition_r <- reactiveVal(FALSE)

    output$entete <- renderUI({
      p <- projet_r()
      nom <- if (!is.null(p$nom)) p$nom else "Sans titre"

      bslib::card(
        bslib::card_body(
          style = "padding: 24px 28px;",
          if (!isTRUE(mode_edition_r())) {
            tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; gap: 16px;",
              tags$div(
                style = "flex: 1; min-width: 0;",
                tags$h2(
                  style = "margin: 0 0 10px 0; font-size: 1.6rem; font-weight: 700; color: #0f172a;",
                  nom),
                tags$div(
                  style = "font-size: 0.9rem; color: #64748b; display: flex; gap: 24px; flex-wrap: wrap;",
                  tags$span(bsicons::bs_icon("calendar-plus"), " Cree le ",
                            format(p$date_creation, "%d/%m/%Y")),
                  tags$span(bsicons::bs_icon("clock-history"), " Modifie le ",
                            format(p$date_modification, "%d/%m/%Y"))
                )
              ),
              actionButton(
                ns("btn_edition"), "Renommer",
                icon = bsicons::bs_icon("pencil-square"),
                class = "btn-outline-primary"
              )
            )
          } else {
            tags$div(
              style = "display: flex; gap: 8px; align-items: flex-start;",
              tags$div(
                style = "flex: 1;",
                textInput(ns("nom_edition"), NULL, value = nom, width = "100%")
              ),
              tags$div(
                style = "display: flex; gap: 6px;",
                actionButton(
                  ns("btn_valider"), NULL,
                  icon = bsicons::bs_icon("check-lg"),
                  class = "btn-primary",
                  title = "Valider"
                ),
                actionButton(
                  ns("btn_annuler"), NULL,
                  icon = bsicons::bs_icon("x-lg"),
                  class = "btn-outline-secondary",
                  title = "Annuler"
                )
              )
            )
          }
        )
      )
    })

    observeEvent(input$btn_edition, { mode_edition_r(TRUE) })
    observeEvent(input$btn_annuler,  { mode_edition_r(FALSE) })
    observeEvent(input$btn_valider, {
      nouveau <- trimws(input$nom_edition %||% "")
      if (nzchar(nouveau)) {
        p <- projet_r()
        p$nom <- nouveau
        p$date_modification <- Sys.Date()
        projet_r(p)
      }
      mode_edition_r(FALSE)
    })

    output$stats <- renderUI({
      p <- projet_r()
      val_donnees <- if (!is.null(p$donnees) && is.data.frame(p$donnees)) {
        paste0(nrow(p$donnees), " x ", ncol(p$donnees))
      } else "-"
      nb_kpi <- if (!is.null(p$kpi)) length(p$kpi) else 0
      nb_graph <- if (!is.null(p$graphiques)) length(p$graphiques) else 0
      nb_pages <- if (!is.null(p$dashboard) && !is.null(p$dashboard$pages)) {
        length(p$dashboard$pages)
      } else 0

      fluidRow(
        column(3, rboard_value_box_stat("database",        "Donnees",    val_donnees,            "#eff6ff", "#2563eb")),
        column(3, rboard_value_box_stat("calculator-fill", "KPI",        as.character(nb_kpi),   "#ecfdf5", "#16a34a")),
        column(3, rboard_value_box_stat("bar-chart-fill",  "Graphiques", as.character(nb_graph), "#fff7ed", "#ea580c")),
        column(3, rboard_value_box_stat("grid-1x2-fill",   "Pages",      as.character(nb_pages), "#f5f3ff", "#7c3aed"))
      )
    })
  })
}

# ==============================================================================
# SECTION 2 : Sauvegarder
# ==============================================================================

mod_projet_save_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 1000px; margin: 0 auto;",
    uiOutput(ns("contenu"))
  )
}

mod_projet_save_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    show_browser_r <- reactiveVal(FALSE)
    selected_r     <- reactiveVal(NULL)
    chemin_r       <- reactiveVal(NULL)
    msg_r          <- reactiveVal(NULL)

    mod_file_browser_server("browser", show_browser_r, selected_r,
                             mode = "save", filetypes = "rds",
                             default_filename = "mon_projet.rds")

    observeEvent(selected_r(), {
      chemin <- selected_r()
      if (!is.null(chemin) && nzchar(chemin)) {
        if (!grepl("\\.rds$", chemin, ignore.case = TRUE)) {
          chemin <- paste0(chemin, ".rds")
        }
        p <- projet_r()
        res <- tryCatch({
          sauver_projet(p, chemin)
          p$date_modification <- Sys.Date()
          projet_r(p)
          chemin_r(chemin)
          list(type = "success", texte = sprintf("Projet sauvegarde : %s", basename(chemin)))
        }, error = function(e) {
          list(type = "danger", texte = sprintf("Erreur : %s", e$message))
        })
        msg_r(res)
      }
      selected_r(NULL)
    })

    output$contenu <- renderUI({
      if (isTRUE(show_browser_r())) {
        return(mod_file_browser_ui(ns("browser")))
      }

      tagList(
        bslib::card(
          bslib::card_body(
            style = "padding: 28px 32px;",
            tags$h4(
              style = "margin: 0 0 10px 0; font-size: 1.15rem; font-weight: 600;",
              bsicons::bs_icon("save", class = "text-primary"),
              " Enregistrer le projet"
            ),
            tags$p(
              style = "color: #64748b; margin-bottom: 24px;",
              "Enregistre l'integralite du projet dans un fichier .rds."
            ),
            actionButton(
              ns("btn_parcourir"), "Parcourir et sauvegarder",
              icon = bsicons::bs_icon("folder-fill"),
              class = "btn-primary"
            ),
            uiOutput(ns("info_chemin"), style = "margin-top: 20px;")
          )
        ),
        if (!is.null(msg_r())) {
          tags$div(style = "margin-top: 18px;",
                   rboard_alerte(msg_r()$type, msg_r()$texte))
        }
      )
    })

    observeEvent(input$btn_parcourir, { show_browser_r(TRUE) })

    output$info_chemin <- renderUI({
      c <- chemin_r()
      if (is.null(c)) return(NULL)
      tags$div(
        style = "font-size: 0.9rem; color: #475569;",
        bsicons::bs_icon("check-circle-fill", class = "text-success"),
        " Dernier fichier : ",
        tags$code(style = "color: #2563eb;", c)
      )
    })
  })
}

# ==============================================================================
# SECTION 3 : Charger
# ==============================================================================

mod_projet_load_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 1000px; margin: 0 auto;",
    uiOutput(ns("contenu"))
  )
}

mod_projet_load_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    show_browser_r <- reactiveVal(FALSE)
    selected_r     <- reactiveVal(NULL)
    chemin_r       <- reactiveVal(NULL)
    msg_r          <- reactiveVal(NULL)

    mod_file_browser_server("browser", show_browser_r, selected_r,
                             mode = "open", filetypes = "rds")

    observeEvent(selected_r(), {
      chemin <- selected_r()
      if (!is.null(chemin) && nzchar(chemin) && file.exists(chemin)) {
        chemin_r(chemin)
        showModal(modalDialog(
          title = tags$div(
            style = "display: flex; align-items: center; gap: 8px;",
            bsicons::bs_icon("exclamation-triangle-fill", class = "text-warning"),
            " Confirmer le chargement"
          ),
          tags$p("Charger ce projet remplacera entierement le projet en cours."),
          tags$p(style = "font-size: 0.88rem; color: #64748b;",
                 "Fichier : ", tags$code(basename(chemin))),
          footer = tagList(
            modalButton("Annuler"),
            actionButton(ns("btn_confirmer"), "Charger", class = "btn-primary",
                         icon = bsicons::bs_icon("folder2-open"))
          ),
          easyClose = TRUE
        ))
      }
      selected_r(NULL)
    })

    observeEvent(input$btn_confirmer, {
      chemin <- chemin_r()
      if (is.null(chemin)) return()
      removeModal()
      res <- tryCatch({
        p_charge <- charger_projet(chemin)
        if (is.null(p_charge$dashboard) || length(p_charge$dashboard) == 0) {
          p_charge$dashboard <- creer_dashboard()
          p_charge$dashboard <- ajouter_page(p_charge$dashboard, creer_page("Page 1"))
        }
        projet_r(p_charge)
        list(type = "success", texte = sprintf("Projet '%s' charge.", p_charge$nom))
      }, error = function(e) {
        list(type = "danger",
             texte = sprintf("Impossible de charger : %s", e$message))
      })
      msg_r(res)
      chemin_r(NULL)
    })

    output$contenu <- renderUI({
      if (isTRUE(show_browser_r())) {
        return(mod_file_browser_ui(ns("browser")))
      }

      tagList(
        bslib::card(
          bslib::card_body(
            style = "padding: 28px 32px;",
            tags$h4(
              style = "margin: 0 0 10px 0; font-size: 1.15rem; font-weight: 600;",
              bsicons::bs_icon("folder2-open", class = "text-primary"),
              " Charger un projet"
            ),
            tags$p(
              style = "color: #64748b; margin-bottom: 24px;",
              "Selectionnez un fichier .rds. Le projet en cours sera remplace."
            ),
            actionButton(
              ns("btn_parcourir"), "Parcourir et charger",
              icon = bsicons::bs_icon("folder-fill"),
              class = "btn-primary"
            )
          )
        ),
        if (!is.null(msg_r())) {
          tags$div(style = "margin-top: 18px;",
                   rboard_alerte(msg_r()$type, msg_r()$texte))
        }
      )
    })

    observeEvent(input$btn_parcourir, { show_browser_r(TRUE) })
  })
}

# ==============================================================================
# SECTION 4 : Nouveau projet
# ==============================================================================

mod_projet_new_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 1000px; margin: 0 auto;",
    bslib::card(
      bslib::card_body(
        style = "padding: 28px 32px;",
        tags$h4(
          style = "margin: 0 0 10px 0; font-size: 1.15rem; font-weight: 600;",
          bsicons::bs_icon("file-earmark-plus", class = "text-primary"),
          " Creer un nouveau projet"
        ),
        tags$p(
          style = "color: #64748b; margin-bottom: 24px;",
          "Cree un projet vide. Une confirmation vous sera demandee."
        ),
        actionButton(
          ns("btn_nouveau"), "Creer un projet vierge",
          icon = bsicons::bs_icon("file-earmark-plus"),
          class = "btn-danger"
        ),
        uiOutput(ns("message"))
      )
    )
  )
}

mod_projet_new_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    msg_r <- reactiveVal(NULL)

    output$message <- renderUI({
      m <- msg_r()
      if (is.null(m)) return(NULL)
      tags$div(style = "margin-top: 18px;", rboard_alerte(m$type, m$texte))
    })

    observeEvent(input$btn_nouveau, {
      showModal(modalDialog(
        title = tags$div(
          style = "display: flex; align-items: center; gap: 8px;",
          bsicons::bs_icon("exclamation-triangle-fill", class = "text-danger"),
          " Confirmer la creation"
        ),
        tags$p("Cette action efface definitivement le projet en cours."),
        tags$p(style = "font-size: 0.88rem; color: #64748b;",
               "Pensez a sauvegarder si necessaire."),
        footer = tagList(
          modalButton("Annuler"),
          actionButton(ns("btn_confirmer"), "Creer un projet vierge",
                       class = "btn-danger",
                       icon = bsicons::bs_icon("file-earmark-plus"))
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_confirmer, {
      removeModal()
      res <- tryCatch({
        p <- nouveau_projet()
        p$dashboard <- creer_dashboard()
        p$dashboard <- ajouter_page(p$dashboard, creer_page("Page 1"))
        projet_r(p)
        list(type = "success", texte = "Nouveau projet vierge initialise.")
      }, error = function(e) {
        list(type = "danger", texte = sprintf("Erreur : %s", e$message))
      })
      msg_r(res)
    })
  })
}

