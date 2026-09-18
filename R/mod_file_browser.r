#' ==============================================================================
#' Rboard - Navigateur de fichiers plein ecran (CORRIGE)
#' ==============================================================================

#' Interface du navigateur
#' @export
mod_file_browser_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 900px; margin: 0 auto;",
    bslib::card(
      bslib::card_body(
        style = "padding: 24px 28px;",

        # En-tete : titre + annuler
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 18px;",
          tags$h4(
            style = "margin: 0; font-weight: 600; font-size: 1.15rem;",
            bsicons::bs_icon("folder-fill", class = "text-primary"),
            uiOutput(ns("titre_dyn"), inline = TRUE)
          ),
          actionButton(ns("btn_cancel"), "Annuler",
                       icon = bsicons::bs_icon("x-lg"),
                       class = "btn-outline-secondary btn-sm")
        ),

        # Barre chemin + remonter
        tags$div(
          style = "display: flex; gap: 8px; align-items: center; margin-bottom: 14px;",
          actionButton(ns("btn_up"), NULL,
                       icon = bsicons::bs_icon("arrow-up"),
                       class = "btn-outline-secondary btn-sm",
                       title = "Dossier parent"),
          tags$div(
            style = paste0(
              "flex: 1; padding: 8px 12px; background: #f1f5f9; border-radius: 8px; ",
              "font-size: 0.85rem; color: #334155; font-family: monospace; ",
              "overflow: hidden; text-overflow: ellipsis; white-space: nowrap;"
            ),
            uiOutput(ns("chemin_courant"), inline = TRUE)
          )
        ),

        # Listing
        tags$div(
          style = paste0(
            "border: 1px solid #e2e8f0; border-radius: 10px; ",
            "max-height: 420px; overflow-y: auto; background: #ffffff;"
          ),
          uiOutput(ns("listing"))
        ),

        # Zone sauvegarde
        uiOutput(ns("zone_save"))
      )
    )
  )
}

#' Serveur du navigateur
#'
#' @param id Identifiant du module.
#' @param visible_r ReactiveVal logique. TRUE = navigateur affiche.
#' @param selected_r ReactiveVal qui recoit le chemin selectionne.
#' @param mode "open" ou "save".
#' @param filetypes Vecteur d'extensions acceptees (sans le point).
#' @param default_filename Nom de fichier par defaut (mode save).
#' @param initial_dir Dossier de depart.
#' @export
mod_file_browser_server <- function(id, visible_r, selected_r,
                                     mode = "open",
                                     filetypes = NULL,
                                     default_filename = "",
                                     initial_dir = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_dir_r <- reactiveVal(NULL)
    nom_fichier_r <- reactiveVal("")

    # Table de correspondance index -> chemin (evite tout probleme d'echappement)
    map_dirs_r  <- reactiveVal(character(0))
    map_files_r <- reactiveVal(character(0))

    # --- Initialisation a l'ouverture ---
    observeEvent(visible_r(), {
      if (isTRUE(visible_r())) {
        d <- initial_dir
        if (is.null(d) || !dir.exists(d)) {
          d <- tryCatch(path.expand("~"), error = function(e) NULL)
        }
        if (is.null(d) || !dir.exists(d)) d <- getwd()
        current_dir_r(d)
        nom_fichier_r(default_filename)
      }
    }, ignoreInit = FALSE)

    # --- Titre ---
    output$titre_dyn <- renderUI({
      if (identical(mode, "save")) " Enregistrer un fichier" else " Choisir un fichier"
    })

    # --- Chemin courant ---
    output$chemin_courant <- renderUI({
      d <- current_dir_r()
      if (is.null(d)) return("")
      d
    })

    # --- Remonter d'un dossier ---
    observeEvent(input$btn_up, {
      d <- current_dir_r()
      if (is.null(d)) return()
      parent <- dirname(d)
      if (!identical(parent, d) && dir.exists(parent)) {
        current_dir_r(parent)
      }
    })

    # --- Clic sur un dossier : index envoye, serveur fait la correspondance ---
    observeEvent(input$clicked_dir_idx, {
      i <- input$clicked_dir_idx
      if (is.null(i) || i < 1) return()
      dirs <- map_dirs_r()
      if (i > length(dirs)) return()
      p <- dirs[i]
      if (dir.exists(p)) current_dir_r(p)
    }, ignoreInit = TRUE)

    # --- Clic sur un fichier ---
    observeEvent(input$clicked_file_idx, {
      i <- input$clicked_file_idx
      if (is.null(i) || i < 1) return()
      files <- map_files_r()
      if (i > length(files)) return()
      p <- files[i]
      if (!file.exists(p)) return()
      if (identical(mode, "open")) {
        selected_r(p)
        visible_r(FALSE)
      } else {
        nom_fichier_r(basename(p))
      }
    }, ignoreInit = TRUE)

    # --- Listing ---
    output$listing <- renderUI({
      d <- current_dir_r()
      if (is.null(d) || !dir.exists(d)) {
        return(tags$div(
          style = "padding: 30px; text-align: center; color: #94a3b8;",
          "Dossier introuvable"
        ))
      }

      items <- tryCatch(
        list.files(d, all.files = FALSE, full.names = TRUE, no.. = TRUE),
        error = function(e) character(0)
      )

      if (length(items) == 0) {
        map_dirs_r(character(0))
        map_files_r(character(0))
        return(tags$div(
          style = "padding: 30px; text-align: center; color: #94a3b8; font-style: italic;",
          "Ce dossier est vide."
        ))
      }

      is_dir <- dir.exists(items)
      dirs <- sort(items[is_dir])
      files <- items[!is_dir]

      if (!is.null(filetypes) && length(filetypes) > 0) {
        pattern <- paste0("\\.(", paste(filetypes, collapse = "|"), ")$")
        files <- files[grepl(pattern, files, ignore.case = TRUE)]
      }
      files <- sort(files)

      # Memorisation des tables
      map_dirs_r(dirs)
      map_files_r(files)

      item_style <- paste0(
        "display: flex; align-items: center; gap: 10px; ",
        "padding: 10px 16px; cursor: pointer; ",
        "border-bottom: 1px solid #f1f5f9; font-size: 0.9rem; color: #334155; ",
        "transition: background 0.15s ease;"
      )

      # Item avec index (aucun chemin dans le JS -> pas d'echappement necessaire)
      mk_item <- function(i, label, icone, couleur, input_id) {
        tags$div(
          style = item_style,
          onclick = sprintf(
            "Shiny.setInputValue('%s', %d, {priority: 'event'});",
            ns(input_id), i
          ),
          bsicons::bs_icon(icone, style = paste0("color: ", couleur, "; font-size: 1.1rem;")),
          tags$span(label)
        )
      }

      items_ui <- c(
        if (length(dirs) > 0) {
          lapply(seq_along(dirs), function(i) {
            mk_item(i, basename(dirs[i]), "folder-fill", "#2563eb", "clicked_dir_idx")
          })
        },
        if (length(files) > 0) {
          lapply(seq_along(files), function(i) {
            mk_item(i, basename(files[i]), "file-earmark-text", "#64748b", "clicked_file_idx")
          })
        }
      )

      tagList(items_ui)
    })

    # --- Zone de sauvegarde (mode save) ---
    output$zone_save <- renderUI({
      if (!identical(mode, "save")) return(NULL)
      tags$div(
        style = "margin-top: 18px; display: flex; gap: 10px; align-items: flex-end;",
        tags$div(
          style = "flex: 1;",
          textInput(ns("nom_fichier"), "Nom du fichier :",
                    value = nom_fichier_r(),
                    width = "100%")
        ),
        actionButton(ns("btn_save"), "Enregistrer",
                     icon = bsicons::bs_icon("save"),
                     class = "btn-primary",
                     style = "padding: 8px 20px; margin-bottom: 18px;")
      )
    })

    observeEvent(input$btn_save, {
      d <- current_dir_r()
      if (is.null(d)) return()
      nom <- trimws(input$nom_fichier %||% "")
      if (!nzchar(nom)) return()
      full <- file.path(d, nom)
      selected_r(full)
      visible_r(FALSE)
    })

    # --- Annuler ---
    observeEvent(input$btn_cancel, {
      selected_r(NULL)
      visible_r(FALSE)
    })
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a