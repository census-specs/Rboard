#' ==============================================================================
#' Rboard - Module "Donnees" (consultation du jeu de donnees)
#' ==============================================================================

#' Interface du module
#' @export
mod_donnees_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 1400px; margin: 0 auto; padding: 8px 0;",
    uiOutput(ns("contenu"))
  )
}

#' Serveur du module
#' @export
mod_donnees_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    donnees_r <- reactive({
      p <- projet_r()
      if (is.null(p$donnees) || !is.data.frame(p$donnees) || nrow(p$donnees) == 0) {
        return(NULL)
      }
      p$donnees
    })

    # ==========================================================================
    # Contenu principal
    # ==========================================================================
    output$contenu <- renderUI({
      df <- donnees_r()

      if (is.null(df)) {
        return(tagList(
          tags$div(class = "alert alert-info", style = "border-radius: 10px;",
            tags$div(style = "display: flex; align-items: flex-start; gap: 10px;",
              bsicons::bs_icon("info-circle-fill", class = "fs-5"),
              tags$div(
                tags$strong("Aucun jeu de donnees charge"),
                tags$br(),
                "Rendez-vous dans l'onglet ", tags$strong("Import"),
                " pour charger un fichier ou un data.frame de l'environnement R."))),
          rboard_etat_vide("database", "Aucune donnee",
                           "Importez un jeu de donnees pour commencer.")
        ))
      }

      tagList(
        # --- Bandeau de synthese ---
        uiOutput(ns("bandeau_synthese")),

        # --- Carte principale avec le tableau interactif ---
        bslib::card(
          bslib::card_body(
            style = "padding: 18px 22px;",
            tags$div(
              style = "display: flex; align-items: center; justify-content: space-between; gap: 12px; margin-bottom: 14px; flex-wrap: wrap;",
              tags$div(
                style = "display: flex; align-items: center; gap: 10px;",
                bsicons::bs_icon("table", class = "text-primary", style = "font-size: 1.2rem;"),
                tags$h5(style = "margin: 0; font-weight: 600; color: #0f172a;",
                        "Jeu de donnees complet")),
              tags$div(
                style = "display: flex; gap: 8px; align-items: center;",
                tags$span(style = "font-size: 0.82rem; color: #64748b;",
                          bsicons::bs_icon("info-circle"),
                          " Cliquez sur un entete pour trier"),
                actionButton(ns("btn_export_csv"), NULL,
                             icon = bsicons::bs_icon("download"),
                             class = "btn-sm btn-outline-secondary",
                             title = "Telecharger en CSV"))),

            # Wrapper avec classe CSS dediee pour cibler le style
            tags$div(
              class = "rboard-dt-wrapper",
              DT::dataTableOutput(ns("table_donnees"))))),

        # --- Bloc resume des variables ---
        bslib::card(
          style = "margin-top: 20px;",
          bslib::card_body(
            style = "padding: 18px 22px;",
            tags$div(
              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 14px;",
              bsicons::bs_icon("list-columns", class = "text-primary", style = "font-size: 1.2rem;"),
              tags$h5(style = "margin: 0; font-weight: 600; color: #0f172a;",
                      "Resume des variables")),
            uiOutput(ns("resume_variables"))))
      )
    })

    # ==========================================================================
    # Bandeau synthese (4 cartes)
    # ==========================================================================
    output$bandeau_synthese <- renderUI({
      df <- donnees_r()
      if (is.null(df)) return(NULL)

      n_lignes <- nrow(df)
      n_colonnes <- ncol(df)

      taille_octets <- as.numeric(utils::object.size(df))
      taille_lisible <- if (taille_octets < 1024) {
        sprintf("%d o", as.integer(taille_octets))
      } else if (taille_octets < 1024^2) {
        sprintf("%.1f Ko", taille_octets / 1024)
      } else {
        sprintf("%.1f Mo", taille_octets / 1024^2)
      }

      nb_na <- sum(sapply(df, function(x) sum(is.na(x))))

      faire_carte <- function(icone, titre, valeur, couleur_bg, couleur_icone) {
        tags$div(
          style = "padding: 16px 18px; box-sizing: border-box; background: #ffffff; border: 1px solid #e2e8f0; border-radius: 10px; display: flex; align-items: center; gap: 14px;",
          tags$div(
            style = sprintf("width: 44px; height: 44px; min-width: 44px; border-radius: 10px; display: flex; align-items: center; justify-content: center; font-size: 1.3rem; background: %s; color: %s;",
                            couleur_bg, couleur_icone),
            bsicons::bs_icon(icone)),
          tags$div(
            style = "flex: 1; min-width: 0;",
            tags$div(style = "font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.05em; color: #64748b; font-weight: 600; margin-bottom: 3px;", titre),
            tags$div(style = "font-size: 1.25rem; font-weight: 700; color: #0f172a; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;", valeur)))
      }

      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        faire_carte("list-ol", "Lignes", format(n_lignes, big.mark = " "),
                    "#eff6ff", "#2563eb"),
        faire_carte("layout-three-columns", "Colonnes", as.character(n_colonnes),
                    "#ecfdf5", "#16a34a"),
        faire_carte("hdd-stack", "Taille memoire", taille_lisible,
                    "#fff7ed", "#ea580c"),
        faire_carte("exclamation-circle", "Valeurs manquantes",
                    format(nb_na, big.mark = " "),
                    "#fef2f2", "#dc2626"))
    })

    # ==========================================================================
    # Tableau interactif
    # ==========================================================================
    output$table_donnees <- DT::renderDataTable({
      df <- donnees_r()
      req(df)

      df_affichage <- df

      for (col in names(df_affichage)) {
        if (inherits(df_affichage[[col]], "Date") ||
            inherits(df_affichage[[col]], "POSIXct")) {
          df_affichage[[col]] <- as.character(df_affichage[[col]])
        }
        if (is.factor(df_affichage[[col]])) {
          df_affichage[[col]] <- as.character(df_affichage[[col]])
        }
      }

      DT::datatable(
        df_affichage,
        rownames = FALSE,
        filter = "top",
        extensions = c("Scroller"),
        options = list(
          pageLength = 25,
          lengthMenu = list(c(10, 25, 50, 100, -1),
                            c("10", "25", "50", "100", "Tout")),
          scrollX = TRUE,
          scrollY = "500px",
          scroller = TRUE,
          deferRender = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(className = "dt-right", targets = "_all")
          ),
          language = list(
            search = "Rechercher :",
            lengthMenu = "Afficher _MENU_ lignes",
            info = "Lignes _START_ a _END_ sur _TOTAL_",
            infoEmpty = "Aucune ligne",
            infoFiltered = "(filtre sur _MAX_ lignes au total)",
            zeroRecords = "Aucun resultat",
            emptyTable = "Aucune donnee disponible",
            paginate = list(
              first = "Premier",
              previous = "Precedent",
              `next` = "Suivant",
              last = "Dernier"
            )
          )
        ),
        class = "display compact stripe hover rboard-dt"
      )
    })

    # ==========================================================================
    # Resume des variables
    # ==========================================================================
    output$resume_variables <- renderUI({
      df <- donnees_r()
      req(df)

      resume <- tryCatch(resumer_variables(df), error = function(e) NULL)
      if (is.null(resume) || nrow(resume) == 0) {
        return(tags$p(style = "color: #94a3b8; font-style: italic;",
                      "Impossible de calculer le resume."))
      }

      td <- "border: 1px solid #e2e8f0; padding: 8px 12px; color: #334155; vertical-align: middle; font-size: 0.85rem;"
      tdr <- "border: 1px solid #e2e8f0; padding: 8px 12px; color: #334155; vertical-align: middle; text-align: right; font-size: 0.85rem; font-family: 'JetBrains Mono', monospace;"
      th <- "border: 1px solid #cbd5e1; padding: 8px 12px; background: #f1f5f9; font-weight: 600; color: #0f172a; text-align: left; white-space: nowrap; font-size: 0.85rem;"

      lignes <- lapply(seq_len(nrow(resume)), function(i) {
        tags$tr(
          tags$td(style = td,
            tags$span(style = "display: inline-flex; align-items: center; gap: 6px;",
              icone_type_variable(resume$type[i]),
              tags$code(style = "font-size: 0.82rem; background: #f1f5f9; color: #334155; padding: 2px 6px; border-radius: 3px;",
                        resume$variable[i]))),
          tags$td(style = td,
            tags$span(class = "badge bg-light text-dark border",
                      resume$type[i])),
          tags$td(style = tdr, as.character(resume$nb_na[i])),
          tags$td(style = tdr, as.character(resume$nb_uniques[i])),
          tags$td(style = paste0(td, " max-width: 320px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;"),
                  resume$exemple[i]))
      })

      tags$div(
        style = "overflow-x: auto; border-radius: 8px; border: 1px solid #cbd5e1;",
        tags$table(
          style = "border-collapse: collapse; width: 100%;",
          tags$thead(
            tags$tr(
              tags$th(style = th, "Variable"),
              tags$th(style = th, "Type"),
              tags$th(style = paste0(th, " text-align: right;"), "NA"),
              tags$th(style = paste0(th, " text-align: right;"), "Uniques"),
              tags$th(style = th, "Exemple"))),
          tags$tbody(lignes)))
    })

    # ==========================================================================
    # Export CSV
    # ==========================================================================
    output$btn_export_csv <- downloadHandler(
      filename = function() {
        p <- projet_r()
        nom <- if (!is.null(p$nom) && nzchar(p$nom)) {
          gsub("[^a-zA-Z0-9_-]", "_", p$nom)
        } else {
          "donnees_rboard"
        }
        paste0(nom, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        df <- donnees_r()
        req(df)
        utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  })
}