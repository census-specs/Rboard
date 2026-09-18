#' ==============================================================================
#' Rboard - Module "Bibliotheque de textes"
#' ==============================================================================

# --- Labels et icones pour les styles de texte ---
rboard_libelle_style_texte <- function(style) {
  stl <- as.character(style %||% "normal")[1]
  if (is.na(stl) || !nzchar(stl)) stl <- "normal"
  switch(stl,
    "titre"         = list(libelle = "Titre", icone = "type-h1", couleur = "#2563eb"),
    "note"          = list(libelle = "Note", icone = "info-circle-fill", couleur = "#0891b2"),
    "avertissement" = list(libelle = "Avertissement", icone = "exclamation-triangle-fill", couleur = "#ea580c"),
    list(libelle = "Normal", icone = "text-paragraph", couleur = "#64748b"))
}

#' Interface du module
#' @export
mod_texte_liste_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 1200px; margin: 0 auto; padding: 8px 0;",
    uiOutput(ns("contenu"))
  )
}

#' Serveur du module
#' @export
mod_texte_liste_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    message_liste_r <- reactiveVal(NULL)
    texte_edition_r <- reactiveVal(NULL)
    id_a_supprimer_r <- reactiveVal(NULL)

    # ==========================================================================
    # Contenu : liste OU formulaire d'edition
    # ==========================================================================
    output$contenu <- renderUI({
      txt <- texte_edition_r()
      if (!is.null(txt)) {
        return(ecran_edition_texte(txt))
      }

      tagList(
        uiOutput(ns("alerte_liste")),
        bslib::card(
          style = "margin-bottom: 24px;",
          bslib::card_body(
            style = "padding: 18px 22px;",
            fluidRow(
              column(7, textInput(ns("filtre_recherche"), NULL,
                                   placeholder = "Rechercher par nom ou contenu...",
                                   width = "100%")),
              column(5, selectInput(ns("filtre_style"), NULL,
                                     choices = c("Tous les styles" = "tous",
                                                 "Normal" = "normal",
                                                 "Titre" = "titre",
                                                 "Note" = "note",
                                                 "Avertissement" = "avertissement"),
                                     width = "100%"))))),
        uiOutput(ns("grille_cartes"))
      )
    })

    # ==========================================================================
    # Ecran d'edition
    # ==========================================================================
    ecran_edition_texte <- function(txt) {
      style_actuel <- as.character(txt$style %||% "normal")[1]
      if (is.na(style_actuel) || !nzchar(style_actuel)) style_actuel <- "normal"

      niveau_actuel <- 2L

      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          bslib::card_body(
            style = "padding: 28px 32px;",
            tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 24px;",
              tags$div(
                style = "display: flex; align-items: center; gap: 12px;",
                actionButton(ns("btn_retour"), NULL,
                             icon = bsicons::bs_icon("arrow-left"),
                             class = "btn-outline-secondary"),
                tags$div(
                  tags$h4(style = "margin: 0; font-weight: 600; font-size: 1.25rem;",
                          bsicons::bs_icon("pencil-square", class = "text-primary"),
                          " Modifier le texte"),
                  tags$div(style = "font-size: 0.85rem; color: #64748b; margin-top: 4px;",
                           "ID : ", tags$code(txt$id)))),

              tags$span(class = "badge bg-light text-dark border",
                        style = "font-size: 0.85rem; padding: 8px 12px;",
                        style_actuel)),

            textInput(ns("edit_nom"), "Nom :",
                      value = as.character(txt$nom %||% "")[1], width = "100%"),

            selectInput(ns("edit_style"), "Style :",
                        choices = c("Normal" = "normal",
                                    "Titre" = "titre",
                                    "Note" = "note",
                                    "Avertissement" = "avertissement"),
                        selected = style_actuel, width = "100%"),

            uiOutput(ns("ui_niveau_titre")),

            textAreaInput(ns("edit_contenu"),
                          "Contenu (Markdown supporte) :",
                          value = as.character(txt$contenu %||% "")[1],
                          rows = 8, width = "100%"),

            tags$div(
              style = "margin-top: 28px; display: flex; gap: 12px; justify-content: flex-end;",
              actionButton(ns("btn_annuler_edit"), "Annuler",
                           icon = bsicons::bs_icon("x-lg"),
                           class = "btn-outline-secondary",
                           style = "padding: 10px 20px;"),
              actionButton(ns("btn_save"), "Enregistrer les modifications",
                           icon = bsicons::bs_icon("check-lg"),
                           class = "btn-primary",
                           style = "padding: 10px 20px;")))
        ),

        bslib::card(
          bslib::card_body(
            style = "padding: 16px;",
            tags$div(style = "font-weight: 600; color: #0f172a; margin-bottom: 12px;",
                     bsicons::bs_icon("eye-fill", class = "text-primary"),
                     " Apercu"),
            uiOutput(ns("apercu_edit_texte"))))
      )
    }

    output$ui_niveau_titre <- renderUI({
      req(input$edit_style)
      if (!identical(input$edit_style, "titre")) return(NULL)

      selectInput(ns("edit_niveau_titre"), "Niveau de titre :",
                  choices = c("H1 - Grand" = 1L,
                              "H2 - Moyen" = 2L,
                              "H3 - Petit" = 3L),
                  selected = 2L, width = "100%")
    })

    output$apercu_edit_texte <- renderUI({
      txt <- texte_edition_r()
      if (is.null(txt)) return(NULL)

      # Apercu construit a partir des inputs en temps reel
      contenu_actuel <- if (!is.null(input$edit_contenu)) {
        input$edit_contenu
      } else {
        as.character(txt$contenu %||% "")[1]
      }

      style_actuel <- if (!is.null(input$edit_style)) {
        input$edit_style
      } else {
        as.character(txt$style %||% "normal")[1]
      }

      fake_elem <- list(
        id = txt$id,
        type = "texte",
        contenu = contenu_actuel,
        style = style_actuel,
        niveau_titre = if (!is.null(input$edit_niveau_titre)) {
          suppressWarnings(as.integer(input$edit_niveau_titre)[1])
        } else {
          2L
        }
      )

      rboard_rendre_texte(fake_elem, projet = NULL, taille = 12, hauteur_unites = 2L)
    })

    observeEvent(input$btn_retour,      { texte_edition_r(NULL) })
    observeEvent(input$btn_annuler_edit, { texte_edition_r(NULL) })

    # ==========================================================================
    # Message
    # ==========================================================================
    output$alerte_liste <- renderUI({
      m <- message_liste_r(); if (is.null(m)) return(NULL)
      rboard_alerte(m$type, m$texte)
    })

    # ==========================================================================
    # Grille des cartes
    # ==========================================================================
    textes_filtres_r <- reactive({
      p <- projet_r()
      textes <- p$textes
      if (is.null(textes) || length(textes) == 0) return(list())

      style_f <- rboard_or(input$filtre_style, "tous")
      rech <- tolower(trimws(rboard_or(input$filtre_recherche, "")))

      res <- list()
      for (id in names(textes)) {
        t <- textes[[id]]
        if (is.null(t)) next

        stl <- as.character(t$style %||% "normal")[1]
        if (is.na(stl) || !nzchar(stl)) stl <- "normal"

        ok_style <- (style_f == "tous") || identical(stl, style_f)
        ok_rech <- TRUE
        if (nzchar(rech)) {
          txt_contenu <- tolower(as.character(t$contenu %||% "")[1])
          txt_nom <- tolower(as.character(t$nom %||% "")[1])
          txt <- paste(txt_nom, txt_contenu, id)
          ok_rech <- grepl(rech, txt, fixed = TRUE)
        }
        if (ok_style && ok_rech) res[[id]] <- t
      }
      res
    })

    output$grille_cartes <- renderUI({
      p <- projet_r()
      total <- if (!is.null(p$textes)) length(p$textes) else 0
      if (total == 0) {
        return(rboard_etat_vide("journal-text", "Aucun texte dans la bibliotheque",
          "Utilisez le Script R pour importer des textes markdown, ou saisissez-en depuis le Dashboard."))
      }
      textes <- textes_filtres_r()
      if (length(textes) == 0) {
        return(rboard_etat_vide("search", "Aucun resultat",
          "Ajustez vos criteres de recherche."))
      }

      cartes <- lapply(textes, function(t) {
        stl <- as.character(t$style %||% "normal")[1]
        if (is.na(stl) || !nzchar(stl)) stl <- "normal"
        infos <- rboard_libelle_style_texte(stl)

        apercu <- as.character(t$contenu %||% "")[1]
        if (nchar(apercu) > 180) apercu <- paste0(substr(apercu, 1, 177), "...")

        onclick_modif <- sprintf(
          "Shiny.setInputValue('%s', {id: '%s', t: Date.now()}, {priority: 'event'});",
          ns("click_modifier"), gsub("'", "\\\\'", t$id))
        onclick_suppr <- sprintf(
          "Shiny.setInputValue('%s', {id: '%s', t: Date.now()}, {priority: 'event'});",
          ns("click_supprimer"), gsub("'", "\\\\'", t$id))

        bslib::card(
          style = "width: 100%; height: 100%; min-width: 0; box-sizing: border-box; overflow-wrap: break-word;",
          bslib::card_header(
            style = "display: flex; justify-content: space-between; align-items: center; gap: 8px; padding: 10px 14px; min-width: 0;",
            tags$div(
              style = "display: flex; align-items: center; gap: 8px; min-width: 0; flex: 1;",
              tags$code(style = "font-size: 0.82rem; font-weight: 700; background: #e2e8f0; color: #1e293b; padding: 2px 6px; border-radius: 4px; flex-shrink: 0;",
                        t$id),
              tags$span(class = "badge",
                        style = sprintf("background: %s; color: #ffffff; font-weight: 500; display: inline-flex; align-items: center; gap: 4px; font-size: 0.72rem;",
                                        infos$couleur),
                        bsicons::bs_icon(infos$icone), infos$libelle))),
          bslib::card_body(style = "padding: 14px 16px; min-width: 0;",
            tags$h5(style = "font-weight: 600; font-size: 0.98rem; color: #0f172a; margin: 0 0 8px 0; overflow-wrap: break-word;",
                    as.character(t$nom %||% "Texte")[1]),
            tags$div(
              style = "font-size: 0.85rem; color: #475569; line-height: 1.4; white-space: pre-wrap; overflow-wrap: break-word; max-height: 120px; overflow: hidden;",
              apercu)),
          bslib::card_footer(
            style = "padding: 10px 14px; background: #ffffff; border-top: 1px solid #f1f5f9; display: flex; gap: 8px;",
            tags$button(
              class = "btn btn-sm btn-outline-primary",
              style = "flex: 1;",
              onclick = onclick_modif,
              bsicons::bs_icon("pencil-square"), " Modifier"
            ),
            tags$button(
              class = "btn btn-sm btn-outline-danger",
              style = "flex: 1;",
              onclick = onclick_suppr,
              bsicons::bs_icon("trash"), " Supprimer"
            )))
      })

      tags$div(
        style = "display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 16px; width: 100%;",
        cartes
      )
    })

    # ==========================================================================
    # Clic Modifier
    # ==========================================================================
    observeEvent(input$click_modifier, {
      info <- input$click_modifier
      if (is.null(info) || is.null(info$id) || !nzchar(info$id)) return()

      p <- projet_r()
      t <- p$textes[[info$id]]
      if (is.null(t)) {
        message_liste_r(list(type = "warning", texte = "Texte introuvable."))
        return()
      }

      message_liste_r(NULL)
      texte_edition_r(t)
    }, ignoreInit = TRUE)

    # ==========================================================================
    # Clic Supprimer
    # ==========================================================================
    observeEvent(input$click_supprimer, {
      info <- input$click_supprimer
      if (is.null(info) || is.null(info$id) || !nzchar(info$id)) return()

      p <- projet_r()
      t <- p$textes[[info$id]]
      if (is.null(t)) return()

      id_a_supprimer_r(info$id)
      showModal(modalDialog(
        title = tags$div(
          style = "display: flex; align-items: center; gap: 8px; color: #dc2626;",
          bsicons::bs_icon("exclamation-triangle-fill"), " Confirmer la suppression"
        ),
        tags$p(sprintf("Supprimer '%s' (ID : %s) ?",
                       as.character(t$nom %||% "Texte")[1], t$id)),
        tags$p(style = "font-size: 0.85rem; color: #64748b;",
               "Les elements de dashboard qui referencent ce texte utiliseront leur copie de secours."),
        footer = tagList(
          modalButton("Annuler"),
          actionButton(ns("modal_btn_confirm_suppr"), "Supprimer",
                       class = "btn-danger", icon = bsicons::bs_icon("trash-fill"))
        ),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$modal_btn_confirm_suppr, {
      id <- id_a_supprimer_r(); if (is.null(id)) return()
      p <- projet_r()
      if (!is.null(p$textes) && !is.null(p$textes[[id]])) {
        p$textes[[id]] <- NULL
        p$date_modification <- Sys.Date()
        projet_r(p)
      }
      removeModal()
      id_a_supprimer_r(NULL)
      message_liste_r(list(type = "info", texte = sprintf("Texte %s supprime.", id)))
    })

    # ==========================================================================
    # Enregistrer
    # ==========================================================================
    observeEvent(input$btn_save, {
      txt <- texte_edition_r(); if (is.null(txt)) return()

      nom <- trimws(as.character(input$edit_nom %||% "")[1])
      if (!nzchar(nom)) nom <- "Texte"

      contenu <- as.character(input$edit_contenu %||% "")[1]
      if (!nzchar(trimws(contenu))) {
        showNotification("Le contenu ne peut pas etre vide.", type = "error", duration = 5)
        return()
      }

      style <- as.character(input$edit_style %||% "normal")[1]
      if (is.na(style) || !nzchar(style)) style <- "normal"

      niveau_val <- NULL
      if (identical(style, "titre")) {
        nt <- suppressWarnings(as.integer(input$edit_niveau_titre %||% 2L)[1])
        if (is.na(nt) || !(nt %in% c(1L, 2L, 3L))) nt <- 2L
        niveau_val <- nt
      }

      p <- projet_r()
      if (is.null(p$textes) || is.null(p$textes[[txt$id]])) {
        showNotification("Texte introuvable.", type = "error", duration = 5)
        return()
      }

      p$textes[[txt$id]]$nom <- nom
      p$textes[[txt$id]]$contenu <- contenu
      p$textes[[txt$id]]$style <- style

      # Pour les titres : niveau_titre n'est pas stocke dans projet$textes, il est
      # deduit dans rboard_rendre_texte a partir de l'element dashboard.
      # On le stocke ici pour permettre un rendu a partir de la bibliotheque.
      if (!is.null(niveau_val)) {
        p$textes[[txt$id]]$niveau_titre <- niveau_val
      } else {
        p$textes[[txt$id]]$niveau_titre <- NULL
      }

      p$date_modification <- Sys.Date()
      projet_r(p)

      texte_edition_r(NULL)
      message_liste_r(list(type = "success",
                           texte = sprintf("Texte '%s' mis a jour.", nom)))
    })
  })
}