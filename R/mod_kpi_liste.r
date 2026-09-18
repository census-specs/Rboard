#' ==============================================================================
#' Rboard - Module "Liste des KPI" (bibliotheque)
#' ==============================================================================

#' Interface du module "Liste des KPI"
#' @export
mod_kpi_liste_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 1200px; margin: 0 auto; padding: 8px 0;",
    uiOutput(ns("contenu"))
  )
}

#' Serveur du module "Liste des KPI"
#' @export
mod_kpi_liste_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    message_liste_r <- reactiveVal(NULL)
    kpi_edition_r <- reactiveVal(NULL)
    regles_edit_r <- reactiveVal(list())
    compteur_regles_edit_r <- reactiveVal(0L)
    id_a_supprimer_r <- reactiveVal(NULL)

    # ======================================================================
    # Contenu : liste OU formulaire d'edition
    # ======================================================================
    output$contenu <- renderUI({
      kpi <- kpi_edition_r()
      if (!is.null(kpi)) {
        return(ecran_edition_kpi(kpi))
      }

      tagList(
        uiOutput(ns("alerte_liste")),
        bslib::card(
          style = "margin-bottom: 24px;",
          bslib::card_body(
            style = "padding: 18px 22px;",
            fluidRow(
              column(7, textInput(ns("filtre_recherche"), NULL,
                                   placeholder = "Rechercher par nom, label ou ID...",
                                   width = "100%")),
              column(5, selectInput(ns("filtre_type"), NULL,
                                     choices = c(
                                       "Tous les types"   = "tous",
                                       "Moyenne"          = "moyenne",
                                       "Mediane"          = "mediane",
                                       "Ecart-type"       = "ecart_type",
                                       "Effectif"         = "effectif",
                                       "Frequence"        = "frequence",
                                       "p-value"          = "p_value",
                                       "d de Cohen"       = "d_cohen",
                                       "Correlation"      = "correlation",
                                       "Tableau"          = "tableau",
                                       "Autre"            = "generique"
                                     ), width = "100%"))))),
        uiOutput(ns("grille_cartes"))
      )
    })

    # ======================================================================
    # Formulaire d'edition (ecran)
    # ======================================================================
    ecran_edition_kpi <- function(kpi) {
      bslib::card(
        bslib::card_body(
          style = "padding: 28px 32px;",
          # En-tete avec bouton retour
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
                        " Modifier le KPI"),
                tags$div(style = "font-size: 0.85rem; color: #64748b; margin-top: 4px;",
                         "ID : ", tags$code(kpi$id)))),
            tags$span(class = "badge bg-light text-dark border",
                      style = "font-size: 0.85rem; padding: 8px 12px;",
                      kpi$type)),

          # Rappel valeur
          tags$div(
            style = "padding: 14px 18px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 10px; margin-bottom: 24px;",
            tags$span(style = "color: #64748b;", "Valeur : "),
            tags$strong(style = "color: #0f172a; font-size: 1.05rem;",
                        as.character(kpi$valeur))),

          # Champs
          textInput(ns("modal_nom"), "Nom :", value = kpi$nom, width = "100%"),
          if (!identical(kpi$type, "tableau"))
            textInput(ns("modal_label"), "Label :",
                      value = rboard_or(kpi$label, ""), width = "100%"),
          textAreaInput(ns("modal_commentaire"), "Commentaire :",
                        value = rboard_or(kpi$commentaire, ""),
                        rows = 3, width = "100%"),

          tags$hr(style = "margin: 24px 0 18px 0;"),

          # Regles de couleur
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 14px;",
            tags$strong(style = "font-size: 1rem;",
                        bsicons::bs_icon("palette-fill", class = "text-primary"),
                        " Regles de couleur"),
            tags$div(
              actionButton(ns("modal_btn_ajout_regle"), "Ajouter",
                           icon = bsicons::bs_icon("plus-lg"),
                           class = "btn-sm btn-outline-primary"),
              actionButton(ns("modal_btn_vider_regles"), "Auto",
                           icon = bsicons::bs_icon("arrow-counterclockwise"),
                           class = "btn-sm btn-outline-secondary",
                           style = "margin-left: 8px;"))),
          uiOutput(ns("modal_ui_regles")),

          # Boutons
          tags$div(
            style = "margin-top: 28px; display: flex; gap: 12px; justify-content: flex-end;",
            actionButton(ns("btn_annuler_edit"), "Annuler",
                         icon = bsicons::bs_icon("x-lg"),
                         class = "btn-outline-secondary",
                         style = "padding: 10px 20px;"),
            actionButton(ns("modal_btn_save"), "Enregistrer les modifications",
                         icon = bsicons::bs_icon("check-lg"),
                         class = "btn-primary",
                         style = "padding: 10px 20px;"))
        )
      )
    }

    observeEvent(input$btn_retour,   { kpi_edition_r(NULL) })
    observeEvent(input$btn_annuler_edit, { kpi_edition_r(NULL) })

    # ======================================================================
    # Message
    # ======================================================================
    output$alerte_liste <- renderUI({
      m <- message_liste_r(); if (is.null(m)) return(NULL)
      rboard_alerte(m$type, m$texte)
    })

    # ======================================================================
    # Grille des cartes
    # ======================================================================
    kpis_filtres_r <- reactive({
      p <- projet_r(); kpis <- p$kpi
      if (is.null(kpis) || length(kpis) == 0) return(list())
      type_f <- rboard_or(input$filtre_type, "tous")
      rech <- tolower(trimws(rboard_or(input$filtre_recherche, "")))
      res <- list()
      for (id in names(kpis)) {
        k <- kpis[[id]]; if (is.null(k)) next
        ok_type <- (type_f == "tous") || (k$type == type_f)
        ok_rech <- TRUE
        if (nzchar(rech)) {
          com <- rboard_or(k$commentaire, "")
          txt <- tolower(paste(k$nom, rboard_or(k$label, ""), k$id, com))
          ok_rech <- grepl(rech, txt, fixed = TRUE)
        }
        if (ok_type && ok_rech) res[[id]] <- k
      }
      res
    })

    output$grille_cartes <- renderUI({
      p <- projet_r()
      total <- if (!is.null(p$kpi)) length(p$kpi) else 0
      if (total == 0) {
        return(rboard_etat_vide("inbox", "Aucun KPI dans le projet",
          "Utilisez le menu 'Ajouter un KPI' pour commencer."))
      }
      kpis <- kpis_filtres_r()
      if (length(kpis) == 0) {
        return(rboard_etat_vide("search", "Aucun resultat",
          "Ajustez vos criteres de recherche."))
      }

      cartes <- lapply(kpis, function(k) {
        icone <- icone_type_kpi(k$type)
        couleur <- if (!is.null(k$couleur) && nzchar(k$couleur)) k$couleur else "primary"
        nom_couleur <- rboard_nom_couleur(couleur)

        corps <- if (identical(k$type, "tableau")) {
          tagList(
            tags$h5(style = "font-weight: 600; font-size: 0.95rem; color: #0f172a; margin: 0 0 10px 0;",
                    k$nom),
            rboard_tableau_html(k$tableau, k$regles_couleur, k$colonne_valeur_index,
                                max_rows = 5, compact = TRUE),
            if (!is.null(k$commentaire) && nzchar(as.character(k$commentaire))) {
              tags$p(style = "font-size: 0.83rem; color: #64748b; font-style: italic; margin: 10px 0 0 0;",
                     k$commentaire)
            }
          )
        } else {
          tagList(
            tags$h5(style = "font-weight: 600; font-size: 0.98rem; color: #0f172a; margin: 0 0 6px 0; overflow-wrap: break-word;",
                    k$nom),
            tags$div(style = "font-size: 1.1rem; font-weight: 700; color: #2563eb; margin-bottom: 8px; overflow-wrap: break-word;",
                     rboard_or(k$label, "")),
            if (!is.null(k$commentaire) && nzchar(as.character(k$commentaire))) {
              tags$p(style = "font-size: 0.83rem; color: #64748b; font-style: italic; margin-bottom: 0;",
                     k$commentaire)
            }
          )
        }

        # IDs de clic : on envoie l'ID du KPI + timestamp
        onclick_modif <- sprintf(
          "Shiny.setInputValue('%s', {id: '%s', t: Date.now()}, {priority: 'event'});",
          ns("click_modifier"), gsub("'", "\\\\'", k$id))
        onclick_suppr <- sprintf(
          "Shiny.setInputValue('%s', {id: '%s', t: Date.now()}, {priority: 'event'});",
          ns("click_supprimer"), gsub("'", "\\\\'", k$id))

        bslib::card(
          style = "width: 100%; height: 100%; min-width: 0; box-sizing: border-box; overflow-wrap: break-word;",
          bslib::card_header(
            style = "display: flex; justify-content: space-between; align-items: center; gap: 8px; padding: 10px 14px; min-width: 0;",
            tags$div(
              style = "display: flex; align-items: center; gap: 8px; min-width: 0; flex: 1;",
              tags$code(style = "font-size: 0.82rem; font-weight: 700; background: #e2e8f0; color: #1e293b; padding: 2px 6px; border-radius: 4px; flex-shrink: 0;",
                        k$id),
              tags$span(class = "badge bg-light text-dark border",
                        style = "display: inline-flex; align-items: center; gap: 4px; font-weight: 500; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 100%;",
                        icone,
                        if (identical(k$type, "tableau")) "tableau" else k$type)
            ),
            tags$span(class = paste0("badge bg-", couleur),
                      style = "font-size: 0.72rem; flex-shrink: 0;", nom_couleur)
          ),
          bslib::card_body(style = "padding: 14px 16px; min-width: 0;", corps),
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
            )
          )
        )
      })

      tags$div(
        style = "display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 16px; width: 100%;",
        cartes
      )
    })

    # ======================================================================
    # Clic sur "Modifier" : un seul observeEvent sur l'input envoye par JS
    # ======================================================================
    observeEvent(input$click_modifier, {
      info <- input$click_modifier
      if (is.null(info) || is.null(info$id) || !nzchar(info$id)) return()

      p <- projet_r()
      k <- p$kpi[[info$id]]
      if (is.null(k)) {
        message_liste_r(list(type = "warning", texte = "KPI introuvable."))
        return()
      }

      message_liste_r(NULL)
      kpi_edition_r(k)
      regles_edit_r(if (!is.null(k$regles_couleur)) k$regles_couleur else list())
      compteur_regles_edit_r(length(regles_edit_r()))
    }, ignoreInit = TRUE)

    # ======================================================================
    # Clic sur "Supprimer" : modal de confirmation
    # ======================================================================
    observeEvent(input$click_supprimer, {
      info <- input$click_supprimer
      if (is.null(info) || is.null(info$id) || !nzchar(info$id)) return()

      p <- projet_r()
      k <- p$kpi[[info$id]]
      if (is.null(k)) return()

      id_a_supprimer_r(info$id)
      showModal(modalDialog(
        title = tags$div(
          style = "display: flex; align-items: center; gap: 8px; color: #dc2626;",
          bsicons::bs_icon("exclamation-triangle-fill"), " Confirmer la suppression"
        ),
        tags$p(sprintf("Supprimer '%s' (ID : %s) ?", k$nom, k$id)),
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
      p <- supprimer_kpi(projet_r(), id)
      projet_r(p); removeModal(); id_a_supprimer_r(NULL)
      message_liste_r(list(type = "info", texte = sprintf("KPI %s supprime.", id)))
    })

    # ======================================================================
    # Regles de couleur
    # ======================================================================
    observeEvent(input$modal_btn_ajout_regle, {
      regles <- regles_edit_r()
      n <- compteur_regles_edit_r() + 1L
      compteur_regles_edit_r(n)
      regles[[length(regles) + 1]] <- list(id = n, signe = "<", valeur = 0.05, couleur = "success")
      regles_edit_r(regles)
    })

    observeEvent(input$modal_btn_vider_regles, { regles_edit_r(list()) })

    output$modal_ui_regles <- renderUI({
      regles <- regles_edit_r()
      if (length(regles) == 0) {
        return(tags$div(style = "padding: 12px 16px; background: #f1f5f9; border-radius: 8px; color: #64748b; font-size: 0.9rem;",
                        "Aucune regle. La couleur metier s'applique."))
      }
      lapply(seq_along(regles), function(i) {
        r <- regles[[i]]
        rboard_regle_ui(ns, r$id, r$signe, r$valeur, r$couleur)
      })
    })

    observe({
      regles <- regles_edit_r()
      for (r in regles) {
        id <- r$id
        btn_id <- paste0("mregle_del_", id)
        if (!is.null(input[[btn_id]]) && input[[btn_id]] > 0) {
          local({
            my_id <- id
            regles <- regles_edit_r()
            regles <- Filter(function(x) x$id != my_id, regles)
            regles_edit_r(regles)
          })
          break
        }
      }
    })

    # ======================================================================
    # Enregistrer les modifications
    # ======================================================================
    observeEvent(input$modal_btn_save, {
      k <- kpi_edition_r(); if (is.null(k)) return()
      nom <- trimws(rboard_or(input$modal_nom, ""))
      com <- trimws(rboard_or(input$modal_commentaire, ""))
      if (nzchar(nom)) k$nom <- nom
      k$commentaire <- if (nzchar(com)) com else NULL
      if (!identical(k$type, "tableau")) {
        label <- trimws(rboard_or(input$modal_label, ""))
        if (nzchar(label)) k$label <- label
      }
      regles <- regles_edit_r()
      regles_finales <- lapply(regles, function(r) {
        id <- r$id
        list(
          signe   = rboard_or(input[[paste0("mregle_signe_", id)]], r$signe),
          valeur  = rboard_or(input[[paste0("mregle_valeur_", id)]], r$valeur),
          couleur = rboard_or(input[[paste0("mregle_couleur_", id)]], r$couleur)
        )
      })
      k$regles_couleur <- regles_finales
      if (!identical(k$type, "tableau")) k$couleur <- decider_couleur(k)
      p <- projet_r(); p$kpi[[k$id]] <- k; projet_r(p)

      kpi_edition_r(NULL)
      message_liste_r(list(type = "success", texte = sprintf("KPI '%s' mis a jour.", k$nom)))
    })
  })
}

