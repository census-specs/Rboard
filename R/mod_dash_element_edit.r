#' ==============================================================================
#' Rboard - Ecran d'edition d'un element du dashboard
#' ==============================================================================

mod_dash_element_edit_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ecran"))
}

mod_dash_element_edit_server <- function(id, projet_r, element_edition_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    format_tableau_r <- reactiveVal(NULL)
    format_graph_r   <- reactiveVal(NULL)
    source_texte_r   <- reactiveVal(NULL)

    observeEvent(element_edition_r(), {
      edit <- element_edition_r()
      if (is.null(edit) || is.null(edit$element)) {
        format_tableau_r(NULL); format_graph_r(NULL); source_texte_r(NULL)
        return()
      }
      el <- edit$element
      type_el <- as.character(el$type)[1]
      p <- projet_r()

      if (identical(type_el, "kpi")) {
        kpi_el <- p$kpi[[el$id_kpi]]
        est_tableau <- !is.null(kpi_el) &&
                       identical(as.character(kpi_el$type)[1], "tableau")
        if (est_tableau) {
          fmt <- if (isTRUE(el$tableau_long)) "long"
                 else if (isTRUE(el$tableau_etendu)) "etendu"
                 else "compact"
          format_tableau_r(fmt)
        } else format_tableau_r(NULL)
        format_graph_r(NULL); source_texte_r(NULL)

      } else if (identical(type_el, "graphique")) {
        format_graph_r(if (isTRUE(el$graphique_long)) "grand" else "standard")
        format_tableau_r(NULL); source_texte_r(NULL)

      } else if (identical(type_el, "texte")) {
        src <- if (!is.null(el$id_texte) && nzchar(as.character(el$id_texte)[1])) {
          "bibliotheque"
        } else "libre"
        source_texte_r(src)
        format_tableau_r(NULL); format_graph_r(NULL)
      }
    }, ignoreNULL = FALSE)

    contexte_edit_r <- reactive({
      edit <- element_edition_r()
      if (is.null(edit)) return(NULL)
      el <- edit$element
      if (is.null(el)) return(NULL)

      p <- projet_r()
      ligne <- obtenir_ligne(p$dashboard, edit$id_page, edit$id_ligne)

      largeur_autres <- 0L
      if (!is.null(ligne) && !is.null(ligne$elements)) {
        for (e in ligne$elements) {
          if (!identical(e$id, edit$id_element)) {
            largeur_autres <- largeur_autres + rboard_largeur_element(e)
          }
        }
      }
      largeur_autres <- as.integer(largeur_autres)
      largeur_max <- as.integer(12L - largeur_autres)
      if (is.na(largeur_max) || largeur_max < 0L) largeur_max <- 0L

      h_page_actuelle <- hauteur_utilisee_page(p$dashboard, edit$id_page)
      h_ligne_actuelle <- rboard_max_hauteurs(ligne$elements)
      h_autres_lignes <- h_page_actuelle - h_ligne_actuelle
      hauteur_max <- as.integer(RBOARD_GRILLE_HAUTEUR - h_autres_lignes)
      if (is.na(hauteur_max) || hauteur_max < 0L) hauteur_max <- 0L

      type_el <- as.character(el$type)[1]
      kpi_el <- NULL
      if (identical(type_el, "kpi")) kpi_el <- p$kpi[[el$id_kpi]]
      est_tableau <- identical(type_el, "kpi") && !is.null(kpi_el) &&
                     identical(as.character(kpi_el$type)[1], "tableau")
      est_graphique <- identical(type_el, "graphique")
      est_texte <- identical(type_el, "texte")

      hauteur_actuelle <- rboard_hauteur_el(el)

      list(edit = edit, el = el, ligne = ligne,
           largeur_max = largeur_max, hauteur_max = hauteur_max,
           largeur_autres = largeur_autres,
           type_el = type_el, kpi_el = kpi_el,
           est_tableau = est_tableau, est_graphique = est_graphique,
           est_texte = est_texte,
           hauteur_actuelle = hauteur_actuelle)
    })

    format_cible_r <- reactive({
      ctx <- contexte_edit_r()
      if (is.null(ctx)) {
        return(list(h_cible = 1L, h_ok = TRUE, tailles = integer(0),
                    longueurs = character(0), tbl_long_cible = FALSE,
                    tbl_etendu_cible = FALSE, gl_long_cible = FALSE, raison = NULL))
      }

      tbl_long_cible   <- identical(format_tableau_r(), "long")
      tbl_etendu_cible <- identical(format_tableau_r(), "etendu")
      gl_long_cible    <- identical(format_graph_r(), "grand")

      h_cible <- if (ctx$est_tableau) {
        if (tbl_long_cible)        HAUTEUR_TABLEAU_LONG
        else if (tbl_etendu_cible) HAUTEUR_TABLEAU_ETENDU
        else                       HAUTEUR_TABLEAU
      } else if (ctx$est_graphique) {
        if (gl_long_cible) HAUTEUR_GRAPHIQUE_LARGE else HAUTEUR_GRAPHIQUE
      } else if (ctx$est_texte) {
        HAUTEUR_TEXTE
      } else HAUTEUR_KPI
      h_cible <- as.integer(h_cible)

      h_ok <- (h_cible <= ctx$hauteur_max)

      raison <- NULL
      if (!h_ok) {
        raison <- sprintf(
          "Ce format necessite %d/%d unites de hauteur, mais seulement %d unites sont disponibles sur cette page (les autres lignes en occupent deja %d).",
          h_cible, RBOARD_GRILLE_HAUTEUR, ctx$hauteur_max,
          RBOARD_GRILLE_HAUTEUR - ctx$hauteur_max)
      }

      largeurs <- if (identical(ctx$type_el, "graphique")) {
        c(6L, 12L)
      } else if (ctx$est_tableau) {
        if (tbl_long_cible)         c(6L, 12L)   # tableau long : 6 ou 12
        else if (tbl_etendu_cible)  6L           # tableau etendu : 6
        else                        c(3L, 6L, 12L)
      } else c(3L, 6L, 12L)

      largeurs <- largeurs[largeurs <= ctx$largeur_max]
      if (length(largeurs) == 0) largeurs <- integer(0)

      longueurs <- sapply(largeurs, function(t) {
        if (t == 3L) "3 colonnes (1/4)"
        else if (t == 6L) "6 colonnes (1/2)"
        else "12 colonnes (pleine largeur)"
      })

      list(h_cible = h_cible, h_ok = h_ok,
           tailles = largeurs, longueurs = longueurs,
           tbl_long_cible = tbl_long_cible,
           tbl_etendu_cible = tbl_etendu_cible,
           gl_long_cible = gl_long_cible,
           raison = raison)
    })

    output$ecran <- renderUI({
      ctx <- contexte_edit_r()
      if (is.null(ctx)) return(NULL)

      el <- ctx$el
      type_el <- ctx$type_el
      est_tableau <- ctx$est_tableau
      est_graphique <- ctx$est_graphique
      est_texte <- ctx$est_texte
      largeur_max <- ctx$largeur_max
      hauteur_max <- ctx$hauteur_max
      hauteur_actuelle <- ctx$hauteur_actuelle

      taille_el <- rboard_largeur_element(el)

      niveau_el <- suppressWarnings(as.integer(el$niveau_titre %||% 2L)[1])
      if (is.na(niveau_el) || !(niveau_el %in% c(1L, 2L, 3L))) niveau_el <- 2L

      suggere_long <- FALSE
      if (est_tableau && !is.null(ctx$kpi_el)) {
        suggere_long <- rboard_suggere_tableau_long(ctx$kpi_el)
      }

      fmt <- format_cible_r()
      h_ok <- isTRUE(fmt$h_ok)
      tailles_courantes <- fmt$tailles
      longueurs_courantes <- fmt$longueurs

      if (length(tailles_courantes) > 0) {
        selected_taille <- if (taille_el %in% tailles_courantes) taille_el
                           else max(tailles_courantes)
        choix_tailles <- setNames(tailles_courantes, longueurs_courantes)
      } else {
        selected_taille <- NULL
        choix_tailles <- character(0)
      }

      h_affichee <- fmt$h_cible
      couleur_h <- if (h_affichee >= 4L) "#be185d"
                  else if (h_affichee >= 3L) "#7c3aed"
                  else if (h_affichee == 2L) "#0891b2"
                  else "#64748b"

      badge_format <- switch(type_el,
        "kpi" = if (est_tableau) {
          switch(format_tableau_r() %||% "compact",
                 "compact" = "Tableau compact",
                 "long"    = "Tableau long",
                 "etendu"  = "Tableau etendu",
                 "Tableau")
        } else "KPI",
        "graphique" = if (identical(format_graph_r(), "grand")) "Graphique grand" else "Graphique",
        "texte" = if (identical(source_texte_r(), "bibliotheque")) "Texte (bibliotheque)" else "Texte",
        "Element")

      bloc_alerte <- NULL
      if (length(tailles_courantes) == 0) {
        msg <- if (!h_ok && !is.null(fmt$raison)) {
          fmt$raison
        } else if (est_tableau && fmt$tbl_long_cible) {
          "Impossible de passer en tableau long : largeur 6/12 ou hauteur 3/6 indisponible."
        } else if (est_tableau && fmt$tbl_etendu_cible) {
          "Impossible de passer en tableau etendu : largeur 6 ou hauteur 4/6 indisponible."
        } else if (est_graphique && fmt$gl_long_cible) {
          "Impossible de passer en graphique grand : hauteur 4/6 indisponible."
        } else {
          "Impossible de modifier : pas assez de place sur cette ligne ou cette page."
        }
        bloc_alerte <- tags$div(
          style = "margin-top: 14px; padding: 12px 16px; background: #fee2e2; border: 1px solid #fca5a5; border-radius: 8px; color: #991b1b; font-size: 0.85rem;",
          bsicons::bs_icon("exclamation-octagon-fill"), " ", msg)
      } else if (!h_ok) {
        msg <- if (!is.null(fmt$raison)) fmt$raison else {
          sprintf("Hauteur requise (%d unites) > disponible (%d unites).",
                  fmt$h_cible, hauteur_max)
        }
        bloc_alerte <- tags$div(
          style = "margin-top: 14px; padding: 12px 16px; background: #fff7ed; border: 1px solid #fdba74; border-radius: 8px; color: #7c2d12; font-size: 0.85rem;",
          bsicons::bs_icon("exclamation-triangle-fill"), " ", msg)
      }

      bloc_tableau <- NULL
      if (est_tableau) {
        info_suggestion <- if (suggere_long && !identical(format_tableau_r(), "long")) {
          tags$div(style = "font-size: 0.82rem; color: #2563eb; margin: -6px 0 12px 0;",
            bsicons::bs_icon("magic"),
            sprintf(" Suggestion : format long (%d lignes de donnees).",
                    if (!is.null(ctx$kpi_el$tableau)) nrow(ctx$kpi_el$tableau) else 0L))
        } else if (!suggere_long && identical(format_tableau_r(), "long")) {
          tags$div(style = "font-size: 0.82rem; color: #2563eb; margin: -6px 0 12px 0;",
            bsicons::bs_icon("magic"),
            sprintf(" Suggestion : format compact (%d lignes de donnees).",
                    if (!is.null(ctx$kpi_el$tableau)) nrow(ctx$kpi_el$tableau) else 0L))
        } else NULL

        bloc_tableau <- tagList(
          tags$hr(style = "margin: 18px 0 14px 0;"),
          radioButtons(ns("edit_tableau_long"), "Format du tableau :",
                       choices = c(
                         "Compact (2/6 hauteur, largeur 3/6/12)" = "compact",
                         "Long (3/6 hauteur, largeur 6 ou 12)"   = "long",
                         "Etendu (4/6 hauteur, largeur 6)"       = "etendu"
                       ),
                       selected = format_tableau_r() %||% "compact",
                       inline = FALSE),
          info_suggestion)
      }

      bloc_graphique <- NULL
      if (est_graphique) {
        bloc_graphique <- tagList(
          tags$hr(style = "margin: 18px 0 14px 0;"),
          radioButtons(ns("edit_graphique_long"), "Format du graphique :",
                       choices = c("Standard (3/6 hauteur)" = "standard",
                                   "Grand (4/6 hauteur)" = "grand"),
                       selected = format_graph_r() %||% "standard",
                       inline = TRUE))
      }

      bloc_source_texte <- NULL
      if (est_texte) {
        bloc_source_texte <- tagList(
          tags$hr(style = "margin: 18px 0 14px 0;"),
          radioButtons(ns("edit_source_texte"), "Source du texte :",
                       choices = c("Saisie libre" = "libre",
                                   "Depuis la bibliotheque" = "bibliotheque"),
                       selected = source_texte_r() %||% "libre",
                       inline = TRUE))
      }

      peut_enregistrer <- h_ok && length(tailles_courantes) > 0

      bouton_save <- if (peut_enregistrer) {
        actionButton(ns("btn_save"), "Enregistrer les modifications",
                     icon = bsicons::bs_icon("check-lg"),
                     class = "btn-primary",
                     style = "padding: 10px 20px;")
      } else {
        tags$button(
          disabled = "disabled",
          class = "btn btn-primary",
          style = "padding: 10px 20px; opacity: 0.5; cursor: not-allowed;",
          bsicons::bs_icon("check-lg"),
          " Enregistrer les modifications")
      }

      bslib::card(
        bslib::card_body(
          style = "padding: 28px 32px;",
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 24px; gap: 12px;",
            tags$div(
              style = "display: flex; align-items: center; gap: 12px; min-width: 0; flex: 1;",
              actionButton(ns("btn_retour"), NULL,
                           icon = bsicons::bs_icon("arrow-left"),
                           class = "btn-outline-secondary"),
              tags$div(style = "min-width: 0;",
                tags$h4(style = "margin: 0; font-weight: 600; font-size: 1.25rem;",
                        bsicons::bs_icon("pencil-square", class = "text-primary"),
                        " Modifier l'element"),
                tags$div(style = "font-size: 0.85rem; color: #64748b; margin-top: 4px;",
                         "ID : ", tags$code(el$id)))),
            tags$div(
              style = "display: flex; gap: 8px; align-items: center; flex-shrink: 0;",
              tags$span(class = "badge bg-light text-dark border",
                        style = "font-size: 0.85rem; padding: 8px 12px;",
                        badge_format),
              tags$span(
                class = "badge",
                style = sprintf("background: %s; color: #ffffff; font-size: 0.85rem; padding: 8px 12px; display: inline-flex; align-items: center; gap: 6px;",
                                couleur_h),
                bsicons::bs_icon("arrows-angle-contract"),
                sprintf(" %d unites", hauteur_actuelle)))),

          if (type_el %in% c("kpi", "graphique")) {
            tagList(
              textInput(ns("edit_nom"), "Nom affiche (laisser vide pour le nom d'origine) :",
                        value = as.character(el$nom_affiche %||% "")[1], width = "100%"),
              textAreaInput(ns("edit_commentaire"), "Commentaire specifique :",
                            value = as.character(el$commentaire %||% "")[1],
                            rows = 3, width = "100%"),
              if (identical(type_el, "kpi") && !est_tableau) {
                selectInput(ns("edit_couleur"), "Couleur forcee :",
                            choices = c("Automatique" = "", "Vert" = "success",
                                        "Rouge" = "danger", "Orange" = "warning",
                                        "Bleu" = "primary", "Cyan" = "info",
                                        "Gris" = "secondary", "Noir" = "dark",
                                        "Clair" = "light"),
                            selected = as.character(el$couleur_forcee %||% "")[1],
                            width = "100%")
              })
          } else {
            tagList(uiOutput(ns("ui_texte_edit_contenu")))
          },

          bloc_tableau,
          bloc_graphique,
          bloc_source_texte,
          uiOutput(ns("ui_texte_edit_biblio")),
          uiOutput(ns("ui_edit_niveau")),

          if (length(tailles_courantes) > 0) {
            selectInput(ns("edit_taille"), "Largeur :",
                        choices = choix_tailles,
                        selected = selected_taille, width = "100%")
          },

          tags$div(
            style = "margin-top: 10px; padding: 12px 14px; background: #f1f5f9; border-radius: 8px; font-size: 0.82rem; color: #475569;",
            tags$div(style = "display: flex; align-items: center; gap: 8px; margin-bottom: 6px;",
              bsicons::bs_icon("info-circle"),
              tags$strong(style = "color: #334155;", "Espace disponible")),
            tags$div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
              tags$div(bsicons::bs_icon("arrows-angle-expand"),
                       sprintf(" Largeur max : %d / 12 col", as.integer(largeur_max))),
              tags$div(bsicons::bs_icon("arrows-angle-contract"),
                       sprintf(" Hauteur dispo. : %d / %d unites",
                               as.integer(hauteur_max),
                               as.integer(RBOARD_GRILLE_HAUTEUR))))),

          bloc_alerte,

          tags$div(
            style = "margin-top: 28px; display: flex; gap: 12px; justify-content: flex-end;",
            actionButton(ns("btn_annuler"), "Annuler",
                         icon = bsicons::bs_icon("x-lg"),
                         class = "btn-outline-secondary",
                         style = "padding: 10px 20px;"),
            bouton_save)
        )
      )
    })

    output$ui_texte_edit_contenu <- renderUI({
      ctx <- contexte_edit_r()
      if (is.null(ctx) || !isTRUE(ctx$est_texte)) return(NULL)

      src <- source_texte_r() %||% "libre"
      el <- ctx$el

      if (identical(src, "libre")) {
        tagList(
          textAreaInput(ns("edit_contenu"),
                        "Contenu (Markdown supporte : **gras**, *italique*, <u>souligne</u>) :",
                        value = as.character(el$contenu %||% "")[1],
                        rows = 6, width = "100%"),
          selectInput(ns("edit_style"), "Style :",
                      choices = c("Normal" = "normal", "Titre" = "titre",
                                  "Note" = "note", "Avertissement" = "avertissement"),
                      selected = as.character(el$style %||% "normal")[1],
                      width = "100%"))
      } else NULL
    })

    output$ui_texte_edit_biblio <- renderUI({
      ctx <- contexte_edit_r()
      if (is.null(ctx) || !isTRUE(ctx$est_texte)) return(NULL)

      src <- source_texte_r() %||% "libre"
      if (!identical(src, "bibliotheque")) return(NULL)

      p <- projet_r()
      textes <- p$textes
      if (is.null(textes) || length(textes) == 0) {
        return(tags$div(
          style = "padding: 12px 14px; background: #fef3c7; border: 1px solid #fcd34d; border-radius: 8px; color: #78350f; font-size: 0.85rem;",
          bsicons::bs_icon("exclamation-triangle-fill"),
          " Aucun texte dans la bibliotheque. Utilisez la saisie libre ou creez d'abord un texte."))
      }

      choix <- setNames(
        sapply(textes, function(t) t$id),
        sapply(textes, function(t) {
          nom <- as.character(t$nom %||% "Texte")[1]
          apercu <- substr(as.character(t$contenu %||% "")[1], 1, 40)
          sprintf("%s | %s...", nom, apercu)
        })
      )

      id_actuel <- as.character(ctx$el$id_texte %||% "")[1]
      selected <- if (nzchar(id_actuel) && id_actuel %in% choix) id_actuel else choix[1]

      tagList(
        selectInput(ns("edit_id_texte"), "Choisir un texte :",
                    choices = choix, selected = selected, width = "100%"),
        uiOutput(ns("ui_texte_edit_biblio_apercu"))
      )
    })

    output$ui_texte_edit_biblio_apercu <- renderUI({
      idt <- input$edit_id_texte
      if (is.null(idt) || !nzchar(idt)) return(NULL)
      p <- projet_r()
      t <- p$textes[[idt]]
      if (is.null(t)) return(NULL)

      apercu <- as.character(t$contenu %||% "")[1]
      if (nchar(apercu) > 250) apercu <- paste0(substr(apercu, 1, 247), "...")

      tags$div(
        style = "margin: 4px 0 12px 0; padding: 10px 14px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; font-size: 0.82rem; color: #475569;",
        bsicons::bs_icon("eye-fill"),
        tags$span(style = "margin-left: 6px; font-weight: 600; color: #334155;",
                  sprintf("Style : %s", as.character(t$style %||% "normal")[1])),
        tags$div(style = "margin-top: 6px; white-space: pre-wrap;", apercu))
    })

    output$ui_edit_niveau <- renderUI({
      ctx <- contexte_edit_r()
      if (is.null(ctx) || !isTRUE(ctx$est_texte)) return(NULL)
      if (!identical(source_texte_r(), "libre")) return(NULL)
      req(input$edit_style)
      if (!identical(input$edit_style, "titre")) return(NULL)

      edit <- element_edition_r()
      niveau_actuel <- 2L
      if (!is.null(edit) && !is.null(edit$element)) {
        n <- suppressWarnings(as.integer(edit$element$niveau_titre %||% 2L)[1])
        if (!is.na(n) && n %in% c(1L, 2L, 3L)) niveau_actuel <- n
      }
      selectInput(ns("edit_niveau_titre"), "Niveau de titre :",
                  choices = c("H1 - Grand" = 1L, "H2 - Moyen" = 2L, "H3 - Petit" = 3L),
                  selected = niveau_actuel, width = "100%")
    })

    observeEvent(input$edit_graphique_long, {
      if (!is.null(input$edit_graphique_long) && nzchar(input$edit_graphique_long)) {
        format_graph_r(input$edit_graphique_long)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$edit_tableau_long, {
      if (!is.null(input$edit_tableau_long) && nzchar(input$edit_tableau_long)) {
        format_tableau_r(input$edit_tableau_long)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$edit_source_texte, {
      if (!is.null(input$edit_source_texte) && nzchar(input$edit_source_texte)) {
        source_texte_r(input$edit_source_texte)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$btn_retour,  { element_edition_r(NULL) })
    observeEvent(input$btn_annuler, { element_edition_r(NULL) })

    observeEvent(input$btn_save, {
      ctx <- contexte_edit_r()
      if (is.null(ctx)) return()
      el <- ctx$el
      if (is.null(el)) return()

      type_el <- ctx$type_el
      est_tableau <- ctx$est_tableau
      est_graphique <- ctx$est_graphique
      est_texte <- ctx$est_texte

      taille <- rboard_largeur_element(el)
      if (!is.null(input$edit_taille)) {
        t <- suppressWarnings(as.integer(input$edit_taille)[1])
        if (!is.na(t)) taille <- t
      }

      vals <- list(taille = taille)

      if (type_el %in% c("kpi", "graphique")) {
        nom <- trimws(as.character(input$edit_nom %||% "")[1])
        com <- trimws(as.character(input$edit_commentaire %||% "")[1])
        vals$nom_affiche <- if (nzchar(nom)) nom else NULL
        vals$commentaire <- if (nzchar(com)) com else NULL

        if (identical(type_el, "kpi") && !est_tableau) {
          coul <- as.character(input$edit_couleur %||% "")[1]
          vals$couleur_forcee <- if (nzchar(coul)) coul else NULL
        }

        if (est_tableau) {
          fmt <- format_tableau_r() %||% "compact"
          vals$tableau_long   <- identical(fmt, "long")
          vals$tableau_etendu <- identical(fmt, "etendu")
        }
        if (est_graphique) {
          vals$graphique_long <- identical(format_graph_r(), "grand")
        }

      } else if (est_texte) {
        src <- source_texte_r() %||% "libre"
        if (identical(src, "bibliotheque")) {
          idt <- input$edit_id_texte
          idt_chr <- if (is.list(idt)) as.character(idt[[1]])[1] else as.character(idt)[1]
          if (is.na(idt_chr) || !nzchar(idt_chr)) {
            showNotification("Aucun texte de bibliotheque selectionne.", type = "error", duration = 5)
            return()
          }
          p <- projet_r()
          t <- p$textes[[idt_chr]]
          if (is.null(t)) {
            showNotification("Texte introuvable.", type = "error", duration = 5)
            return()
          }
          vals$id_texte <- idt_chr
          vals$contenu <- as.character(t$contenu %||% "")[1]
          vals$style <- as.character(t$style %||% "normal")[1]
          vals$niveau_titre <- NULL
        } else {
          txt <- trimws(as.character(input$edit_contenu %||% "")[1])
          vals$contenu <- if (nzchar(txt)) txt else as.character(el$contenu)[1]
          style <- as.character(input$edit_style %||% "normal")[1]
          vals$style <- style
          vals$id_texte <- NULL
          if (identical(style, "titre")) {
            nt <- suppressWarnings(as.integer(input$edit_niveau_titre %||% 2L)[1])
            if (is.na(nt) || !(nt %in% c(1L, 2L, 3L))) nt <- 2L
            vals$niveau_titre <- nt
          } else vals$niveau_titre <- NULL
        }
      }

      p <- projet_r()
      resultat <- tryCatch({
        p$dashboard <- modifier_element(p$dashboard, ctx$edit$id_page,
                                         ctx$edit$id_ligne,
                                         ctx$edit$id_element, vals)
        p$date_modification <- Sys.Date()
        projet_r(p)
        TRUE
      }, error = function(e) {
        showNotification(
          tags$div(
            bsicons::bs_icon("exclamation-octagon-fill"),
            tags$strong(" Impossible de modifier : "),
            conditionMessage(e)),
          type = "error", duration = 10)
        FALSE
      })
      if (resultat) element_edition_r(NULL)
    })
  })
}