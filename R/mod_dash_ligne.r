# ==============================================================================
# Rboard - Gestion d'une ligne du dashboard
# ==============================================================================

#' @export
mod_dash_ligne_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    style = "margin-bottom: 14px;",
    bslib::card_header(
      style = "display: flex; justify-content: space-between; align-items: center; gap: 8px; padding: 10px 14px;",
      tags$div(
        style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
        tags$span(
          style = "font-weight: 600; color: #0f172a; display: inline-flex; align-items: center; gap: 8px;",
          bsicons::bs_icon("layout-three-columns", class = "text-primary"),
          uiOutput(ns("titre_ligne"), inline = TRUE)),
        uiOutput(ns("badge_largeur"), inline = TRUE),
        uiOutput(ns("badge_hauteur"), inline = TRUE)),
      tags$div(
        style = "display: flex; gap: 6px;",
        actionButton(ns("btn_ligne_haut"), NULL,
                     icon = bsicons::bs_icon("arrow-up"),
                     class = "btn-sm btn-outline-secondary", title = "Monter"),
        actionButton(ns("btn_ligne_bas"), NULL,
                     icon = bsicons::bs_icon("arrow-down"),
                     class = "btn-sm btn-outline-secondary", title = "Descendre"),
        actionButton(ns("btn_ajouter_element"),
                     label = "Ajouter un element",
                     icon = bsicons::bs_icon("plus-lg"),
                     class = "btn-sm btn-primary"),
        actionButton(ns("btn_suppr_ligne"), NULL,
                     icon = bsicons::bs_icon("trash"),
                     class = "btn-sm btn-outline-danger", title = "Supprimer"))),
    bslib::card_body(
      style = "padding: 12px 14px;",
      uiOutput(ns("zone_elements"))))
}

#' @export
mod_dash_ligne_server <- function(id, projet_r, id_page, id_ligne, element_edition_r = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ligne_r <- reactive({
      p <- projet_r()
      obtenir_ligne(p$dashboard, id_page, id_ligne)
    })

    restante_largeur_r <- reactive({
      ligne <- ligne_r()
      if (is.null(ligne) || is.null(ligne$elements) || length(ligne$elements) == 0) {
        return(12L)
      }
      as.integer(12L - rboard_somme_largeurs(ligne$elements))
    })

    restante_hauteur_r <- reactive({
      p <- projet_r()
      ligne <- ligne_r()
      h_page <- hauteur_utilisee_page(p$dashboard, id_page)
      h_ligne_actuelle <- rboard_max_hauteurs(ligne$elements)
      as.integer(RBOARD_GRILLE_HAUTEUR - (h_page - h_ligne_actuelle))
    })

    tailles_dispo_pour <- function(type, kpi = NULL,
                                    tableau_long = FALSE,
                                    tableau_etendu = FALSE,
                                    graphique_long = FALSE) {
      h_cible <- rboard_hauteur_element(type, kpi,
                                         tableau_long = tableau_long,
                                         tableau_etendu = tableau_etendu,
                                         graphique_long = graphique_long)

      ligne <- ligne_r()
      h_ligne_actuelle <- rboard_max_hauteurs(ligne$elements)
      h_nouvelle_ligne <- max(h_ligne_actuelle, h_cible)
      if (h_nouvelle_ligne > restante_hauteur_r()) return(character(0))

      largeurs <- rboard_largeurs_autorisees(type, kpi,
                                              tableau_long = tableau_long,
                                              tableau_etendu = tableau_etendu,
                                              graphique_long = graphique_long)
      largeurs <- largeurs[largeurs <= restante_largeur_r()]
      if (length(largeurs) == 0) return(character(0))

      setNames(largeurs, sapply(largeurs, function(t) {
        if (t == 3L) "3 colonnes (1/4)"
        else if (t == 6L) "6 colonnes (1/2)"
        else "12 colonnes (pleine largeur)"
      }))
    }

    output$titre_ligne <- renderUI({
      p <- projet_r()
      page <- obtenir_page(p$dashboard, id_page)
      pos <- "?"
      if (!is.null(page) && !is.null(page$lignes)) {
        idx <- which(sapply(page$lignes, function(l) identical(l$id, id_ligne)))
        if (length(idx) > 0) pos <- idx[1]
      }
      tags$span(
        style = "display: inline-flex; align-items: center; gap: 8px;",
        tags$code(style = "background: #1e293b; color: #ffffff; padding: 2px 8px; border-radius: 4px; font-size: 0.82rem; font-weight: 700; letter-spacing: 0.05em;",
                  id_ligne),
        tags$span(style = "color: #64748b; font-size: 0.85rem; font-weight: 500;",
                  sprintf("position %s", pos)))
    })

    output$badge_largeur <- renderUI({
      total <- as.integer(12L - restante_largeur_r())
      classe <- if (total > 12L) "badge bg-danger"
                else if (total == 12L) "badge bg-success"
                else "badge bg-light text-dark border"
      tags$span(class = classe, sprintf("L %d / 12 col", total))
    })

    output$badge_hauteur <- renderUI({
      ligne <- ligne_r()
      h_ligne <- rboard_max_hauteurs(ligne$elements)
      classe <- if (h_ligne >= RBOARD_GRILLE_HAUTEUR) "badge bg-danger"
                else "badge bg-light text-dark border"
      tags$span(class = classe, sprintf("H %d / %d unites", h_ligne, RBOARD_GRILLE_HAUTEUR))
    })

    observeEvent(input$btn_ligne_haut, {
      p <- projet_r()
      p$dashboard <- deplacer_ligne(p$dashboard, id_page, id_ligne, "haut")
      p$date_modification <- Sys.Date(); projet_r(p)
    })
    observeEvent(input$btn_ligne_bas, {
      p <- projet_r()
      p$dashboard <- deplacer_ligne(p$dashboard, id_page, id_ligne, "bas")
      p$date_modification <- Sys.Date(); projet_r(p)
    })
    observeEvent(input$btn_suppr_ligne, {
      p <- projet_r()
      p$dashboard <- supprimer_ligne(p$dashboard, id_page, id_ligne)
      p$date_modification <- Sys.Date(); projet_r(p)
    })

    # ==========================================================================
    # Modal d'ajout
    # ==========================================================================
    observeEvent(input$btn_ajouter_element, {
      p <- projet_r()
      restante_largeur <- restante_largeur_r()
      restante_hauteur <- restante_hauteur_r()

      if (restante_largeur <= 0L) {
        showNotification(
          "Cette ligne est pleine (12/12 colonnes). Creez une nouvelle ligne.",
          type = "warning", duration = 5)
        return()
      }

      kpis_stat <- p$kpi
      graphs <- p$graphiques
      textes_biblio <- p$textes

      choix_kpi <- if (!is.null(kpis_stat) && length(kpis_stat) > 0) {
        setNames(sapply(kpis_stat, function(k) k$id),
                 sapply(kpis_stat, function(k) sprintf("%s | %s", k$id, k$nom)))
      } else c("(Aucun KPI disponible)" = "")

      choix_graph <- if (!is.null(graphs) && length(graphs) > 0) {
        setNames(sapply(graphs, function(g) g$id),
                 sapply(graphs, function(g) sprintf("%s | %s", g$id, g$nom)))
      } else c("(Aucun graphique disponible)" = "")

      choix_texte_biblio <- if (!is.null(textes_biblio) && length(textes_biblio) > 0) {
        setNames(sapply(textes_biblio, function(t) t$id),
                 sapply(textes_biblio, function(t) {
                   nom <- as.character(t$nom %||% "Texte")[1]
                   apercu <- substr(as.character(t$contenu %||% "")[1], 1, 40)
                   sprintf("%s | %s...", nom, apercu)
                 }))
      } else c("(Aucun texte dans la bibliotheque)" = "")

      showModal(modalDialog(
        title = tags$div(
          style = "display: flex; align-items: center; gap: 8px;",
          bsicons::bs_icon("plus-lg", class = "text-primary"),
          " Ajouter un element a la ligne"),
        size = "l", easyClose = TRUE,

        tags$div(
          style = "padding: 10px 14px; background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 8px; font-size: 0.85rem; color: #1e40af; margin-bottom: 14px;",
          bsicons::bs_icon("info-circle"),
          sprintf(" Espace restant : %d / 12 colonnes, %d / %d unites de hauteur.",
                  as.integer(restante_largeur),
                  as.integer(restante_hauteur),
                  as.integer(RBOARD_GRILLE_HAUTEUR))),

        radioButtons(ns("type_ajout"), "Type d'element :",
                     choices = c("KPI statistique" = "kpi",
                                 "Graphique" = "graphique",
                                 "Texte" = "texte"),
                     selected = "kpi", inline = TRUE),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'kpi'", ns("type_ajout")),
          selectInput(ns("id_kpi_ajout"), "Choisir un KPI :",
                      choices = choix_kpi, width = "100%"),
          uiOutput(ns("ui_format_tableau")),
          selectInput(ns("couleur_ajout"), "Couleur forcee (optionnel) :",
                      choices = c("Automatique" = "", "Vert" = "success",
                                  "Rouge" = "danger", "Orange" = "warning",
                                  "Bleu" = "primary", "Cyan" = "info",
                                  "Gris" = "secondary", "Noir" = "dark",
                                  "Clair" = "light"),
                      width = "100%"),
          uiOutput(ns("ui_taille_kpi"))),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'graphique'", ns("type_ajout")),
          selectInput(ns("id_graph_ajout"), "Choisir un graphique :",
                      choices = choix_graph, width = "100%"),
          radioButtons(ns("graphique_long_ajout"), "Format du graphique :",
                       choices = c("Standard (3/6 hauteur)" = "standard",
                                   "Grand (4/6 hauteur)" = "grand"),
                       selected = "standard", inline = TRUE),
          uiOutput(ns("ui_taille_graph"))),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'texte'", ns("type_ajout")),
          radioButtons(ns("source_texte_ajout"), "Source du texte :",
                       choices = c("Saisie libre" = "libre",
                                   "Depuis la bibliotheque" = "bibliotheque"),
                       selected = "libre", inline = TRUE),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'libre'", ns("source_texte_ajout")),
            textAreaInput(ns("contenu_texte_ajout"),
                          "Contenu (Markdown supporte) :",
                          rows = 4, width = "100%"),
            selectInput(ns("style_texte_ajout"), "Style :",
                        choices = c("Normal" = "normal", "Titre" = "titre",
                                    "Note" = "note", "Avertissement" = "avertissement"),
                        selected = "normal", width = "100%")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'bibliotheque'", ns("source_texte_ajout")),
            selectInput(ns("id_texte_biblio_ajout"), "Choisir un texte :",
                        choices = choix_texte_biblio, width = "100%"),
            uiOutput(ns("apercu_texte_biblio"))),
          uiOutput(ns("ui_niveau_titre")),
          uiOutput(ns("ui_taille_texte"))),

        tags$hr(),

        textInput(ns("nom_affiche_ajout"), "Nom affiche (optionnel) :",
                  value = "", width = "100%"),
        textAreaInput(ns("commentaire_ajout"), "Commentaire (optionnel) :",
                      rows = 2, width = "100%"),

        footer = tagList(
          modalButton("Annuler"),
          actionButton(ns("btn_valider_ajout"), "Ajouter",
                       icon = bsicons::bs_icon("check-lg"), class = "btn-primary"))))
    })

    # ==========================================================================
    # UI format tableau
    # ==========================================================================
    output$ui_format_tableau <- renderUI({
      idk <- input$id_kpi_ajout
      if (is.null(idk) || !nzchar(idk)) return(NULL)
      p <- projet_r()
      kpi <- p$kpi[[idk]]
      if (is.null(kpi) || !identical(as.character(kpi$type)[1], "tableau")) return(NULL)

      suggere_long <- rboard_suggere_tableau_long(kpi)
      n_lignes <- if (!is.null(kpi$tableau)) nrow(kpi$tableau) else 0L
      n_colonnes <- if (!is.null(kpi$tableau)) ncol(kpi$tableau) else 0L

      defaut <- if (suggere_long) "long" else "compact"

      tagList(
        radioButtons(ns("format_tableau_ajout"), "Format du tableau :",
                     choices = c(
                       "Compact (2/6 hauteur, largeur 3/6/12)" = "compact",
                       "Long (3/6 hauteur, largeur 6 ou 12)"   = "long",
                       "Etendu (4/6 hauteur, largeur 6)"       = "etendu"
                     ),
                     selected = defaut, inline = FALSE),
        tags$div(
          style = "font-size: 0.82rem; color: #64748b; margin: -6px 0 10px 0;",
          bsicons::bs_icon("magic"),
          sprintf(" Tableau de %d lignes x %d colonnes. Suggestion : format %s.",
                  as.integer(n_lignes), as.integer(n_colonnes),
                  if (suggere_long) "long" else "compact")))
    })

    # ==========================================================================
    # UI taille KPI
    # ==========================================================================
    output$ui_taille_kpi <- renderUI({
      idk <- input$id_kpi_ajout
      if (is.null(idk) || !nzchar(idk)) return(NULL)
      p <- projet_r()
      kpi <- p$kpi[[idk]]
      if (is.null(kpi)) return(NULL)

      est_tableau <- identical(as.character(kpi$type)[1], "tableau")

      if (!est_tableau) {
        tailles <- tailles_dispo_pour("kpi", kpi = kpi)
        if (length(tailles) == 0) {
          return(tags$div(
            style = "padding: 10px 14px; background: #fee2e2; border: 1px solid #fca5a5; border-radius: 8px; color: #991b1b; font-size: 0.85rem;",
            bsicons::bs_icon("exclamation-triangle-fill"),
            " Pas assez de place pour ce KPI."))
        }
        return(selectInput(ns("taille_ajout"), "Largeur :",
                           choices = tailles, selected = 6L, width = "100%"))
      }

      fmt <- input$format_tableau_ajout
      if (is.null(fmt) || !nzchar(fmt)) fmt <- "compact"

      tbl_long   <- identical(fmt, "long")
      tbl_etendu <- identical(fmt, "etendu")

      tailles <- tailles_dispo_pour("kpi", kpi = kpi,
                                     tableau_long = tbl_long,
                                     tableau_etendu = tbl_etendu)

      if (length(tailles) == 0) {
        msg <- if (tbl_long) {
          "Pas assez de place pour un tableau long (largeur 6 ou 12, hauteur 3/6). Essayez un autre format."
        } else if (tbl_etendu) {
          "Pas assez de place pour un tableau etendu (largeur 6, hauteur 4/6). Essayez un autre format."
        } else {
          "Pas assez de place pour ce tableau compact."
        }
        return(tags$div(
          style = "padding: 10px 14px; background: #fee2e2; border: 1px solid #fca5a5; border-radius: 8px; color: #991b1b; font-size: 0.85rem;",
          bsicons::bs_icon("exclamation-triangle-fill"), " ", msg))
      }

      if (tbl_etendu) {
        return(tags$div(
          style = "padding: 10px 14px; background: #f1f5f9; border-radius: 8px; font-size: 0.85rem; color: #334155;",
          bsicons::bs_icon("info-circle"),
          " Largeur : 6 colonnes (moitie de page, forcee)."))
      }

      # Tableau long ET compact : selectInput normal
      selectInput(ns("taille_ajout"), "Largeur :",
                  choices = tailles, selected = 6L, width = "100%")
    })

    output$ui_taille_graph <- renderUI({
      gl <- identical(input$graphique_long_ajout, "grand")
      tailles <- tailles_dispo_pour("graphique", graphique_long = gl)
      if (length(tailles) == 0) {
        msg <- if (gl) {
          "Pas assez de place pour un graphique grand (4/6 hauteur). Essayez le format standard."
        } else {
          "Pas assez de place pour un graphique (largeur min 6, hauteur 3/6)."
        }
        return(tags$div(
          style = "padding: 10px 14px; background: #fee2e2; border: 1px solid #fca5a5; border-radius: 8px; color: #991b1b; font-size: 0.85rem;",
          bsicons::bs_icon("exclamation-triangle-fill"), " ", msg))
      }
      selectInput(ns("taille_ajout"), "Largeur :",
                  choices = tailles, selected = 6L, width = "100%")
    })

    output$ui_taille_texte <- renderUI({
      tailles <- tailles_dispo_pour("texte")
      if (length(tailles) == 0) {
        return(tags$div(
          style = "padding: 10px 14px; background: #fee2e2; border: 1px solid #fca5a5; border-radius: 8px; color: #991b1b; font-size: 0.85rem;",
          bsicons::bs_icon("exclamation-triangle-fill"),
          " Pas assez de place pour ce texte."))
      }
      selectInput(ns("taille_ajout"), "Largeur :",
                  choices = tailles, selected = 6L, width = "100%")
    })

    output$apercu_texte_biblio <- renderUI({
      idt <- input$id_texte_biblio_ajout
      if (is.null(idt) || !nzchar(idt)) return(NULL)
      p <- projet_r()
      txt_obj <- p$textes[[idt]]
      if (is.null(txt_obj)) return(NULL)

      apercu <- as.character(txt_obj$contenu %||% "")[1]
      if (nchar(apercu) > 200) apercu <- paste0(substr(apercu, 1, 197), "...")

      tags$div(
        style = "margin: 4px 0 12px 0; padding: 10px 14px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; font-size: 0.82rem; color: #475569;",
        bsicons::bs_icon("eye-fill"),
        tags$span(style = "margin-left: 6px; font-weight: 600; color: #334155;",
                  sprintf("Style : %s", as.character(txt_obj$style %||% "normal")[1])),
        tags$div(style = "margin-top: 6px; white-space: pre-wrap;", apercu))
    })

    output$ui_niveau_titre <- renderUI({
      if (!identical(input$type_ajout, "texte")) return(NULL)
      if (!identical(input$source_texte_ajout, "libre")) return(NULL)
      req(input$style_texte_ajout)
      if (!identical(input$style_texte_ajout, "titre")) return(NULL)
      selectInput(ns("niveau_titre_ajout"), "Niveau de titre :",
                  choices = c("H1 - Grand" = 1L,
                              "H2 - Moyen" = 2L,
                              "H3 - Petit" = 3L),
                  selected = 2L, width = "100%")
    })

    # ==========================================================================
    # Validation
    # ==========================================================================
    observeEvent(input$btn_valider_ajout, {
      raw_type <- input$type_ajout
      type <- if (is.list(raw_type)) as.character(raw_type[[1]])[1]
              else as.character(raw_type)[1]
      if (is.na(type) || length(type) == 0) type <- "kpi"

      nom_aff <- trimws(as.character(input$nom_affiche_ajout %||% "")[1])
      com <- trimws(as.character(input$commentaire_ajout %||% "")[1])
      p <- projet_r()

      hauteur <- 1L
      tbl_long   <- FALSE
      tbl_etendu <- FALSE
      gl_long <- FALSE
      id_texte <- NULL
      contenu_texte <- NULL
      style_texte <- "normal"
      niveau_titre <- NULL

      if (identical(type, "kpi")) {
        idk <- input$id_kpi_ajout
        idk_chr <- if (is.list(idk)) as.character(idk[[1]])[1] else as.character(idk)[1]
        if (is.na(idk_chr) || !nzchar(idk_chr)) {
          showNotification("Aucun KPI selectionne.", type = "error", duration = 5); return()
        }
        kpi_choisi <- p$kpi[[idk_chr]]
        if (is.null(kpi_choisi)) {
          showNotification("KPI introuvable.", type = "error", duration = 5); return()
        }
        est_tableau <- identical(as.character(kpi_choisi$type)[1], "tableau")

        if (est_tableau) {
          fmt <- input$format_tableau_ajout
          if (is.null(fmt) || !nzchar(fmt)) fmt <- "compact"
          tbl_long   <- identical(fmt, "long")
          tbl_etendu <- identical(fmt, "etendu")
        }
        hauteur <- rboard_hauteur_element("kpi", kpi_choisi,
                                           tableau_long = tbl_long,
                                           tableau_etendu = tbl_etendu)

      } else if (identical(type, "graphique")) {
        gl_long <- identical(input$graphique_long_ajout, "grand")
        hauteur <- rboard_hauteur_element("graphique", graphique_long = gl_long)

      } else {
        src <- input$source_texte_ajout
        if (is.null(src) || !nzchar(src)) src <- "libre"

        if (identical(src, "bibliotheque")) {
          idt <- input$id_texte_biblio_ajout
          idt_chr <- if (is.list(idt)) as.character(idt[[1]])[1] else as.character(idt)[1]
          if (is.na(idt_chr) || !nzchar(idt_chr) || is.null(p$textes[[idt_chr]])) {
            showNotification("Aucun texte de bibliotheque selectionne.", type = "error", duration = 5)
            return()
          }
          id_texte <- idt_chr
          txt_obj <- p$textes[[idt_chr]]
          contenu_texte <- as.character(txt_obj$contenu %||% "")[1]
          style_texte <- as.character(txt_obj$style %||% "normal")[1]
        } else {
          txt <- trimws(as.character(input$contenu_texte_ajout %||% "")[1])
          if (!nzchar(txt)) {
            showNotification("Contenu du texte vide.", type = "error", duration = 5); return()
          }
          contenu_texte <- txt
          style_texte <- as.character(input$style_texte_ajout %||% "normal")[1]
          if (is.na(style_texte) || !nzchar(style_texte)) style_texte <- "normal"
          if (identical(style_texte, "titre")) {
            nt <- suppressWarnings(as.integer(input$niveau_titre_ajout %||% 2L)[1])
            if (is.na(nt) || !(nt %in% c(1L, 2L, 3L))) nt <- 2L
            niveau_titre <- nt
          }
        }
        hauteur <- rboard_hauteur_element("texte")
      }

      largeur <- suppressWarnings(as.integer(input$taille_ajout)[1])
      if (is.na(largeur)) largeur <- 6L

      if (tbl_long && !(largeur %in% c(6L, 12L))) largeur <- 12L
      if (tbl_etendu) largeur <- 6L

      elem <- tryCatch({
        if (identical(type, "kpi")) {
          coul <- input$couleur_ajout
          coul_chr <- if (is.null(coul) || is.list(coul)) "" else as.character(coul)[1]
          creer_element(
            "kpi",
            id_kpi = as.character(input$id_kpi_ajout)[1],
            taille_largeur = largeur,
            nom_affiche = if (nzchar(nom_aff)) nom_aff else NULL,
            commentaire = if (nzchar(com)) com else NULL,
            couleur_forcee = if (nzchar(coul_chr)) coul_chr else NULL,
            hauteur_unites = hauteur,
            tableau_long = tbl_long,
            tableau_etendu = tbl_etendu)

        } else if (identical(type, "graphique")) {
          idg <- input$id_graph_ajout
          idg_chr <- if (is.list(idg)) as.character(idg[[1]])[1] else as.character(idg)[1]
          if (is.na(idg_chr) || !nzchar(idg_chr)) stop("Aucun graphique selectionne.", call. = FALSE)
          creer_element(
            "graphique", id_kpi = idg_chr,
            taille_largeur = largeur,
            nom_affiche = if (nzchar(nom_aff)) nom_aff else NULL,
            commentaire = if (nzchar(com)) com else NULL,
            hauteur_unites = hauteur,
            graphique_long = gl_long)

        } else {
          creer_element(
            "texte",
            contenu = contenu_texte,
            style = style_texte,
            taille_largeur = largeur,
            niveau_titre = niveau_titre,
            hauteur_unites = hauteur,
            id_texte = id_texte)
        }
      }, error = function(e) {
        showNotification(e$message, type = "error", duration = 5)
        NULL
      })

      if (is.null(elem)) return()

      resultat <- tryCatch({
        p$dashboard <- ajouter_element(p$dashboard, id_page, id_ligne, elem)
        p$date_modification <- Sys.Date()
        projet_r(p)
        TRUE
      }, error = function(e) {
        showNotification(e$message, type = "error", duration = 6)
        FALSE
      })

      if (resultat) removeModal()
    })

    # ==========================================================================
    # Zone d'affichage des elements
    # ==========================================================================
    output$zone_elements <- renderUI({
      ligne <- ligne_r()
      if (is.null(ligne) || is.null(ligne$elements) || length(ligne$elements) == 0) {
        return(tags$div(
          style = "padding: 12px; text-align: center; color: #94a3b8; font-style: italic; font-size: 0.88rem;",
          "Aucun element. Cliquez sur 'Ajouter un element'."))
      }

      for (el in ligne$elements) {
        if (identical(as.character(el$type)[1], "graphique")) {
          local({
            my_el <- el
            output[[paste0("edit_plot_", my_el$id)]] <- renderPlot({
              proj <- projet_r()
              g_obj <- proj$graphiques[[my_el$id_kpi]]
              if (!is.null(g_obj) && !is.null(g_obj$objet)) {
                tryCatch(print(g_obj$objet), error = function(e) {
                  graphics::plot.new()
                  graphics::text(0.5, 0.5, "Erreur de rendu", col = "#dc2626")
                })
              } else {
                graphics::plot.new()
                graphics::text(0.5, 0.5, "Graphique introuvable", col = "#94a3b8")
              }
            })
          })
        }
      }

      tagList(
        lapply(ligne$elements, function(el) {
          mod_dash_element_ui(ns(paste0("el_", el$id)))
        }))
    })

    observe({
      ligne <- ligne_r()
      if (is.null(ligne) || is.null(ligne$elements)) return()
      for (el in ligne$elements) {
        local({
          my_el <- el
          my_id_element <- my_el$id
          mod_dash_element_server(
            id = paste0("el_", my_id_element),
            projet_r = projet_r,
            id_page = id_page,
            id_ligne = id_ligne,
            id_element = my_id_element,
            element_edition_r = element_edition_r)
        })
      }
    })
  })
}