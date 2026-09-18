#' ==============================================================================
#' Rboard - Gestion d'un element du dashboard
#' ==============================================================================

rboard_libelle_element <- function(el, projet) {
  type_el <- as.character(el$type)[1]

  # ---------------------------------------------------------------------------
  # KPI
  # ---------------------------------------------------------------------------
  if (identical(type_el, "kpi")) {
    k <- projet$kpi[[el$id_kpi]]
    if (!is.null(k)) {
      sous_type <- as.character(k$type)[1]

      if (identical(sous_type, "tableau")) {
        libelle <- if (isTRUE(el$tableau_long))        "Tableau long"
                   else if (isTRUE(el$tableau_etendu)) "Tableau etendu"
                   else                                "Tableau"
        couleur <- if (isTRUE(el$tableau_long))        "#7c3aed"
                   else if (isTRUE(el$tableau_etendu)) "#be185d"
                   else                                "#7c3aed"
        return(list(libelle = libelle, icone = "table", couleur = couleur))
      }

      return(list(
        libelle = switch(sous_type,
          "moyenne" = "Moyenne", "mediane" = "Mediane",
          "ecart_type" = "Ecart-type", "effectif" = "Effectif",
          "frequence" = "Frequence", "p_value" = "p-value",
          "d_cohen" = "d de Cohen", "correlation" = "Correlation",
          "texte" = "Texte", toupper(sous_type)),
        icone = switch(sous_type,
          "moyenne" = "bar-chart-fill", "mediane" = "graph-up",
          "ecart_type" = "activity", "effectif" = "people-fill",
          "frequence" = "pie-chart-fill", "p_value" = "award-fill",
          "d_cohen" = "arrows-expand", "correlation" = "link-45deg",
          "texte" = "text-paragraph", "bookmark-fill"),
        couleur = "#2563eb"))
    }
    return(list(libelle = "KPI", icone = "calculator-fill", couleur = "#2563eb"))
  }

  # ---------------------------------------------------------------------------
  # Graphique
  # ---------------------------------------------------------------------------
  if (identical(type_el, "graphique")) {
    g <- projet$graphiques[[el$id_kpi]]
    est_grand <- isTRUE(el$graphique_long)
    libelle <- if (est_grand) "Graphique grand" else "Graphique"
    if (!is.null(g)) {
      tg <- as.character(g$type_graphique %||% libelle)[1]
      return(list(libelle = tg, icone = "bar-chart-fill", couleur = "#ea580c"))
    }
    return(list(libelle = libelle, icone = "bar-chart-fill", couleur = "#ea580c"))
  }

  # ---------------------------------------------------------------------------
  # Texte
  # ---------------------------------------------------------------------------
  id_texte <- if (!is.null(el$id_texte)) as.character(el$id_texte)[1] else NULL
  depuis_biblio <- !is.null(id_texte) && nzchar(id_texte) &&
                   !is.null(projet$textes) &&
                   !is.null(projet$textes[[id_texte]])

  # Recupere le style (libre ou bibliotheque)
  stl <- if (depuis_biblio) {
    as.character(projet$textes[[id_texte]]$style %||% "normal")[1]
  } else {
    as.character(el$style %||% "normal")[1]
  }
  if (is.na(stl) || !nzchar(stl)) stl <- "normal"

  libelle_style <- switch(stl,
    "titre" = "Titre",
    "note" = "Note",
    "avertissement" = "Avertissement",
    "Texte")

  if (depuis_biblio) {
    return(list(libelle = paste0(libelle_style, " (biblio)"),
                icone = "journal-text",
                couleur = "#0891b2"))
  }

  list(
    libelle = libelle_style,
    icone = "text-paragraph",
    couleur = "#64748b")
}

# ==============================================================================
# Interface
# ==============================================================================

mod_dash_element_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = paste0(
      "border: 1px solid #e2e8f0; border-radius: 6px; ",
      "background: #f8fafc; padding: 8px 10px; margin-bottom: 6px; ",
      "display: flex; align-items: center; justify-content: space-between; gap: 8px;"),
    uiOutput(ns("info_element"), inline = TRUE),
    tags$div(
      style = "display: flex; gap: 4px; flex-shrink: 0;",
      actionButton(ns("btn_gauche"), NULL, icon = bsicons::bs_icon("arrow-left"),
                   class = "btn-sm btn-outline-secondary",
                   style = "padding: 2px 8px;", title = "Deplacer a gauche"),
      actionButton(ns("btn_droite"), NULL, icon = bsicons::bs_icon("arrow-right"),
                   class = "btn-sm btn-outline-secondary",
                   style = "padding: 2px 8px;", title = "Deplacer a droite"),
      actionButton(ns("btn_modifier"), NULL, icon = bsicons::bs_icon("pencil-square"),
                   class = "btn-sm btn-outline-primary",
                   style = "padding: 2px 8px;", title = "Modifier"),
      actionButton(ns("btn_suppr"), NULL, icon = bsicons::bs_icon("trash"),
                   class = "btn-sm btn-outline-danger",
                   style = "padding: 2px 8px;", title = "Supprimer")))
}

# ==============================================================================
# Serveur
# ==============================================================================

mod_dash_element_server <- function(id, projet_r, id_page, id_ligne, id_element,
                                     element_edition_r = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Element courant ----------------------------------------------------
    element_r <- reactive({
      p <- projet_r()
      if (is.null(p$dashboard) || is.null(p$dashboard$pages)) return(NULL)
      page <- obtenir_page(p$dashboard, id_page)
      if (is.null(page) || is.null(page$lignes)) return(NULL)
      idx_l <- which(sapply(page$lignes, function(l) identical(l$id, id_ligne)))
      if (length(idx_l) == 0) return(NULL)
      ligne <- page$lignes[[idx_l[1]]]
      if (is.null(ligne$elements) || length(ligne$elements) == 0) return(NULL)
      idx_e <- which(sapply(ligne$elements, function(e) identical(e$id, id_element)))
      if (length(idx_e) == 0) return(NULL)
      ligne$elements[[idx_e[1]]]
    })

    # --- Rendu info ---------------------------------------------------------
    output$info_element <- renderUI({
      el <- element_r()
      if (is.null(el)) {
        return(tags$em(style = "color: #94a3b8; font-size: 0.85rem;",
                       "Element introuvable"))
      }

      p <- projet_r()
      infos <- rboard_libelle_element(el, p)

      # --- Nom principal ---
      nom <- as.character(el$nom_affiche %||% "")[1]
      if (!nzchar(nom)) {
        type_el <- as.character(el$type)[1]
        if (identical(type_el, "kpi")) {
          k <- p$kpi[[el$id_kpi]]
          nom <- if (!is.null(k)) as.character(k$nom)[1] else "KPI inconnu"
        } else if (identical(type_el, "graphique")) {
          g <- p$graphiques[[el$id_kpi]]
          nom <- if (!is.null(g)) as.character(g$nom)[1] else "Graphique inconnu"
        } else {
          # Texte : nom depuis la bibliotheque, sinon extrait du contenu
          id_texte <- if (!is.null(el$id_texte)) as.character(el$id_texte)[1] else NULL
          if (!is.null(id_texte) && nzchar(id_texte) &&
              !is.null(p$textes) && !is.null(p$textes[[id_texte]])) {
            nom <- as.character(p$textes[[id_texte]]$nom %||% "Texte (biblio)")[1]
          } else {
            txt <- as.character(el$contenu %||% "")[1]
            nom <- if (nzchar(txt)) {
              premier_mot <- substr(txt, 1, 40)
              if (nchar(txt) > 40) paste0(premier_mot, "...") else premier_mot
            } else {
              "Texte"
            }
          }
        }
      }
      if (nchar(nom) > 40) nom <- paste0(substr(nom, 1, 37), "...")

      # --- Largeur ---
      taille <- rboard_largeur_element(el)

      # --- Hauteur ---
      hauteur <- rboard_niveau_element(el, p)

      # --- Couleur du badge hauteur ---
      couleur_h <- if (hauteur >= 4L) "#be185d"
                  else if (hauteur >= 3L) "#7c3aed"
                  else if (hauteur == 2L) "#0891b2"
                  else "#64748b"

      # --- Petite icone supplementaire pour indiquer la source biblio ---
      id_texte <- if (!is.null(el$id_texte)) as.character(el$id_texte)[1] else NULL
      source_biblio <- !is.null(id_texte) && nzchar(id_texte) &&
                       !is.null(p$textes) && !is.null(p$textes[[id_texte]])

      tags$div(
        style = "display: flex; align-items: center; gap: 8px; min-width: 0; flex: 1;",

        # ID
        tags$code(
          style = paste0(
            "font-size: 0.75rem; font-weight: 700; ",
            "background: #1e293b; color: #ffffff; ",
            "padding: 2px 6px; border-radius: 3px; ",
            "letter-spacing: 0.05em; flex-shrink: 0;"),
          el$id),

        # Badge type
        tags$span(
          class = "badge",
          style = sprintf(
            paste0("background: %s; color: #ffffff; font-weight: 500; ",
                   "font-size: 0.72rem; display: inline-flex; align-items: center; ",
                   "gap: 4px; flex-shrink: 0;"),
            infos$couleur),
          bsicons::bs_icon(infos$icone), infos$libelle),

        # Nom
        tags$span(
          style = paste0(
            "font-size: 0.85rem; color: #334155; ",
            "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; ",
            "min-width: 0;"),
          nom),

        # Badge largeur
        tags$span(
          class = "badge bg-light text-dark border",
          style = "font-size: 0.7rem; flex-shrink: 0;",
          bsicons::bs_icon("arrows-angle-expand"),
          sprintf(" %d col", taille)),

        # Badge hauteur
        tags$span(
          class = "badge",
          style = sprintf(
            paste0("background: %s; color: #ffffff; ",
                   "font-size: 0.7rem; flex-shrink: 0;"),
            couleur_h),
          bsicons::bs_icon("arrows-angle-contract"),
          sprintf(" %d unites", hauteur))
      )
    })

    # --- Actions ------------------------------------------------------------
    observeEvent(input$btn_gauche, {
      p <- projet_r()
      p$dashboard <- deplacer_element(p$dashboard, id_page, id_ligne, id_element, "gauche")
      p$date_modification <- Sys.Date()
      projet_r(p)
    })

    observeEvent(input$btn_droite, {
      p <- projet_r()
      p$dashboard <- deplacer_element(p$dashboard, id_page, id_ligne, id_element, "droite")
      p$date_modification <- Sys.Date()
      projet_r(p)
    })

    observeEvent(input$btn_suppr, {
      p <- projet_r()
      p$dashboard <- supprimer_element(p$dashboard, id_page, id_ligne, id_element)
      p$date_modification <- Sys.Date()
      projet_r(p)
    })

    observeEvent(input$btn_modifier, {
      el <- element_r(); req(el)
      if (!is.null(element_edition_r)) {
        element_edition_r(list(
          id_page = id_page, id_ligne = id_ligne,
          id_element = id_element, element = el))
      }
    })
  })
}