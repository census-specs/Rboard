#' ==============================================================================
#' Rboard - Apercu du dashboard
#' ==============================================================================

rboard_couleur_kpi_bg <- function(cle) {
  switch(as.character(cle),
    "primary" = "#2563eb", "success" = "#16a34a", "warning" = "#ea580c",
    "danger" = "#dc2626", "info" = "#0891b2", "secondary" = "#64748b",
    "dark" = "#1e293b", "light" = "#e2e8f0", "#2563eb")
}

rboard_couleur_kpi_fg <- function(cle) {
  if (identical(as.character(cle), "light")) "#0f172a" else "#ffffff"
}

# ==============================================================================
# Markdown
# ==============================================================================

#' @export
rboard_md_to_html <- function(txt) {
  if (is.null(txt) || length(txt) == 0) return(htmltools::HTML(""))
  txt <- as.character(txt)[1]
  if (is.na(txt) || !nzchar(txt)) return(htmltools::HTML(""))

  if (requireNamespace("commonmark", quietly = TRUE)) {
    html <- tryCatch(
      commonmark::markdown_html(txt, extensions = TRUE, smart = FALSE, hardbreaks = TRUE),
      error = function(e) htmltools::htmlEscape(txt)
    )
    html <- paste0(
      "<div style=\"display:block;\">",
      "<style scoped>.rboard-md-host p{margin:0;padding:0;display:inline}.rboard-md-host p+p{display:block;margin-top:3px}.rboard-md-host ul{margin:0;padding-left:16px}.rboard-md-host code{background:#f1f5f9;padding:1px 4px;border-radius:3px;font-family:monospace;font-size:0.9em}</style>",
      "<span class=\"rboard-md-host\">",
      html,
      "</span></div>"
    )
    return(htmltools::HTML(html))
  }

  h <- htmltools::htmlEscape(txt)
  h <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", h, perl = TRUE)
  h <- gsub("\\*(.+?)\\*", "<em>\\1</em>", h, perl = TRUE)
  h <- gsub("<u>(.+?)</u>", "<u>\\1</u>", h, perl = TRUE)
  htmltools::HTML(h)
}

# ==============================================================================
# Constantes de dimensions
# ==============================================================================

RBOARD_UNITE_HAUTEUR <- 100L
RBOARD_GAP           <- 16L
RBOARD_HAUTEUR_PAGE_MAX <- 720L

# ==============================================================================
# Niveau d'un element (= sa hauteur en unites)
# ==============================================================================

#' @export
rboard_niveau_element <- function(el, projet = NULL) {
  if (is.null(el)) return(1L)

  h <- suppressWarnings(as.integer(el$hauteur_unites)[1])
  if (!is.na(h) && h %in% c(1L, 2L, 3L, 4L)) return(h)

  type_el <- as.character(el$type)[1]
  if (identical(type_el, "graphique")) {
    return(if (isTRUE(el$graphique_long)) HAUTEUR_GRAPHIQUE_LARGE else HAUTEUR_GRAPHIQUE)
  }
  if (identical(type_el, "kpi") && !is.null(projet)) {
    k <- projet$kpi[[el$id_kpi]]
    if (!is.null(k) && identical(as.character(k$type)[1], "tableau")) {
      if (isTRUE(el$tableau_long))   return(HAUTEUR_TABLEAU_LONG)
      if (isTRUE(el$tableau_etendu)) return(HAUTEUR_TABLEAU_ETENDU)
      return(HAUTEUR_TABLEAU)
    }
  }
  1L
}

# ==============================================================================
# Hauteurs en pixels
# ==============================================================================

#' @export
rboard_hauteur_ligne <- function(ligne, projet = NULL) {
  if (is.null(ligne) || is.null(ligne$elements) || length(ligne$elements) == 0) {
    return(0L)
  }
  niveaux <- vapply(ligne$elements,
                    function(el) rboard_niveau_element(el, projet),
                    integer(1))
  h_u <- max(niveaux)
  if (h_u == 0L) return(0L)
  as.integer(h_u * RBOARD_UNITE_HAUTEUR + (h_u - 1L) * RBOARD_GAP)
}

#' @export
rboard_hauteur_page <- function(page, projet = NULL) {
  if (is.null(page) || is.null(page$lignes) || length(page$lignes) == 0) {
    return(0L)
  }
  hauteurs <- vapply(page$lignes,
                     function(l) rboard_hauteur_ligne(l, projet),
                     integer(1))
  hauteurs <- hauteurs[hauteurs > 0L]
  if (length(hauteurs) == 0L) return(0L)
  as.integer(sum(hauteurs) + max(0L, length(hauteurs) - 1L) * RBOARD_GAP)
}

# ==============================================================================
# Config taille
# ==============================================================================

#' @export
rboard_config_taille <- function(type, taille, hauteur_unites = NULL, style = "normal") {
  t <- as.integer(taille)
  if (is.na(t) || !(t %in% c(3L, 6L, 12L))) t <- 6L

  h <- suppressWarnings(as.integer(hauteur_unites)[1])
  if (is.na(h) || !(h %in% c(1L, 2L, 3L, 4L))) h <- 1L

  pad_h <- if (t >= 12L) 20L else if (t >= 6L) 16L else 12L
  pad_v <- if (t >= 12L) 14L else if (t >= 6L) 12L else 10L

  if (identical(type, "kpi")) {
    list(pad_h = pad_h, pad_v = pad_v,
         font_titre = if (t >= 12L) 0.80 else if (t >= 6L) 0.72 else 0.66,
         font_valeur = if (t >= 12L) 2.60 else if (t >= 6L) 2.20 else 1.70,
         font_com = if (t >= 12L) 0.76 else if (t >= 6L) 0.70 else 0.66,
         font_icone = if (t >= 12L) 1.6 else if (t >= 6L) 1.35 else 1.2)
  } else if (identical(type, "tableau")) {
    font_base <- if (h >= 4L) {
      if (t >= 12L) 0.78 else 0.72
    } else if (h >= 3L) {
      if (t >= 12L) 0.82 else 0.76
    } else {
      if (t >= 12L) 0.88 else 0.82
    }
    list(pad_h = pad_h, pad_v = pad_v,
         font_titre = if (t >= 12L) 0.88 else 0.82,
         font_base = font_base)
  } else if (identical(type, "graphique")) {
    list(plot_h = NULL, pad = 8L,
         font_titre = if (t >= 12L) 0.88 else 0.84)
  } else {
    list(pad_h = pad_h, pad_v = pad_v,
         font = if (t >= 12L) 0.95 else if (t >= 6L) 0.90 else 0.85,
         lignes = if (h >= 2L) 5L else if (t >= 12L) 5L else if (t >= 6L) 4L else 3L)
  }
}

# ==============================================================================
# Rendu KPI box
# ==============================================================================

rboard_rendre_kpi_box <- function(elem, kpi, taille = 6, hauteur_unites = 1L) {
  cfg <- rboard_config_taille("kpi", taille, hauteur_unites)
  nom <- elem$nom_affiche %||% kpi$nom
  com <- elem$commentaire %||% kpi$commentaire
  couleur <- if (!is.null(elem$couleur_forcee) && nzchar(elem$couleur_forcee) &&
                 elem$couleur_forcee != "automatique") {
    elem$couleur_forcee
  } else kpi$couleur %||% "primary"
  bg <- rboard_couleur_kpi_bg(couleur)
  fg <- rboard_couleur_kpi_fg(couleur)
  icone <- icone_type_kpi(kpi$type)
  valeur_affichee <- formater_valeur(kpi$valeur, kpi$format)

  tags$div(
    style = sprintf(
      paste0("background: %s; color: %s; border-radius: 10px; ",
             "padding: %dpx %dpx; height: 100%%; width: 100%%; ",
             "box-sizing: border-box; overflow: hidden; ",
             "box-shadow: 0 2px 6px rgba(0,0,0,0.08); ",
             "display: flex; flex-direction: column; justify-content: space-between;"),
      bg, fg, cfg$pad_v, cfg$pad_h),
    tags$div(style = sprintf("font-size: %srem; opacity: 0.88; font-weight: 600; text-transform: uppercase; letter-spacing: 0.04em; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; flex-shrink: 0;",
                             cfg$font_titre), nom),
    tags$div(
      style = "display: flex; align-items: center; justify-content: space-between; gap: 8px; min-width: 0; flex: 1;",
      tags$div(style = sprintf("font-size: %srem; font-weight: 800; line-height: 1.05; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; flex: 1; min-width: 0;",
                               cfg$font_valeur), valeur_affichee),
      tags$div(style = sprintf("font-size: %srem; opacity: 0.85; flex-shrink: 0;", cfg$font_icone), icone)),
    if (!is.null(com) && nzchar(com)) {
      tags$div(style = sprintf("font-size: %srem; opacity: 0.85; font-style: italic; line-height: 1.2; overflow: hidden; white-space: nowrap; text-overflow: ellipsis; flex-shrink: 0;",
                               cfg$font_com), com)
    } else tags$div(style = "height: 0;"))
}

# ==============================================================================
# Rendu tableau
# ==============================================================================

rboard_rendre_kpi_tableau <- function(elem, kpi, taille = 6, hauteur_unites = 2L) {
  cfg <- rboard_config_taille("tableau", taille, hauteur_unites)
  nom <- elem$nom_affiche %||% kpi$nom
  com <- elem$commentaire %||% kpi$commentaire
  regles <- if (!is.null(kpi$regles_couleur)) kpi$regles_couleur else list()

  contenu_tableau <- rboard_tableau_html(
    tableau = kpi$tableau, regles = regles,
    colonne_valeur_index = kpi$colonne_valeur_index,
    max_rows = NULL, compact = FALSE, font_base = cfg$font_base)

  annexes <- kpi$stats_annexes
  bloc_annexes <- NULL
  if (!is.null(annexes) && length(annexes) > 0) {
    items <- list()
    if (!is.null(annexes$p_value)) {
      items[[length(items) + 1]] <- tags$span(
        style = sprintf("font-size: %srem; color: #475569; margin-right: 14px;", cfg$font_base - 0.02),
        tags$strong("p : "), formater_valeur(annexes$p_value, "decimal_3"))
    }
    if (!is.null(annexes$v_cramer)) {
      items[[length(items) + 1]] <- tags$span(
        style = sprintf("font-size: %srem; color: #475569;", cfg$font_base - 0.02),
        tags$strong("V : "), formater_valeur(annexes$v_cramer, "decimal_2"))
    }
    if (length(items) > 0) {
      bloc_annexes <- tags$div(
        style = "margin-top: 4px; display: flex; flex-wrap: wrap; align-items: center; flex-shrink: 0;",
        items)
    }
  }

  tags$div(
    style = sprintf(
      paste0("background: #ffffff; border: 1px solid #e2e8f0; border-radius: 10px; ",
             "padding: %dpx %dpx; height: 100%%; width: 100%%; ",
             "box-shadow: 0 2px 6px rgba(0,0,0,0.05); ",
             "box-sizing: border-box; overflow: hidden; ",
             "display: flex; flex-direction: column;"),
      cfg$pad_v, cfg$pad_h),
    tags$div(style = sprintf("font-size: %srem; font-weight: 600; color: #0f172a; margin-bottom: 4px; display: flex; align-items: center; gap: 6px; flex-shrink: 0;",
                             cfg$font_titre),
             bsicons::bs_icon("table"), nom),
    tags$div(style = "flex: 1; min-height: 0; overflow: hidden;", contenu_tableau),
    if (!is.null(bloc_annexes)) bloc_annexes,
    if (!is.null(com) && nzchar(com)) {
      tags$p(style = sprintf("font-size: %srem; color: #64748b; font-style: italic; margin: 4px 0 0 0; flex-shrink: 0;",
                             cfg$font_base - 0.05), com)
    })
}

rboard_rendre_kpi <- function(elem, projet, taille = 6, hauteur_unites = NULL) {
  k <- projet$kpi[[elem$id_kpi]]
  if (is.null(k)) {
    return(tags$div(class = "alert alert-warning",
                    sprintf("KPI '%s' introuvable.", elem$id_kpi)))
  }

  h <- hauteur_unites
  if (is.null(h)) h <- rboard_niveau_element(elem, projet)

  if (identical(as.character(k$type)[1], "tableau")) {
    rboard_rendre_kpi_tableau(elem, k, taille, h)
  } else {
    rboard_rendre_kpi_box(elem, k, taille, h)
  }
}

# ==============================================================================
# Rendu graphique encapsule dans une tuile
# ==============================================================================

rboard_rendre_graphique_ui <- function(elem, projet, prefixe, taille = 6,
                                        hauteur_unites = NULL, ns = NULL) {
  h <- hauteur_unites
  if (is.null(h)) h <- rboard_niveau_element(elem, projet)

  cfg <- rboard_config_taille("graphique", taille, h)
  g_obj <- projet$graphiques[[elem$id_kpi]]
  nom <- elem$nom_affiche %||% (if (!is.null(g_obj)) g_obj$nom else "Graphique")
  com <- elem$commentaire
  plot_id <- if (!is.null(ns)) ns(prefixe) else prefixe

  tags$div(
    style = paste0(
      "background: #ffffff; border: 1px solid #e2e8f0; border-radius: 10px; ",
      "padding: 0; height: 100%; width: 100%; ",
      "box-shadow: 0 2px 6px rgba(0,0,0,0.05); box-sizing: border-box; ",
      "overflow: hidden; display: flex; flex-direction: column;"),
    tags$div(
      style = sprintf(
        paste0("font-size: %srem; font-weight: 600; color: #0f172a; ",
               "padding: 10px 14px; border-bottom: 1px solid #f1f5f9; ",
               "display: flex; align-items: center; gap: 6px; ",
               "flex-shrink: 0; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"),
        cfg$font_titre),
      bsicons::bs_icon("bar-chart-fill", style = "color: #64748b; flex-shrink: 0;"),
      tags$span(style = "min-width: 0; overflow: hidden; text-overflow: ellipsis;", nom)),
    tags$div(
      style = "flex: 1; min-height: 0; padding: 6px 8px 4px 8px; position: relative; overflow: hidden;",
      shiny::plotOutput(plot_id, height = "100%", width = "100%")),
    if (!is.null(com) && nzchar(com)) {
      tags$div(
        style = "padding: 0 12px 8px 12px; flex-shrink: 0;",
        tags$p(style = "font-size: 0.72rem; color: #64748b; font-style: italic; margin: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
               com))
    }
  )
}

# ==============================================================================
# Rendu texte
# ==============================================================================

rboard_rendre_texte <- function(elem, projet = NULL, taille = 6, hauteur_unites = 1L) {
  cfg <- rboard_config_taille("texte", taille, hauteur_unites)

  id_texte <- if (!is.null(elem$id_texte)) as.character(elem$id_texte)[1] else NULL

  if (!is.null(id_texte) && nzchar(id_texte) &&
      !is.null(projet) && !is.null(projet$textes) &&
      !is.null(projet$textes[[id_texte]])) {
    t <- projet$textes[[id_texte]]
    txt <- as.character(t$contenu %||% "")[1]
    stl <- as.character(t$style %||% "normal")[1]
  } else {
    txt <- as.character(elem$contenu %||% "")[1]
    stl <- as.character(elem$style %||% "normal")[1]
  }

  if (is.na(stl) || !nzchar(stl)) stl <- "normal"

  html_md <- rboard_md_to_html(txt)
  pad_h <- cfg$pad_h
  pad_v <- cfg$pad_v

  if (identical(stl, "titre")) {
    nt <- suppressWarnings(as.integer(elem$niveau_titre %||% 2L)[1])
    if (is.na(nt) || !(nt %in% c(1L, 2L, 3L))) nt <- 2L

    font_size <- switch(as.character(nt),
      "1" = 1.55, "2" = 1.30, "3" = 1.10, 1.30)

    return(tags$div(
      style = sprintf(
        paste0("border-radius: 10px; padding: %dpx %dpx; ",
               "height: 100%%; width: 100%%; box-sizing: border-box; ",
               "background: #eff6ff; border-left: 4px solid #2563eb; ",
               "color: #1e40af; font-weight: 700; ",
               "font-size: %srem; line-height: 1.25; ",
               "box-shadow: 0 2px 6px rgba(0,0,0,0.05); ",
               "display: flex; flex-direction: column; justify-content: center; ",
               "overflow: hidden;"),
        pad_v, pad_h, font_size),
      html_md
    ))
  }

  if (identical(stl, "note") || identical(stl, "avertissement")) {
    bg <- if (identical(stl, "note")) "#ecfeff" else "#fff7ed"
    border <- if (identical(stl, "note")) "#0891b2" else "#ea580c"
    fg <- if (identical(stl, "note")) "#155e75" else "#7c2d12"
    icone <- if (identical(stl, "note")) "info-circle-fill" else "exclamation-triangle-fill"

    return(tags$div(
      style = sprintf(
        paste0("border-radius: 10px; padding: %dpx %dpx; ",
               "height: 100%%; width: 100%%; box-sizing: border-box; ",
               "background: %s; border-left: 4px solid %s; color: %s; ",
               "font-size: %srem; line-height: 1.4; ",
               "box-shadow: 0 2px 6px rgba(0,0,0,0.05); ",
               "display: flex; flex-direction: column; justify-content: center; ",
               "overflow: hidden;"),
        pad_v, pad_h, bg, border, fg, cfg$font),
      tags$div(
        style = "display: flex; align-items: flex-start; gap: 10px; min-width: 0;",
        tags$div(style = sprintf("flex-shrink: 0; margin-top: 2px; font-size: %srem;", cfg$font * 1.15),
                 bsicons::bs_icon(icone)),
        tags$div(style = "min-width: 0; flex: 1;", html_md))
    ))
  }

  tags$div(
    style = sprintf(
      paste0("border-radius: 10px; padding: %dpx %dpx; ",
             "height: 100%%; width: 100%%; box-sizing: border-box; ",
             "background: #f8fafc; border: 1px solid #e2e8f0; color: #334155; ",
             "font-size: %srem; line-height: 1.4; ",
             "box-shadow: 0 2px 6px rgba(0,0,0,0.05); ",
             "display: flex; flex-direction: column; justify-content: center; ",
             "overflow: hidden;"),
      pad_v, pad_h, cfg$font),
    html_md
  )
}

# ==============================================================================
# Rendu ligne
# ==============================================================================

#' @export
rboard_rendre_ligne <- function(ligne, projet, prefixe = "apercu", ns = NULL) {
  if (is.null(ligne$elements) || length(ligne$elements) == 0) {
    return(tags$div(
      style = "padding: 16px; text-align: center; color: #cbd5e1; font-style: italic; font-size: 0.85rem; border: 1px dashed #e2e8f0; border-radius: 8px; margin-bottom: 16px;",
      "Ligne vide"))
  }

  els <- ligne$elements
  niveaux <- vapply(els, function(el) rboard_niveau_element(el, projet), integer(1))
  h_max <- max(niveaux)
  if (is.na(h_max) || h_max < 1L) h_max <- 1L

  ordre <- order(-niveaux)
  els <- els[ordre]
  niveaux <- niveaux[ordre]

  elements_ui <- lapply(seq_along(els), function(i) {
    el <- els[[i]]
    niveau <- as.integer(niveaux[i])
    w_int <- rboard_largeur_element(el)

    # Contraintes de largeur pour les tableaux
    if (identical(as.character(el$type)[1], "kpi")) {
      k <- projet$kpi[[el$id_kpi]]
      if (!is.null(k) && identical(as.character(k$type)[1], "tableau")) {
        if (isTRUE(el$tableau_long)) {
          # Tableau long : 6 ou 12, fallback 12 si invalide
          if (!(w_int %in% c(6L, 12L))) w_int <- 12L
        } else if (isTRUE(el$tableau_etendu)) {
          w_int <- 6L
        }
      }
    }

    prefixe_el <- paste0(prefixe, "_", el$id)
    contenu <- if (identical(as.character(el$type)[1], "kpi")) {
      rboard_rendre_kpi(el, projet, taille = w_int, hauteur_unites = niveau)
    } else if (identical(as.character(el$type)[1], "graphique")) {
      rboard_rendre_graphique_ui(el, projet, prefixe_el, taille = w_int,
                                  hauteur_unites = niveau, ns = ns)
    } else {
      rboard_rendre_texte(el, projet = projet, taille = w_int,
                          hauteur_unites = niveau)
    }

    tags$div(
      style = sprintf(
        "grid-column: span %d; grid-row: span %d; min-width: 0; min-height: 0; overflow: hidden;",
        w_int, niveau),
      contenu)
  })

  tags$div(
    style = sprintf(
      paste0("display: grid; ",
             "grid-template-columns: repeat(12, minmax(0, 1fr)); ",
             "grid-template-rows: repeat(%d, var(--rboard-unit, %dpx)); ",
             "gap: var(--rboard-gap, %dpx); ",
             "margin-bottom: var(--rboard-gap, %dpx); ",
             "align-items: stretch;"),
      h_max, RBOARD_UNITE_HAUTEUR, RBOARD_GAP, RBOARD_GAP),
    elements_ui)
}

# ==============================================================================
# Rendu page complete
# ==============================================================================

#' @export
rboard_rendre_page <- function(page, projet, prefixe = "apercu", fond = "#ffffff", ns = NULL) {
  if (is.null(page)) return(NULL)

  lignes_ui <- if (is.null(page$lignes) || length(page$lignes) == 0) {
    rboard_etat_vide("layout-three-columns", "Page vide",
                     "Ajoutez une ligne pour commencer.")
  } else {
    lapply(page$lignes, function(l) rboard_rendre_ligne(l, projet, prefixe, ns))
  }

  tags$div(
    style = sprintf("background: %s; padding: 20px; border-radius: 8px; width: 100%%; box-sizing: border-box;", fond),
    tags$h3(style = "font-weight: 700; color: #0f172a; margin: 0 0 16px 0; font-size: 1.4rem;",
            page$titre),
    tagList(lignes_ui))
}

# ==============================================================================
# Enregistrement des plots
# ==============================================================================

rboard_enregistrer_plots <- function(projet_r, output, prefixe = "apercu") {
  proj <- tryCatch(projet_r(), error = function(e) NULL)
  if (is.null(proj) || is.null(proj$dashboard) || is.null(proj$dashboard$pages)) return()

  for (page in proj$dashboard$pages) {
    if (is.null(page$lignes)) next
    for (ligne in page$lignes) {
      if (is.null(ligne$elements)) next
      for (el in ligne$elements) {
        if (!identical(as.character(el$type)[1], "graphique")) next
        local({
          my_el <- el
          out_id <- paste0(prefixe, "_", my_el$id)
          output[[out_id]] <- shiny::renderPlot({
            p_now <- tryCatch(projet_r(), error = function(e) NULL)
            if (is.null(p_now)) {
              plot.new(); par(mar = c(0, 0, 0, 0))
              text(0.5, 0.5, "Chargement...", col = "#94a3b8", cex = 1.1)
              return(invisible(NULL))
            }
            g_obj <- p_now$graphiques[[my_el$id_kpi]]
            if (is.null(g_obj) || is.null(g_obj$objet)) {
              plot.new(); par(mar = c(0, 0, 0, 0))
              text(0.5, 0.5, sprintf("Graphique '%s' introuvable", my_el$id_kpi),
                   col = "#94a3b8", cex = 1.1)
              return(invisible(NULL))
            }
            tryCatch(print(g_obj$objet), error = function(e) {
              plot.new(); par(mar = c(0, 0, 0, 0))
              text(0.5, 0.5, paste("Erreur :", conditionMessage(e)), col = "#dc2626", cex = 1)
            })
          }, res = 96)
        })
      }
    }
  }
}

# ==============================================================================
# Module apercu
# ==============================================================================

mod_dash_apercu_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("zone_apercu"))
}

mod_dash_apercu_server <- function(id, projet_r, page_courante_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      p <- projet_r(); req(p)
      rboard_enregistrer_plots(projet_r, output, prefixe = "apercu")
    })

    output$zone_apercu <- renderUI({
      p <- projet_r()
      id_page <- page_courante_r()
      if (is.null(id_page)) return(rboard_etat_vide("grid-1x2", "Aucune page"))
      page <- obtenir_page(p$dashboard, id_page)
      if (is.null(page)) return(NULL)
      rboard_rendre_page(page, p, prefixe = "apercu", fond = "#ffffff", ns = ns)
    })
  })
}