#' ==============================================================================
#' Rboard - Export PDF du dashboard (A4 paysage)
#' ==============================================================================

A4_LAND_W_IN   <- 11.69
A4_LAND_H_IN   <- 8.27
PDF_MARGIN_IN  <- 0.35
PDF_TITLE_H_IN <- 0.50

PDF_ROW_UNIT_IN  <- 0.92
PDF_INNER_GAP_IN <- 0.10
PDF_ROW_GAP_IN   <- 0.14

#' Strip markdown -> texte brut
#' @keywords internal
stripper_markdown <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return("")
  t <- txt
  t <- gsub("\\*\\*\\*(.+?)\\*\\*\\*", "\\1", t, perl = TRUE)
  t <- gsub("\\*\\*(.+?)\\*\\*", "\\1", t, perl = TRUE)
  t <- gsub("__(.+?)__", "\\1", t, perl = TRUE)
  t <- gsub("\\*(.+?)\\*", "\\1", t, perl = TRUE)
  t <- gsub("_(.+?)_", "\\1", t, perl = TRUE)
  t <- gsub("~~(.+?)~~", "\\1", t, perl = TRUE)
  t <- gsub("`(.+?)`", "\\1", t, perl = TRUE)
  t <- gsub("^#{1,6}\\s+", "", t, perl = TRUE)
  t <- gsub("<u>(.+?)</u>", "\\1", t, perl = TRUE)
  t <- gsub("<[^>]+>", "", t)
  t
}

# ==============================================================================
# Export principal
# ==============================================================================

exporter_pdf_dashboard <- function(projet, chemin, titre = NULL, pages_a_exporter = NULL) {
  if (is.null(projet$dashboard) || length(projet$dashboard$pages) == 0) {
    stop("Le projet ne contient aucune page.", call. = FALSE)
  }
  pages <- projet$dashboard$pages
  if (!is.null(pages_a_exporter)) {
    pages <- Filter(function(p) p$id %in% pages_a_exporter, pages)
    if (length(pages) == 0) stop("Aucune page selectionnee.", call. = FALSE)
  }
  titre_final <- if (!is.null(titre) && nzchar(trimws(titre))) trimws(titre)
                 else if (!is.null(projet$nom) && nzchar(projet$nom)) trimws(projet$nom)
                 else "Rapport Rboard"

  grDevices::pdf(chemin, width = A4_LAND_W_IN, height = A4_LAND_H_IN, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  for (i in seq_along(pages)) {
    dessiner_page_pdf(pages[[i]], projet, titre_final,
                      numero = i, total = length(pages))
  }
  invisible(chemin)
}

# ==============================================================================
# Dessin d'une page
# ==============================================================================

dessiner_page_pdf <- function(page, projet, titre, numero = 1, total = 1) {
  grid::grid.newpage()

  x_left   <- PDF_MARGIN_IN
  y_bottom <- PDF_MARGIN_IN
  W <- A4_LAND_W_IN - 2 * PDF_MARGIN_IN
  H <- A4_LAND_H_IN - 2 * PDF_MARGIN_IN

  grid::pushViewport(grid::viewport(
    x = grid::unit(x_left, "in"), y = grid::unit(y_bottom + H, "in"),
    width = grid::unit(W, "in"), height = grid::unit(PDF_TITLE_H_IN, "in"),
    just = c("left", "top")))
  grid::grid.text(titre, x = grid::unit(0, "npc"), y = grid::unit(0.55, "npc"),
                  just = c("left", "center"),
                  gp = grid::gpar(fontsize = 16, fontface = "bold", col = "#0f172a"))
  grid::grid.text(page$titre, x = grid::unit(1, "npc"), y = grid::unit(0.55, "npc"),
                  just = c("right", "center"),
                  gp = grid::gpar(fontsize = 12, col = "#64748b"))
  grid::grid.text(sprintf("%d / %d", numero, total),
                  x = grid::unit(1, "npc"), y = grid::unit(0.05, "npc"),
                  just = c("right", "bottom"),
                  gp = grid::gpar(fontsize = 8, col = "#94a3b8"))
  grid::popViewport()

  y_top <- y_bottom + H - PDF_TITLE_H_IN - 0.10

  if (is.null(page$lignes) || length(page$lignes) == 0) {
    grid::pushViewport(grid::viewport(
      x = grid::unit(x_left, "in"), y = grid::unit(y_bottom, "in"),
      width = grid::unit(W, "in"), height = grid::unit(y_top - y_bottom, "in"),
      just = c("left", "bottom")))
    grid::grid.text("Page vide",
                    gp = grid::gpar(fontsize = 12, col = "#94a3b8", fontface = "italic"))
    grid::popViewport()
    return(invisible(NULL))
  }

  y_courant <- y_top
  for (ligne in page$lignes) {
    if (is.null(ligne$elements) || length(ligne$elements) == 0) next

    niveaux <- vapply(ligne$elements,
                      function(el) rboard_niveau_element(el, projet),
                      integer(1))
    max_niveau <- max(niveaux)
    if (max_niveau < 1L) max_niveau <- 1L

    h_row <- max_niveau * PDF_ROW_UNIT_IN + (max_niveau - 1L) * PDF_INNER_GAP_IN
    y_row_bottom <- y_courant - h_row

    if (y_row_bottom < y_bottom - 0.001) break

    dessiner_ligne_pdf(ligne, projet, x_left, y_row_bottom, W, h_row)
    y_courant <- y_row_bottom - PDF_ROW_GAP_IN
  }
  invisible(NULL)
}

# ==============================================================================
# Placement des elements dans une ligne (4 passes)
# ==============================================================================

dessiner_ligne_pdf <- function(ligne, projet, x_left, y_bottom, w_total, h_row) {
  col_unit <- (w_total - 11 * PDF_INNER_GAP_IN) / 12

  els <- ligne$elements
  niveaux <- vapply(els, function(el) rboard_niveau_element(el, projet), integer(1))

  positions <- list()
  cols_prises <- integer(0)
  col_courant <- 0L

  place_un_element <- function(i, force_start = NULL) {
    span <- as.integer(els[[i]]$taille)
    if (is.na(span) || !(span %in% c(3L, 6L, 12L))) span <- 6L

    # Contraintes de largeur pour les tableaux
    if (identical(as.character(els[[i]]$type)[1], "kpi")) {
      if (isTRUE(els[[i]]$tableau_long)) {
        if (!(span %in% c(6L, 12L))) span <- 12L
      } else if (isTRUE(els[[i]]$tableau_etendu)) {
        span <- 6L
      }
    }

    if (!is.null(force_start)) {
      start <- as.integer(force_start)
      ok <- all((start + seq_len(span) - 1) %in% setdiff(0:11, cols_prises))
      if (!ok) return(FALSE)
      positions[[length(positions) + 1]] <<- list(
        idx = i, col_start = start, span = span, niveau = niveaux[i])
      cols_prises <<- c(cols_prises, start + seq_len(span) - 1)
      col_courant <<- start + span
      return(TRUE)
    }

    cols_libres <- setdiff(0:11, cols_prises)
    for (start in sort(cols_libres)) {
      if (all((start + seq_len(span) - 1) %in% cols_libres)) {
        positions[[length(positions) + 1]] <<- list(
          idx = i, col_start = start, span = span, niveau = niveaux[i])
        cols_prises <<- c(cols_prises, start + seq_len(span) - 1)
        return(TRUE)
      }
    }
    FALSE
  }

  for (i in seq_along(els)) {
    if (niveaux[i] != 4L) next
    place_un_element(i, force_start = col_courant)
  }
  for (i in seq_along(els)) {
    if (niveaux[i] != 3L) next
    place_un_element(i, force_start = col_courant)
  }
  for (i in seq_along(els)) {
    if (niveaux[i] != 2L) next
    place_un_element(i)
  }
  for (i in seq_along(els)) {
    if (niveaux[i] != 1L) next
    place_un_element(i)
  }

  for (pos in positions) {
    el <- els[[pos$idx]]
    span <- pos$span
    w_el <- span * col_unit + (span - 1) * PDF_INNER_GAP_IN
    x_el <- x_left + pos$col_start * (col_unit + PDF_INNER_GAP_IN)

    niveau_el <- as.integer(pos$niveau)
    h_el <- niveau_el * PDF_ROW_UNIT_IN + (niveau_el - 1L) * PDF_INNER_GAP_IN
    y_el <- y_bottom + h_row - h_el

    dessiner_element_pdf(el, projet, x_el, y_el, w_el, h_el, niveau_el)
  }
  invisible(NULL)
}

# ==============================================================================
# Dessin d'un element
# ==============================================================================

dessiner_element_pdf <- function(el, projet, x, y, w, h, niveau = 1L) {
  type_el <- as.character(el$type)[1]
  if (identical(type_el, "kpi")) {
    k <- projet$kpi[[el$id_kpi]]
    if (is.null(k)) {
      dessiner_placeholder_pdf(x, y, w, h, "KPI introuvable")
      return(invisible(NULL))
    }
    if (identical(as.character(k$type)[1], "tableau")) {
      dessiner_tableau_pdf(el, k, x, y, w, h, niveau)
    } else {
      dessiner_kpi_card_pdf(el, k, x, y, w, h)
    }
  } else if (identical(type_el, "graphique")) {
    g <- projet$graphiques[[el$id_kpi]]
    if (is.null(g) || is.null(g$objet)) {
      dessiner_placeholder_pdf(x, y, w, h, "Graphique introuvable")
      return(invisible(NULL))
    }
    dessiner_graphique_pdf(el, g, x, y, w, h, niveau)
  } else if (identical(type_el, "texte")) {
    dessiner_texte_pdf(el, projet, x, y, w, h, niveau)
  }
  invisible(NULL)
}

dessiner_placeholder_pdf <- function(x, y, w, h, msg) {
  grid::pushViewport(grid::viewport(
    x = grid::unit(x, "in"), y = grid::unit(y, "in"),
    width = grid::unit(w, "in"), height = grid::unit(h, "in"),
    just = c("left", "bottom")))
  grid::grid.rect(gp = grid::gpar(fill = "#fee2e2", col = "#dc2626", lwd = 1))
  grid::grid.text(msg, gp = grid::gpar(fontsize = 10, col = "#991b1b"))
  grid::popViewport()
}

dessiner_kpi_card_pdf <- function(el, kpi, x, y, w, h) {
  nom <- el$nom_affiche %||% kpi$nom
  com <- el$commentaire %||% kpi$commentaire
  couleur <- if (!is.null(el$couleur_forcee) && nzchar(el$couleur_forcee) &&
                 el$couleur_forcee != "automatique") {
    el$couleur_forcee
  } else kpi$couleur %||% "primary"
  bg <- rboard_couleur_kpi_bg(couleur)
  valeur <- formater_valeur(kpi$valeur, kpi$format)

  grid::pushViewport(grid::viewport(
    x = grid::unit(x, "in"), y = grid::unit(y, "in"),
    width = grid::unit(w, "in"), height = grid::unit(h, "in"),
    just = c("left", "bottom")))
  grid::grid.roundrect(x = grid::unit(0, "npc"), y = grid::unit(0, "npc"),
                       width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
                       r = grid::unit(0.06, "in"),
                       just = c("left", "bottom"),
                       gp = grid::gpar(fill = bg, col = NA))
  grid::grid.text(nom, x = grid::unit(0.08, "npc"), y = grid::unit(0.88, "npc"),
                  just = c("left", "center"),
                  gp = grid::gpar(fontsize = 7.5, fontface = "bold",
                                  col = "white", alpha = 0.9))
  grid::grid.text(valeur, x = grid::unit(0.08, "npc"), y = grid::unit(0.45, "npc"),
                  just = c("left", "center"),
                  gp = grid::gpar(fontsize = 18, fontface = "bold", col = "white"))
  if (!is.null(com) && nzchar(com)) {
    com_court <- if (nchar(com) > 70) paste0(substr(com, 1, 67), "...") else com
    grid::grid.text(com_court, x = grid::unit(0.08, "npc"), y = grid::unit(0.12, "npc"),
                    just = c("left", "center"),
                    gp = grid::gpar(fontsize = 7, col = "white",
                                    alpha = 0.85, fontface = "italic"))
  }
  grid::popViewport()
}

dessiner_tableau_pdf <- function(el, kpi, x, y, w, h, niveau = 2L) {
  nom <- el$nom_affiche %||% kpi$nom
  tab <- kpi$tableau

  grid::pushViewport(grid::viewport(
    x = grid::unit(x, "in"), y = grid::unit(y, "in"),
    width = grid::unit(w, "in"), height = grid::unit(h, "in"),
    just = c("left", "bottom")))
  grid::grid.roundrect(x = grid::unit(0, "npc"), y = grid::unit(0, "npc"),
                       width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
                       r = grid::unit(0.06, "in"),
                       just = c("left", "bottom"),
                       gp = grid::gpar(fill = "white", col = "#e2e8f0", lwd = 1))
  grid::grid.text(nom, x = grid::unit(0.03, "npc"), y = grid::unit(0.96, "npc"),
                  just = c("left", "top"),
                  gp = grid::gpar(fontsize = 10, fontface = "bold", col = "#0f172a"))

  if (!is.null(tab) && nrow(tab) > 0) {
    if (requireNamespace("gridExtra", quietly = TRUE)) {
      grid::pushViewport(grid::viewport(
        x = grid::unit(0.02, "npc"), y = grid::unit(0.02, "npc"),
        width = grid::unit(0.96, "npc"), height = grid::unit(0.88, "npc"),
        just = c("left", "bottom")))
      tryCatch({
        n <- nrow(tab); n_col <- ncol(tab)
        base_size <- if (as.integer(niveau) >= 4L) 5.5
                     else if (as.integer(niveau) >= 3L) 6
                     else max(5, 9 - 0.35 * n)
        base_size <- max(5, base_size - 0.15 * n)

        tg <- gridExtra::tableGrob(
          tab, rows = NULL,
          theme = gridExtra::ttheme_minimal(
            base_size = base_size,
            core = list(bg_params = list(fill = "white", col = "#cbd5e1"),
                        fg_params = list(col = "#334155")),
            colhead = list(bg_params = list(fill = "#f1f5f9", col = "#cbd5e1"),
                           fg_params = list(fontface = "bold", col = "#0f172a"))))
        tg$widths <- grid::unit(rep(1 / n_col, n_col), "npc")
        tg$heights <- grid::unit(rep(1 / (n + 1), n + 1), "npc")
        grid::grid.draw(tg)
      }, error = function(e) {
        grid::grid.text(paste("Erreur tableau :", conditionMessage(e)),
                        gp = grid::gpar(fontsize = 9, col = "#dc2626"))
      })
      grid::popViewport()
    }
  }
  grid::popViewport()
}

dessiner_graphique_pdf <- function(el, g, x, y, w, h, niveau = 3L) {
  nom <- el$nom_affiche %||% g$nom

  grid::pushViewport(grid::viewport(
    x = grid::unit(x, "in"), y = grid::unit(y, "in"),
    width = grid::unit(w, "in"), height = grid::unit(h, "in"),
    just = c("left", "bottom")))
  grid::grid.roundrect(x = grid::unit(0, "npc"), y = grid::unit(0, "npc"),
                       width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
                       r = grid::unit(0.06, "in"),
                       just = c("left", "bottom"),
                       gp = grid::gpar(fill = "white", col = "#e2e8f0", lwd = 1))
  grid::grid.text(nom, x = grid::unit(0.03, "npc"), y = grid::unit(0.96, "npc"),
                  just = c("left", "top"),
                  gp = grid::gpar(fontsize = 10, fontface = "bold", col = "#0f172a"))

  grid::pushViewport(grid::viewport(
    x = grid::unit(0.02, "npc"), y = grid::unit(0.02, "npc"),
    width = grid::unit(0.96, "npc"), height = grid::unit(0.86, "npc"),
    just = c("left", "bottom")))
  tryCatch({
    grob <- ggplot2::ggplotGrob(g$objet)
    grid::grid.draw(grob)
  }, error = function(e) {
    grid::grid.text(paste("Erreur graphique :", conditionMessage(e)),
                    gp = grid::gpar(fontsize = 9, col = "#dc2626"))
  })
  grid::popViewport()
  grid::popViewport()
}

#' Texte en PDF (markdown strip + niveau titre)
#' @keywords internal
dessiner_texte_pdf <- function(el, projet = NULL, x, y, w, h, niveau = 1L) {
  id_texte <- if (!is.null(el$id_texte)) as.character(el$id_texte)[1] else NULL

  if (!is.null(id_texte) && nzchar(id_texte) &&
      !is.null(projet) && !is.null(projet$textes) &&
      !is.null(projet$textes[[id_texte]])) {
    t <- projet$textes[[id_texte]]
    txt_md <- as.character(t$contenu %||% "")[1]
    stl <- as.character(t$style %||% "normal")[1]
  } else {
    txt_md <- as.character(el$contenu %||% "")[1]
    stl <- as.character(el$style %||% "normal")[1]
  }
  if (is.na(stl) || !nzchar(stl)) stl <- "normal"

  txt <- stripper_markdown(txt_md)

  bg <- switch(stl, "titre" = "#eff6ff", "note" = "#ecfeff",
               "avertissement" = "#fff7ed", "#f8fafc")
  border <- switch(stl, "titre" = "#2563eb", "note" = "#0891b2",
                   "avertissement" = "#ea580c", "#e2e8f0")
  fg <- switch(stl, "titre" = "#1e40af", "note" = "#155e75",
               "avertissement" = "#7c2d12", "#334155")

  font_size <- if (identical(stl, "titre")) {
    nt <- suppressWarnings(as.integer(el$niveau_titre %||% 2L)[1])
    if (is.na(nt) || !(nt %in% c(1L, 2L, 3L))) nt <- 2L
    switch(as.character(nt), "1" = 20, "2" = 16, "3" = 13, 16)
  } else if (identical(stl, "note") || identical(stl, "avertissement")) {
    10
  } else {
    10
  }

  grid::pushViewport(grid::viewport(
    x = grid::unit(x, "in"), y = grid::unit(y, "in"),
    width = grid::unit(w, "in"), height = grid::unit(h, "in"),
    just = c("left", "bottom")))
  grid::grid.roundrect(x = grid::unit(0, "npc"), y = grid::unit(0, "npc"),
                       width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
                       r = grid::unit(0.06, "in"),
                       just = c("left", "bottom"),
                       gp = grid::gpar(fill = bg, col = border, lwd = 1.5))

  char_w_in <- font_size / 72 * 0.55
  max_chars <- max(10, floor((w - 0.2) / char_w_in))
  txt_wrapped <- paste(strwrap(txt, width = max_chars), collapse = "\n")

  grid::grid.text(txt_wrapped, x = grid::unit(0.08, "npc"), y = grid::unit(0.5, "npc"),
                  just = c("left", "center"),
                  gp = grid::gpar(fontsize = font_size, col = fg,
                                  fontface = if (identical(stl, "titre")) "bold" else "plain",
                                  lineheight = 1.2))
  grid::popViewport()
}