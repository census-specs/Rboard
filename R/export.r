#' ==============================================================================
#' Rboard - Moteur d'exportation
#' ==============================================================================

#' Ouvrir le dossier contenant un fichier (multi-plateforme)
#' @keywords internal
rboard_ouvrir_dossier <- function(chemin) {
  dossier <- dirname(chemin)
  os <- Sys.info()[["sysname"]]
  tryCatch({
    if (os == "Windows") shell.exec(dossier)
    else if (os == "Darwin") system2("open", shQuote(dossier))
    else system2("xdg-open", shQuote(dossier))
    TRUE
  }, error = function(e) FALSE)
}

# ==============================================================================
# Themes HTML
# ==============================================================================

rboard_themes_html <- function() {
  c(
    "Flatly (bleu sobre)"   = "flatly",
    "Cosmo (bleu clair)"    = "cosmo",
    "Journal (magazine)"    = "journal",
    "Darkly (mode sombre)"  = "darkly",
    "Minty (vert frais)"    = "minty",
    "Lux (elegant dore)"    = "lux"
  )
}

#' Retourne les variables CSS pour un theme
#' @keywords internal
rboard_theme_css_vars <- function(theme) {
  switch(theme,
    "flatly"  = list(pri = "#2c3e50", bg = "#f8f9fa", card = "#ffffff",
                     txt = "#212529", muted = "#6c757d", bd = "#dee2e6",
                     font = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif"),
    "cosmo"   = list(pri = "#2780e3", bg = "#f5f7fa", card = "#ffffff",
                     txt = "#111827", muted = "#6b7280", bd = "#e5e7eb",
                     font = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif"),
    "journal" = list(pri = "#eb6864", bg = "#ffffff", card = "#ffffff",
                     txt = "#222222", muted = "#777777", bd = "#dddddd",
                     font = "Georgia, 'Times New Roman', serif"),
    "darkly"  = list(pri = "#375a7f", bg = "#222222", card = "#2d2d2d",
                     txt = "#e9ecef", muted = "#adb5bd", bd = "#444444",
                     font = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif"),
    "minty"   = list(pri = "#78c2ad", bg = "#f8f9fa", card = "#ffffff",
                     txt = "#343a40", muted = "#6c757d", bd = "#dfe6e9",
                     font = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif"),
    "lux"     = list(pri = "#b99511", bg = "#ffffff", card = "#ffffff",
                     txt = "#1a1a1a", muted = "#6c757d", bd = "#e0d5b7",
                     font = "Georgia, 'Times New Roman', serif"),
    list(pri = "#2563eb", bg = "#ffffff", card = "#ffffff",
         txt = "#0f172a", muted = "#64748b", bd = "#e2e8f0",
         font = "system-ui, sans-serif")
  )
}

# ==============================================================================
# Conversion image -> base64
# ==============================================================================

rboard_plot_base64 <- function(plot, width = 6, height = 3.2, dpi = 140) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch({
    ggplot2::ggsave(tmp, plot = plot, width = width, height = height, dpi = dpi)
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(tmp)) return(NULL)
  if (!requireNamespace("knitr", quietly = TRUE)) return(NULL)
  tryCatch(knitr::image_uri(tmp), error = function(e) NULL)
}

# ==============================================================================
# Rendu HTML d'un element
# ==============================================================================

rboard_html_tableau <- function(elem, kpi, hauteur_unites = 2L) {
  nom <- elem$nom_affiche %||% kpi$nom
  com <- elem$commentaire %||% kpi$commentaire
  regles <- if (!is.null(kpi$regles_couleur)) kpi$regles_couleur else list()
  tab <- kpi$tableau

  if (is.null(tab) || nrow(tab) == 0) {
    return(sprintf("<div class='kpi-tableau'><div class='tt'>%s</div><p>Aucune donnee</p></div>",
                   htmltools::htmlEscape(nom)))
  }

  n_col <- ncol(tab)
  n_row <- nrow(tab)

  h_int <- as.integer(hauteur_unites)
  base <- if (h_int >= 4L) 0.76
          else if (h_int >= 3L) 0.82
          else 0.92

  fsize <- max(0.55, base - 0.03 * n_row)
  if (h_int >= 3L) fsize <- max(0.55, fsize - 0.02)

  th_cells <- paste(sapply(names(tab), function(c)
    sprintf("<th>%s</th>", htmltools::htmlEscape(c))), collapse = "")

  body_rows <- character(n_row)
  for (i in seq_len(n_row)) {
    tds <- character(n_col)
    est_total <- tolower(as.character(tab[i, 1])) %in% c("total", "totaux")
    for (j in seq_len(n_col)) {
      val <- as.character(tab[i, j])
      bg <- ""; fg <- ""
      if (j > 1 && !est_total && length(regles) > 0) {
        v_num <- suppressWarnings(as.numeric(tab[i, j]))
        if (!is.na(v_num)) {
          couleur <- NULL
          for (r in regles) if (isTRUE(appliquer_regle(v_num, r))) { couleur <- r$couleur; break }
          if (!is.null(couleur)) {
            pal <- rboard_couleurs_pastel(couleur)
            bg <- sprintf("background:%s;", pal$bg)
            fg <- sprintf("color:%s;font-weight:600;", pal$fg)
          }
        }
      }
      align <- if (j == 1) "left" else "right"
      weight <- if (est_total) "700" else "500"
      tds[j] <- sprintf("<td style='text-align:%s;%s%sfont-weight:%s;'>%s</td>",
                        align, bg, fg, weight, htmltools::htmlEscape(val))
    }
    body_rows[i] <- sprintf("<tr>%s</tr>", paste(tds, collapse = ""))
  }

  annexes <- kpi$stats_annexes
  bloc_annexes <- ""
  if (!is.null(annexes) && length(annexes) > 0) {
    parts <- character(0)
    if (!is.null(annexes$p_value))
      parts <- c(parts, sprintf("<span><strong>p :</strong> %s</span>",
                                formater_valeur(annexes$p_value, "decimal_3")))
    if (!is.null(annexes$v_cramer))
      parts <- c(parts, sprintf("<span><strong>V :</strong> %s</span>",
                                formater_valeur(annexes$v_cramer, "decimal_2")))
    if (length(parts) > 0)
      bloc_annexes <- sprintf("<div class='annexes'>%s</div>", paste(parts, collapse = " - "))
  }

  com_html <- ""
  if (!is.null(com) && nzchar(com))
    com_html <- sprintf("<div class='com'>%s</div>", htmltools::htmlEscape(com))

  sprintf(paste0(
    "<div class='kpi-tableau'>",
    "<div class='tt'>%s</div>",
    "<div class='wrap'>",
    "<table style='font-size:%srem;'><thead><tr>%s</tr></thead><tbody>%s</tbody></table>",
    "</div>",
    "%s%s",
    "</div>"
  ), htmltools::htmlEscape(nom), round(fsize, 2), th_cells,
     paste(body_rows, collapse = ""), bloc_annexes, com_html)
}

rboard_html_kpi_card <- function(elem, kpi) {
  nom <- elem$nom_affiche %||% kpi$nom
  com <- elem$commentaire %||% kpi$commentaire
  couleur <- if (!is.null(elem$couleur_forcee) && nzchar(elem$couleur_forcee) &&
                 elem$couleur_forcee != "automatique") {
    elem$couleur_forcee
  } else kpi$couleur %||% "primary"
  bg <- rboard_couleur_kpi_bg(couleur)
  valeur <- formater_valeur(kpi$valeur, kpi$format)

  com_html <- ""
  if (!is.null(com) && nzchar(com))
    com_html <- sprintf("<div class='kc-com'>%s</div>", htmltools::htmlEscape(com))

  sprintf(paste0(
    "<div class='kpi-card' style='background:%s;'>",
    "<div class='kc-nom'>%s</div>",
    "<div class='kc-val'>%s</div>",
    "%s",
    "</div>"
  ), bg, htmltools::htmlEscape(nom), htmltools::htmlEscape(valeur), com_html)
}

#' Rendu HTML d'un graphique
#' @keywords internal
rboard_html_graphique <- function(elem, g, hauteur_unites = 3L) {
  nom <- elem$nom_affiche %||% g$nom
  com <- elem$commentaire

  plot_h_in <- switch(as.character(as.integer(hauteur_unites)),
    "1" = 1.5,
    "2" = 2.9,
    "3" = 4.3,
    "4" = 5.7,
    2.9)

  uri <- rboard_plot_base64(g$objet, width = 6, height = plot_h_in)
  img <- if (is.null(uri)) "<div class='ph'>Graphique indisponible</div>"
         else sprintf("<img src='%s' alt='graphique' />", uri)

  com_html <- ""
  if (!is.null(com) && nzchar(com))
    com_html <- sprintf("<div class='com'>%s</div>", htmltools::htmlEscape(com))

  sprintf(paste0(
    "<div class='graph'>",
    "<div class='tt'>%s</div>",
    "<div class='wrap'>%s</div>",
    "%s",
    "</div>"
  ), htmltools::htmlEscape(nom), img, com_html)
}

#' Rendu HTML d'un texte (libre ou bibliotheque)
#' @keywords internal
rboard_html_texte <- function(elem, projet = NULL) {
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

  html_md <- if (requireNamespace("commonmark", quietly = TRUE)) {
    tryCatch(commonmark::markdown_html(txt, extensions = TRUE, smart = FALSE, hardbreaks = TRUE),
             error = function(e) htmltools::htmlEscape(txt))
  } else {
    h <- htmltools::htmlEscape(txt)
    h <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", h, perl = TRUE)
    h <- gsub("\\*(.+?)\\*", "<em>\\1</em>", h, perl = TRUE)
    h
  }

  if (identical(stl, "titre")) {
    nt <- suppressWarnings(as.integer(elem$niveau_titre %||% 2L)[1])
    if (is.na(nt) || !(nt %in% c(1L, 2L, 3L))) nt <- 2L
    tag <- paste0("h", nt)
    return(sprintf("<div class='txt-titre txt-niv-%d'><%s>%s</%s></div>",
                   nt, tag, html_md, tag))
  }

  if (identical(stl, "note"))
    return(sprintf("<div class='txt-note'><span class='ico'>i</span><div>%s</div></div>", html_md))
  if (identical(stl, "avertissement"))
    return(sprintf("<div class='txt-avert'><span class='ico'>!</span><div>%s</div></div>", html_md))
  sprintf("<div class='txt-normal'>%s</div>", html_md)
}

#' Rendu HTML d'une ligne
#' @keywords internal
rboard_html_ligne <- function(ligne, projet) {
  if (is.null(ligne$elements) || length(ligne$elements) == 0) return("")

  els <- ligne$elements
  niveaux <- vapply(els, function(el) rboard_niveau_element(el, projet), integer(1))
  ordre <- order(-niveaux)
  els <- els[ordre]
  niveaux <- niveaux[ordre]

  html_els <- character(length(els))
  for (i in seq_along(els)) {
    el <- els[[i]]
    w_int <- as.integer(el$taille)
    if (is.na(w_int) || !(w_int %in% c(3L, 6L, 12L))) w_int <- 6L

    niveau <- as.integer(niveaux[i])
    if (!(niveau %in% c(1L, 2L, 3L, 4L))) niveau <- 1L

    type_el <- as.character(el$type)[1]
    contenu <- ""

    if (identical(type_el, "kpi")) {
      k <- projet$kpi[[el$id_kpi]]
      if (is.null(k)) {
        contenu <- "<div class='ph'>KPI introuvable</div>"
      } else if (identical(as.character(k$type)[1], "tableau")) {
        # Contraintes de largeur pour les tableaux
        if (isTRUE(el$tableau_long)) {
          if (!(w_int %in% c(6L, 12L))) w_int <- 12L
        } else if (isTRUE(el$tableau_etendu)) {
          w_int <- 6L
        } else if (!w_int %in% c(3L, 6L, 12L)) {
          w_int <- 6L
        }
        contenu <- rboard_html_tableau(el, k, hauteur_unites = niveau)
      } else {
        contenu <- rboard_html_kpi_card(el, k)
      }
    } else if (identical(type_el, "graphique")) {
      g <- projet$graphiques[[el$id_kpi]]
      if (is.null(g) || is.null(g$objet)) {
        contenu <- "<div class='ph'>Graphique introuvable</div>"
      } else {
        contenu <- rboard_html_graphique(el, g, hauteur_unites = niveau)
      }
    } else if (identical(type_el, "texte")) {
      contenu <- rboard_html_texte(el, projet = projet)
    }

    html_els[i] <- sprintf("<div class='el col-%d row-%d'>%s</div>",
                           w_int, niveau, contenu)
  }

  sprintf("<div class='row'>%s</div>", paste(html_els, collapse = ""))
}

# ==============================================================================
# CSS
# ==============================================================================

rboard_html_css <- function(pal) {
  paste0(
    "* { box-sizing: border-box; }\n",
    "html, body {\n",
    "  margin: 0; padding: 0;\n",
    "  background: ", pal$bg, ";\n",
    "  color: ", pal$txt, ";\n",
    "  font-family: ", pal$font, ";\n",
    "  font-size: 16px;\n",
    "  line-height: 1.5;\n",
    "}\n",
    ".container {\n",
    "  max-width: 1180px;\n",
    "  margin: 30px auto;\n",
    "  padding: 40px 44px;\n",
    "  background: ", pal$card, ";\n",
    "  border-radius: 12px;\n",
    "  box-shadow: 0 4px 24px rgba(0,0,0,0.08);\n",
    "}\n",
    "h1.rapport-titre {\n",
    "  font-size: 2rem;\n",
    "  font-weight: 700;\n",
    "  color: ", pal$pri, ";\n",
    "  margin: 0 0 14px 0;\n",
    "  padding-bottom: 14px;\n",
    "  border-bottom: 3px solid ", pal$pri, ";\n",
    "  line-height: 1.2;\n",
    "}\n",
    ".meta {\n",
    "  display: flex;\n",
    "  flex-wrap: wrap;\n",
    "  gap: 20px;\n",
    "  font-size: 0.88rem;\n",
    "  color: ", pal$muted, ";\n",
    "  margin-bottom: 24px;\n",
    "  padding-bottom: 20px;\n",
    "  border-bottom: 1px solid ", pal$bd, ";\n",
    "}\n",
    ".meta strong { color: ", pal$txt, "; font-weight: 600; }\n",
    ".meta .badge {\n",
    "  display: inline-block;\n",
    "  background: ", pal$pri, ";\n",
    "  color: white;\n",
    "  padding: 3px 10px;\n",
    "  border-radius: 12px;\n",
    "  font-size: 0.78rem;\n",
    "  font-weight: 600;\n",
    "}\n",
    ".description-bloc {\n",
    "  padding: 16px 20px;\n",
    "  background: ", pal$bg, ";\n",
    "  border-left: 4px solid ", pal$pri, ";\n",
    "  border-radius: 6px;\n",
    "  margin-bottom: 30px;\n",
    "  color: ", pal$txt, ";\n",
    "  font-size: 0.94rem;\n",
    "  line-height: 1.6;\n",
    "}\n",
    "h2.page-titre {\n",
    "  font-size: 1.5rem;\n",
    "  font-weight: 700;\n",
    "  color: ", pal$txt, ";\n",
    "  margin: 32px 0 18px 0;\n",
    "  padding: 10px 0 10px 16px;\n",
    "  border-left: 5px solid ", pal$pri, ";\n",
    "  background: ", pal$bg, ";\n",
    "  border-radius: 4px;\n",
    "}\n",
    ".row {\n",
    "  display: grid;\n",
    "  grid-template-columns: repeat(12, minmax(0, 1fr));\n",
    "  grid-auto-rows: 100px;\n",
    "  grid-auto-flow: row dense;\n",
    "  gap: 16px;\n",
    "  margin-bottom: 16px;\n",
    "}\n",
    ".el { min-width: 0; height: 100%; overflow: hidden; }\n",
    ".col-3 { grid-column: span 3; }\n",
    ".col-6 { grid-column: span 6; }\n",
    ".col-12 { grid-column: span 12; }\n",
    ".row-1 { grid-row: span 1; }\n",
    ".row-2 { grid-row: span 2; }\n",
    ".row-3 { grid-row: span 3; }\n",
    ".row-4 { grid-row: span 4; }\n",

    ".kpi-card {\n",
    "  height: 100%;\n",
    "  padding: 14px 16px;\n",
    "  border-radius: 10px;\n",
    "  color: white;\n",
    "  display: flex;\n",
    "  flex-direction: column;\n",
    "  justify-content: space-between;\n",
    "  overflow: hidden;\n",
    "  box-shadow: 0 2px 6px rgba(0,0,0,0.08);\n",
    "}\n",
    ".kc-nom {\n",
    "  font-size: 0.72rem;\n",
    "  font-weight: 600;\n",
    "  text-transform: uppercase;\n",
    "  letter-spacing: 0.05em;\n",
    "  opacity: 0.92;\n",
    "  white-space: nowrap;\n",
    "  overflow: hidden;\n",
    "  text-overflow: ellipsis;\n",
    "}\n",
    ".kc-val {\n",
    "  font-size: 2.2rem;\n",
    "  font-weight: 800;\n",
    "  line-height: 1.05;\n",
    "  white-space: nowrap;\n",
    "  overflow: hidden;\n",
    "  text-overflow: ellipsis;\n",
    "}\n",
    ".kc-com {\n",
    "  font-size: 0.7rem;\n",
    "  font-style: italic;\n",
    "  opacity: 0.85;\n",
    "  white-space: nowrap;\n",
    "  overflow: hidden;\n",
    "  text-overflow: ellipsis;\n",
    "  line-height: 1.2;\n",
    "}\n",

    ".kpi-tableau {\n",
    "  background: ", pal$card, ";\n",
    "  border: 1px solid ", pal$bd, ";\n",
    "  border-radius: 10px;\n",
    "  padding: 12px 14px;\n",
    "  height: 100%;\n",
    "  display: flex;\n",
    "  flex-direction: column;\n",
    "  box-shadow: 0 2px 6px rgba(0,0,0,0.05);\n",
    "  overflow: hidden;\n",
    "}\n",
    ".tt {\n",
    "  font-size: 0.88rem;\n",
    "  font-weight: 700;\n",
    "  color: ", pal$txt, ";\n",
    "  margin-bottom: 6px;\n",
    "  white-space: nowrap;\n",
    "  overflow: hidden;\n",
    "  text-overflow: ellipsis;\n",
    "  flex-shrink: 0;\n",
    "}\n",
    ".kpi-tableau .wrap {\n",
    "  flex: 1;\n",
    "  min-height: 0;\n",
    "  overflow: hidden;\n",
    "}\n",
    "table {\n",
    "  border-collapse: collapse;\n",
    "  width: 100%;\n",
    "  table-layout: fixed;\n",
    "}\n",
    "th, td {\n",
    "  border: 1px solid ", pal$bd, ";\n",
    "  padding: 3px 7px;\n",
    "  overflow: hidden;\n",
    "  text-overflow: ellipsis;\n",
    "  white-space: nowrap;\n",
    "}\n",
    "th {\n",
    "  background: ", pal$bg, ";\n",
    "  font-weight: 600;\n",
    "  color: ", pal$txt, ";\n",
    "  text-align: left;\n",
    "}\n",
    ".annexes {\n",
    "  font-size: 0.78rem;\n",
    "  color: ", pal$muted, ";\n",
    "  margin-top: 4px;\n",
    "  flex-shrink: 0;\n",
    "}\n",
    ".annexes span { margin-right: 12px; }\n",

    ".graph {\n",
    "  background: ", pal$card, ";\n",
    "  border: 1px solid ", pal$bd, ";\n",
    "  border-radius: 10px;\n",
    "  padding: 10px 12px;\n",
    "  height: 100%;\n",
    "  display: flex;\n",
    "  flex-direction: column;\n",
    "  box-shadow: 0 2px 6px rgba(0,0,0,0.05);\n",
    "  overflow: hidden;\n",
    "}\n",
    ".graph .wrap {\n",
    "  flex: 1;\n",
    "  min-height: 0;\n",
    "  display: flex;\n",
    "  align-items: center;\n",
    "  justify-content: center;\n",
    "  overflow: hidden;\n",
    "}\n",
    ".graph img {\n",
    "  max-width: 100%;\n",
    "  max-height: 100%;\n",
    "  object-fit: contain;\n",
    "  display: block;\n",
    "}\n",

    ".txt-normal, .txt-note, .txt-avert {\n",
    "  height: 100%;\n",
    "  padding: 12px 16px;\n",
    "  border-radius: 10px;\n",
    "  font-size: 0.9rem;\n",
    "  line-height: 1.45;\n",
    "  overflow: hidden;\n",
    "  display: flex;\n",
    "  flex-direction: column;\n",
    "  justify-content: center;\n",
    "  box-shadow: 0 2px 6px rgba(0,0,0,0.05);\n",
    "}\n",
    ".txt-normal {\n",
    "  background: ", pal$bg, ";\n",
    "  border: 1px solid ", pal$bd, ";\n",
    "  color: ", pal$txt, ";\n",
    "}\n",
    ".txt-note {\n",
    "  background: #ecfeff;\n",
    "  border-left: 4px solid #0891b2;\n",
    "  color: #155e75;\n",
    "  flex-direction: row;\n",
    "  align-items: center;\n",
    "  gap: 10px;\n",
    "}\n",
    ".txt-avert {\n",
    "  background: #fff7ed;\n",
    "  border-left: 4px solid #ea580c;\n",
    "  color: #7c2d12;\n",
    "  flex-direction: row;\n",
    "  align-items: center;\n",
    "  gap: 10px;\n",
    "}\n",
    ".txt-note .ico, .txt-avert .ico {\n",
    "  width: 22px; height: 22px;\n",
    "  display: inline-flex;\n",
    "  align-items: center;\n",
    "  justify-content: center;\n",
    "  border-radius: 50%;\n",
    "  font-weight: 700;\n",
    "  font-size: 0.9rem;\n",
    "  flex-shrink: 0;\n",
    "}\n",
    ".txt-note .ico { background: #0891b2; color: white; }\n",
    ".txt-avert .ico { background: #ea580c; color: white; }\n",

    ".txt-titre {\n",
    "  height: 100%;\n",
    "  padding: 14px 20px;\n",
    "  border-radius: 10px;\n",
    "  background: #eff6ff;\n",
    "  border-left: 5px solid #2563eb;\n",
    "  color: #1e40af;\n",
    "  display: flex;\n",
    "  flex-direction: column;\n",
    "  justify-content: center;\n",
    "  overflow: hidden;\n",
    "  box-shadow: 0 2px 6px rgba(0,0,0,0.05);\n",
    "}\n",
    ".txt-titre h1, .txt-titre h2, .txt-titre h3, .txt-titre p {\n",
    "  margin: 0;\n",
    "  color: inherit;\n",
    "  line-height: 1.15;\n",
    "  font-weight: 700;\n",
    "  overflow: hidden;\n",
    "  text-overflow: ellipsis;\n",
    "}\n",
    ".txt-niv-1 h1, .txt-niv-1 p { font-size: 2.1rem; }\n",
    ".txt-niv-2 h2, .txt-niv-2 p { font-size: 1.65rem; }\n",
    ".txt-niv-3 h3, .txt-niv-3 p { font-size: 1.3rem; }\n",

    ".txt-normal strong, .txt-note strong, .txt-avert strong, .txt-titre strong { font-weight: 700; }\n",
    ".txt-normal em, .txt-note em, .txt-avert em { font-style: italic; }\n",
    ".txt-normal u, .txt-note u, .txt-avert u { text-decoration: underline; }\n",
    ".txt-normal code, .txt-note code, .txt-avert code {\n",
    "  background: rgba(0,0,0,0.06);\n",
    "  padding: 1px 5px;\n",
    "  border-radius: 3px;\n",
    "  font-family: 'JetBrains Mono', monospace;\n",
    "  font-size: 0.9em;\n",
    "}\n",
    ".txt-normal ul, .txt-note ul, .txt-avert ul, .txt-titre ul {\n",
    "  margin: 4px 0; padding-left: 20px;\n",
    "}\n",
    ".txt-normal p, .txt-note p, .txt-avert p { margin: 0 0 4px 0; }\n",
    ".txt-normal p:last-child, .txt-note p:last-child, .txt-avert p:last-child { margin-bottom: 0; }\n",

    ".ph {\n",
    "  padding: 20px; text-align: center;\n",
    "  color: #dc2626; font-style: italic;\n",
    "  background: #fee2e2; border-radius: 8px;\n",
    "  height: 100%;\n",
    "  display: flex; align-items: center; justify-content: center;\n",
    "}\n",

    ".footer {\n",
    "  margin-top: 40px;\n",
    "  padding-top: 20px;\n",
    "  border-top: 1px solid ", pal$bd, ";\n",
    "  font-size: 0.82rem;\n",
    "  color: ", pal$muted, ";\n",
    "  text-align: center;\n",
    "}\n",

    "@media print {\n",
    "  body { background: white; }\n",
    "  .container { box-shadow: none; margin: 0; padding: 20px; max-width: 100%; }\n",
    "  .page-break { page-break-after: always; }\n",
    "}\n"
  )
}

# ==============================================================================
# Exporter HTML
# ==============================================================================

#' Exporter le dashboard en HTML autonome
#' @export
exporter_html <- function(projet, chemin, titre = NULL, pages_a_exporter = NULL,
                          theme = "flatly", auteur = NULL, institution = NULL,
                          description = NULL, inclure_date = TRUE) {

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

  pal <- rboard_theme_css_vars(theme)
  css <- rboard_html_css(pal)

  meta_parts <- character(0)
  if (!is.null(auteur) && nzchar(trimws(auteur)))
    meta_parts <- c(meta_parts, sprintf("<span><strong>Auteur :</strong> %s</span>",
                                        htmltools::htmlEscape(trimws(auteur))))
  if (!is.null(institution) && nzchar(trimws(institution)))
    meta_parts <- c(meta_parts, sprintf("<span><strong>Institution :</strong> %s</span>",
                                        htmltools::htmlEscape(trimws(institution))))
  if (isTRUE(inclure_date))
    meta_parts <- c(meta_parts, sprintf("<span><strong>Date :</strong> %s</span>",
                                        format(Sys.time(), "%d/%m/%Y a %H:%M")))

  meta_html <- if (length(meta_parts) > 0)
    sprintf("<div class='meta'>%s</div>", paste(meta_parts, collapse = ""))
  else ""

  desc_html <- ""
  if (!is.null(description) && nzchar(trimws(description)))
    desc_html <- sprintf("<div class='description-bloc'>%s</div>",
                         htmltools::htmlEscape(trimws(description)))

  pages_html <- character(0)
  for (i in seq_along(pages)) {
    page <- pages[[i]]
    rows_html <- if (is.null(page$lignes) || length(page$lignes) == 0) {
      "<p style='color:#94a3b8;font-style:italic;'>Page vide</p>"
    } else {
      paste(sapply(page$lignes, function(l) rboard_html_ligne(l, projet)), collapse = "")
    }
    badge_num <- sprintf("<span class='badge'>Page %d / %d</span>", i, length(pages))
    pages_html <- c(pages_html, sprintf(paste0(
      "<section class='page'>",
      "<h2 class='page-titre'>%s %s</h2>",
      "%s",
      "</section>"
    ), htmltools::htmlEscape(page$titre), badge_num, rows_html))
  }

  footer_html <- sprintf(
    "<div class='footer'>Genere par <strong>Rboard</strong> le %s</div>",
    format(Sys.time(), "%d/%m/%Y a %H:%M"))

  html_final <- paste0(
    "<!DOCTYPE html>\n<html lang='fr'>\n<head>\n",
    "<meta charset='UTF-8'>\n",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>\n",
    "<title>", htmltools::htmlEscape(titre_final), "</title>\n",
    "<style>\n", css, "\n</style>\n",
    "</head>\n<body>\n",
    "<div class='container'>\n",
    "<h1 class='rapport-titre'>", htmltools::htmlEscape(titre_final), "</h1>\n",
    meta_html, "\n",
    desc_html, "\n",
    paste(pages_html, collapse = "\n"),
    footer_html,
    "</div>\n</body>\n</html>"
  )

  writeLines(html_final, chemin, useBytes = TRUE)
  invisible(chemin)
}

# ==============================================================================
# Word
# ==============================================================================

exporter_word <- function(projet, chemin, titre = NULL, pages_a_exporter = NULL) {
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

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, titre_final, style = "heading 1")
  doc <- officer::body_add_par(doc,
    sprintf("Date de generation : %s", format(Sys.time(), "%d/%m/%Y a %H:%M")),
    style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")

  tmp_dir <- tempfile("rboard_docx_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  for (page in pages) {
    doc <- officer::body_add_par(doc, page$titre, style = "heading 2")
    if (is.null(page$lignes) || length(page$lignes) == 0) next
    for (ligne in page$lignes) {
      if (is.null(ligne$elements) || length(ligne$elements) == 0) next
      for (el in ligne$elements) {
        type_el <- as.character(el$type)[1]
        if (identical(type_el, "kpi")) {
          k <- projet$kpi[[el$id_kpi]]
          if (is.null(k)) {
            doc <- officer::body_add_par(doc, "[KPI introuvable]", style = "Normal")
          } else if (identical(as.character(k$type)[1], "tableau")) {
            doc <- officer::body_add_par(doc, el$nom_affiche %||% k$nom, style = "heading 3")
            ft <- flextable::flextable(k$tableau)
            ft <- flextable::autofit(ft)
            doc <- flextable::body_add_flextable(doc, ft)
          } else {
            nom <- el$nom_affiche %||% k$nom
            doc <- officer::body_add_par(doc, sprintf("%s : %s", nom, k$label), style = "heading 3")
            com <- el$commentaire %||% k$commentaire
            if (!is.null(com) && nzchar(com))
              doc <- officer::body_add_par(doc, com, style = "Normal")
          }
        } else if (identical(type_el, "graphique")) {
          g <- projet$graphiques[[el$id_kpi]]
          if (!is.null(g) && !is.null(g$objet)) {
            img <- file.path(tmp_dir, paste0("g_", el$id, ".png"))
            niveau <- rboard_niveau_element(el, projet)
            h_img <- switch(as.character(as.integer(niveau)),
              "1" = 2.5, "2" = 3.5, "3" = 4.8, "4" = 6.0, 3.8)
            ok <- tryCatch({
              ggplot2::ggsave(img, plot = g$objet, width = 6, height = h_img, dpi = 150)
              TRUE
            }, error = function(e) FALSE)
            if (ok) doc <- officer::body_add_img(doc, src = img, width = 6, height = h_img)
          }
        } else if (identical(type_el, "texte")) {
          id_texte <- if (!is.null(el$id_texte)) as.character(el$id_texte)[1] else NULL
          if (!is.null(id_texte) && nzchar(id_texte) &&
              !is.null(projet$textes) && !is.null(projet$textes[[id_texte]])) {
            txt <- as.character(projet$textes[[id_texte]]$contenu %||% "")[1]
          } else {
            txt <- as.character(el$contenu %||% "")[1]
          }
          txt_clean <- gsub("\\*\\*(.+?)\\*\\*", "\\1", txt, perl = TRUE)
          txt_clean <- gsub("\\*(.+?)\\*", "\\1", txt_clean, perl = TRUE)
          txt_clean <- gsub("^#+\\s+", "", txt_clean, perl = TRUE)
          doc <- officer::body_add_par(doc, txt_clean, style = "Normal")
        }
      }
    }
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }
  print(doc, target = chemin)
  invisible(chemin)
}