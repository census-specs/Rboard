#' ==============================================================================
#' Rboard - Theme, couleurs et icones
#' ==============================================================================

rboard_couleurs <- function() {
  list(
    primary = "#2563eb", primary_dark = "#1d4ed8", primary_light = "#eff6ff",
    bg = "#ffffff", bg_soft = "#f8fafc", bg_muted = "#f1f5f9",
    border = "#e2e8f0", border_strong = "#cbd5e1",
    texte = "#0f172a", texte_moyen = "#475569", texte_secondaire = "#64748b",
    texte_discret = "#94a3b8",
    success = "#16a34a", warning = "#ea580c", danger = "#dc2626",
    info = "#0891b2", dark = "#1e293b", light = "#f8fafc"
  )
}

rboard_couleurs_pastel <- function(cle) {
  switch(as.character(cle),
    "success"   = list(bg = "#dcfce7", fg = "#166534"),
    "danger"    = list(bg = "#fee2e2", fg = "#991b1b"),
    "warning"   = list(bg = "#ffedd5", fg = "#9a3412"),
    "primary"   = list(bg = "#dbeafe", fg = "#1e40af"),
    "info"      = list(bg = "#cffafe", fg = "#155e75"),
    "secondary" = list(bg = "#f1f5f9", fg = "#475569"),
    "dark"      = list(bg = "#1e293b", fg = "#ffffff"),
    "light"     = list(bg = "#f8fafc", fg = "#334155"),
    list(bg = "#f1f5f9", fg = "#334155"))
}

theme_rboard <- function() {
  theme <- bslib::bs_theme(
    version = 5,
    primary = "#2563eb", secondary = "#64748b",
    success = "#16a34a", info = "#0891b2",
    warning = "#ea580c", danger = "#dc2626",
    bg = "#ffffff", fg = "#0f172a",

    base_font = bslib::font_collection(
      "Corbel",
      "Segoe UI",
      "-apple-system",
      "Helvetica Neue",
      "sans-serif"
    ),
    heading_font = bslib::font_collection(
      "Corbel",
      "Segoe UI",
      "-apple-system",
      "Helvetica Neue",
      "sans-serif"
    ),
    code_font = bslib::font_collection(
      "'JetBrains Mono'",
      "Consolas",
      "Menlo",
      "DejaVu Sans Mono",
      "monospace"
    ),

    "border-radius" = "10px",
    "border-radius-sm" = "6px",
    "border-radius-lg" = "14px",
    "card-border-radius" = "10px",
    "card-cap-bg" = "#ffffff",
    "card-border-color" = "#e2e8f0",
    "btn-border-radius" = "6px",
    "input-border-radius" = "8px",

    "font-size-base" = "0.98rem",
    "line-height-base" = "1.65"
  )

  theme <- bslib::bs_add_rules(theme, "
    /* ============================================================
       Styles generaux
       ============================================================ */
    .card + .card, .bslib-card + .bslib-card { margin-top: 22px; }
    .card-body { padding: 22px 26px; }
    .form-group, .shiny-input-container { margin-bottom: 18px; }
    .shiny-input-container label { font-weight: 500; color: #334155; margin-bottom: 6px; }
    .btn { padding: 8px 16px; }
    .btn-sm { padding: 6px 12px; }
    h1, h2, h3, h4, h5 { margin-top: 0; }

    /* ============================================================
       Corrections z-index / position
       ============================================================ */
    .card, .card-body, .card-header, .card-footer, .bslib-card,
    .accordion, .accordion-body, .accordion-item,
    .tab-content, .tab-pane, .navset-card-tab, .navset-card-pill, .tabbable {
      overflow: visible !important;
    }
    .card, .card-body, .bslib-card, .tab-pane, .tab-content, .accordion-item {
      position: static !important;
      z-index: auto !important;
    }
    .tab-pane, .tab-pane.active, .tab-pane.fade {
      transform: none !important;
      transition: none !important;
    }
    .selectize-control { position: relative; z-index: auto; }
    .selectize-control .selectize-input { position: relative; z-index: auto; }
    .selectize-control.dropdown-active { z-index: 100000 !important; }
    .selectize-control.dropdown-active .selectize-input { z-index: 100001 !important; }
    .selectize-dropdown, .selectize-dropdown.form-control,
    .selectize-dropdown.single, .selectize-dropdown.multi {
      z-index: 100002 !important;
      position: absolute !important;
      box-shadow: 0 8px 24px rgba(15, 23, 42, 0.15) !important;
      border: 1px solid #cbd5e1 !important;
      border-radius: 8px !important;
    }
    select, .form-select { position: relative; z-index: auto; }
    .modal { z-index: 105000 !important; }
    .modal-backdrop { z-index: 104999 !important; }
    .popover { z-index: 106000 !important; }
    .tooltip { z-index: 107000 !important; }

    /* ============================================================
       POLICE MONOSPACE CIBLEE - JetBrains Mono
       ============================================================ */
    code, pre, kbd, samp {
      font-family: 'JetBrains Mono', 'Consolas', 'Menlo', 'DejaVu Sans Mono', monospace;
    }
    pre {
      font-size: 0.85rem;
      line-height: 1.55;
    }
    .kc-val {
      font-family: 'JetBrains Mono', 'Consolas', 'Menlo', monospace;
      font-variant-numeric: tabular-nums;
      letter-spacing: -0.02em;
    }
    .badge {
      font-variant-numeric: tabular-nums;
    }
    input[type='number'] {
      font-variant-numeric: tabular-nums;
    }
    table th, table td {
      font-variant-numeric: tabular-nums;
    }
    table td[style*='text-align: right'],
    table td[style*='text-align:right'] {
      font-family: 'JetBrains Mono', 'Consolas', 'Menlo', monospace;
      font-size: 0.92em;
    }
    .rboard-code-block, textarea.code {
      font-family: 'JetBrains Mono', 'Consolas', 'Menlo', monospace;
    }

    /* ============================================================
       DATATABLES - Style Rboard avec bordures completes
       ============================================================ */

    .rboard-dt-wrapper,
    .dataTables_wrapper {
      font-family: inherit;
    }

    .dataTables_wrapper .row:first-child {
      margin-bottom: 10px;
    }

    table.dataTable,
    table.dataTable.display,
    table.dataTable.stripe {
      border-collapse: collapse !important;
      border: 1px solid #cbd5e1 !important;
      border-radius: 6px;
      width: 100% !important;
      background: #ffffff;
      font-size: 0.85rem;
    }

    /* En-tetes */
    table.dataTable thead th {
      background: #f1f5f9 !important;
      color: #0f172a !important;
      font-weight: 600 !important;
      border: 1px solid #cbd5e1 !important;
      padding: 8px 12px !important;
      text-align: left;
      vertical-align: middle;
      white-space: nowrap;
      font-size: 0.85rem;
    }
    table.dataTable thead th.sorting,
    table.dataTable thead th.sorting_asc,
    table.dataTable thead th.sorting_desc {
      background: #f1f5f9 !important;
      cursor: pointer;
    }
    table.dataTable thead th.sorting:after,
    table.dataTable thead th.sorting_asc:after,
    table.dataTable thead th.sorting_desc:after {
      opacity: 0.5;
    }

    /* Lignes et cellules */
    table.dataTable tbody tr {
      background: #ffffff;
    }
    table.dataTable tbody tr:nth-child(even) {
      background: #fafbfc;
    }
    table.dataTable tbody tr:hover {
      background: #f1f5f9 !important;
    }
    table.dataTable tbody td {
      border: 1px solid #e2e8f0 !important;
      padding: 6px 12px !important;
      color: #334155 !important;
      vertical-align: middle;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      font-variant-numeric: tabular-nums;
    }
    table.dataTable tbody td.dt-right,
    table.dataTable thead th.dt-right {
      font-family: 'JetBrains Mono', 'Consolas', 'Menlo', monospace;
      text-align: right;
    }

    /* Filtres par colonne */
    table.dataTable thead tr.filters th {
      background: #f8fafc !important;
      border: 1px solid #cbd5e1 !important;
      padding: 4px 6px !important;
    }
    table.dataTable thead tr.filters th input {
      width: 100% !important;
      padding: 4px 6px !important;
      font-size: 0.8rem !important;
      border: 1px solid #cbd5e1 !important;
      border-radius: 4px !important;
      box-sizing: border-box;
      background: #ffffff;
    }
    table.dataTable thead tr.filters th input:focus {
      outline: none;
      border-color: #2563eb !important;
      box-shadow: 0 0 0 2px rgba(37, 99, 235, 0.15);
    }

    /* Pagination */
    .dataTables_wrapper .dataTables_paginate {
      margin-top: 12px;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      border: 1px solid #cbd5e1 !important;
      background: #ffffff !important;
      color: #334155 !important;
      border-radius: 6px !important;
      padding: 5px 10px !important;
      margin: 0 2px;
      font-size: 0.85rem;
      cursor: pointer;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
      background: #f1f5f9 !important;
      color: #0f172a !important;
      border-color: #94a3b8 !important;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current,
    .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
      background: #2563eb !important;
      color: #ffffff !important;
      border-color: #2563eb !important;
      font-weight: 600;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button.disabled,
    .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover {
      color: #cbd5e1 !important;
      background: #f8fafc !important;
      border-color: #e2e8f0 !important;
      cursor: not-allowed;
      opacity: 0.6;
    }

    /* Info lignes */
    .dataTables_wrapper .dataTables_info {
      color: #64748b !important;
      font-size: 0.85rem;
      padding-top: 12px;
    }

    /* Length menu */
    .dataTables_wrapper .dataTables_length select {
      border: 1px solid #cbd5e1 !important;
      border-radius: 6px !important;
      padding: 4px 8px !important;
      background: #ffffff;
      color: #334155;
      font-size: 0.85rem;
    }
    .dataTables_wrapper .dataTables_length label {
      color: #64748b;
      font-size: 0.85rem;
      font-weight: 500;
    }

    /* Search input */
    .dataTables_wrapper .dataTables_filter input {
      border: 1px solid #cbd5e1 !important;
      border-radius: 6px !important;
      padding: 6px 10px !important;
      font-size: 0.85rem;
      background: #ffffff;
      margin-left: 6px;
    }
    .dataTables_wrapper .dataTables_filter input:focus {
      outline: none;
      border-color: #2563eb !important;
      box-shadow: 0 0 0 2px rgba(37, 99, 235, 0.15);
    }
    .dataTables_wrapper .dataTables_filter label {
      color: #64748b;
      font-size: 0.85rem;
      font-weight: 500;
    }

    /* Scroller */
    .dataTables_scroll {
      border-radius: 6px;
      overflow: hidden;
    }
    .dataTables_scrollHead,
    .dataTables_scrollBody {
      border: none !important;
    }
    .dataTables_scrollHead table.dataTable {
      border-bottom: none !important;
      border-radius: 6px 6px 0 0 !important;
    }
    .dataTables_scrollBody table.dataTable {
      border-top: none !important;
      border-radius: 0 0 6px 6px !important;
    }

    /* Empty state */
    table.dataTable tbody td.dataTables_empty {
      text-align: center;
      color: #94a3b8;
      font-style: italic;
      padding: 30px !important;
    }
  ")

  theme
}

logo_rboard <- function() {
  shiny::tags$span(
    style = paste0(
      "display: inline-flex; align-items: center; gap: 8px; ",
      "font-size: 1.1rem; font-weight: 700; color: #2563eb; ",
      "line-height: 1; letter-spacing: -0.01em;"
    ),
    bsicons::bs_icon("palette", size = "1.15em"),
    shiny::tags$span("Rboard")
  )
}

icone_menu <- function(nom) {
  switch(tolower(as.character(nom)),
    "projet"      = bsicons::bs_icon("folder-fill"),
    "import"      = bsicons::bs_icon("upload"),
    "donnees"     = bsicons::bs_icon("database"),
    "kpi"         = bsicons::bs_icon("calculator-fill"),
    "graphiques"  = bsicons::bs_icon("bar-chart-fill"),
    "script"      = bsicons::bs_icon("code-slash"),
    "export"      = bsicons::bs_icon("download"),
    "dashboard"   = bsicons::bs_icon("grid-1x2-fill"),
    bsicons::bs_icon("circle"))
}

icone_type_variable <- function(type) {
  switch(tolower(as.character(type)),
    "numerique" = bsicons::bs_icon("123"),
    "facteur"   = bsicons::bs_icon("tag-fill"),
    "date"      = bsicons::bs_icon("calendar-event-fill"),
    "logique"   = bsicons::bs_icon("toggle-on"),
    "autre"     = bsicons::bs_icon("question-circle"),
    bsicons::bs_icon("question-circle"))
}

icone_type_graphique <- function(type) {
  switch(as.character(type),
    "Nuage de points"     = bsicons::bs_icon("scatter-chart"),
    "Ligne"               = bsicons::bs_icon("graph-up"),
    "Ligne (inverse)"     = bsicons::bs_icon("graph-up"),
    "Barplot"             = bsicons::bs_icon("bar-chart"),
    "Barplot (effectifs)" = bsicons::bs_icon("bar-chart-fill"),
    "Boxplot"             = bsicons::bs_icon("box"),
    "Boxplot (inverse)"   = bsicons::bs_icon("box"),
    "Violon"              = bsicons::bs_icon("music-note-beamed"),
    "Points"              = bsicons::bs_icon("circle-fill"),
    "Aire"                = bsicons::bs_icon("graph-up-arrow"),
    "Densite"             = bsicons::bs_icon("wave"),
    "Mosaique"            = bsicons::bs_icon("grid-3x3"),
    bsicons::bs_icon("graph-up"))
}

icone_type_kpi <- function(type) {
  switch(tolower(as.character(type)),
    "moyenne"     = bsicons::bs_icon("bar-chart-fill"),
    "mediane"     = bsicons::bs_icon("graph-up"),
    "ecart_type"  = bsicons::bs_icon("activity"),
    "effectif"    = bsicons::bs_icon("people-fill"),
    "frequence"   = bsicons::bs_icon("pie-chart-fill"),
    "p_value"     = bsicons::bs_icon("award-fill"),
    "d_cohen"     = bsicons::bs_icon("arrows-expand"),
    "correlation" = bsicons::bs_icon("link-45deg"),
    "tableau"     = bsicons::bs_icon("table"),
    "texte"       = bsicons::bs_icon("text-paragraph"),
    "graphique"   = bsicons::bs_icon("bar-chart-fill"),
    bsicons::bs_icon("bookmark-fill"))
}

rboard_alerte <- function(type = "info", message, icone = NULL) {
  if (is.null(icone)) {
    icone <- switch(type,
      "success" = "check-circle-fill", "warning" = "exclamation-triangle-fill",
      "danger"  = "x-circle-fill", "info" = "info-circle-fill",
      "info-circle-fill")
  }
  shiny::tags$div(
    class = paste0("alert alert-", type),
    style = "display: flex; align-items: center; gap: 12px; border-radius: 10px; margin-bottom: 20px; padding: 14px 18px;",
    bsicons::bs_icon(icone),
    shiny::tags$span(message)
  )
}

rboard_etat_vide <- function(icone = "inbox", titre, sous_titre = NULL) {
  shiny::tags$div(
    style = paste0(
      "text-align: center; padding: 56px 28px; ",
      "background: #f8fafc; border: 1px dashed #cbd5e1; border-radius: 12px; ",
      "color: #64748b;"
    ),
    shiny::tags$div(style = "font-size: 2.2rem; color: #94a3b8; margin-bottom: 14px;",
                    bsicons::bs_icon(icone)),
    shiny::tags$h5(style = "margin: 0 0 8px 0; color: #334155; font-weight: 600;", titre),
    if (!is.null(sous_titre)) shiny::tags$p(style = "margin: 0; font-size: 0.92rem;", sous_titre)
  )
}

#' Rendu HTML d'un tableau KPI : sans scroll, police auto-adaptee
#'
#' @export
rboard_tableau_html <- function(tableau, regles = list(),
                                 colonne_valeur_index = NULL,
                                 max_rows = NULL, compact = FALSE,
                                 font_base = 0.85) {
  if (is.null(tableau) || nrow(tableau) == 0) {
    return(tags$div(style = "color: #94a3b8; font-style: italic; padding: 12px;",
                    "Tableau vide"))
  }

  affichage <- tableau
  limite_atteinte <- FALSE
  if (!is.null(max_rows) && nrow(affichage) > max_rows) {
    affichage <- affichage[seq_len(max_rows), , drop = FALSE]
    limite_atteinte <- TRUE
  }

  n <- nrow(affichage)
  fb <- as.numeric(font_base)[1]
  if (is.na(fb) || fb <= 0) fb <- 0.85
  facteur <- max(0.55, min(1, 1.1 - 0.06 * n))
  fb_ajuste <- fb * facteur
  if (fb_ajuste < 0.45) fb_ajuste <- 0.45

  padding <- if (compact) "3px 6px" else "4px 8px"
  font_size <- paste0(round(fb_ajuste, 2), "rem")

  cell_base <- paste0("border: 1px solid #e2e8f0; padding: ", padding, "; ",
                      "vertical-align: middle; font-size: ", font_size, "; ",
                      "overflow: hidden; text-overflow: ellipsis; white-space: nowrap;")
  cell_head <- paste0("border: 1px solid #cbd5e1; padding: ", padding, "; ",
                      "background: #f1f5f9; font-weight: 600; color: #0f172a; ",
                      "text-align: left; font-size: ", font_size, "; ",
                      "overflow: hidden; text-overflow: ellipsis; white-space: nowrap;")

  entetes <- names(affichage)
  n_col <- ncol(affichage)
  col_width <- paste0(round(100 / n_col, 2), "%")

  header <- tags$thead(tags$tr(lapply(entetes, function(c) {
    tags$th(style = paste0(cell_head, " width: ", col_width, ";"), c)
  })))

  body <- tags$tbody(lapply(seq_len(n), function(i) {
    tags$tr(lapply(seq_len(n_col), function(j) {
      val <- affichage[i, j]
      est_coloree <- if (!is.null(colonne_valeur_index)) j == colonne_valeur_index else j > 1
      est_total <- tolower(as.character(affichage[i, 1])) %in% c("total", "totaux")

      if (est_coloree && !est_total && length(regles) > 0) {
        v_num <- suppressWarnings(as.numeric(val))
        if (!is.na(v_num)) {
          couleur <- NULL
          for (r in regles) {
            if (isTRUE(appliquer_regle(v_num, r))) { couleur <- r$couleur; break }
          }
          if (!is.null(couleur)) {
            pal <- rboard_couleurs_pastel(couleur)
            return(tags$td(
              style = paste0("border: 1px solid #e2e8f0; padding: ", padding, "; ",
                             "background: ", pal$bg, "; color: ", pal$fg, "; ",
                             "font-weight: 600; text-align: right; ",
                             "font-size: ", font_size, "; ",
                             "font-family: 'JetBrains Mono', 'Consolas', 'Menlo', monospace; ",
                             "font-variant-numeric: tabular-nums; ",
                             "overflow: hidden; text-overflow: ellipsis; white-space: nowrap;"),
              as.character(val)))
          }
        }
      }

      style_td <- if (j == 1) {
        paste0(cell_base, " text-align: left; font-weight: ", if (est_total) "700" else "500", ";")
      } else {
        paste0(cell_base, " text-align: right; ",
               "font-family: 'JetBrains Mono', 'Consolas', 'Menlo', monospace; ",
               "font-variant-numeric: tabular-nums;",
               if (est_total) " font-weight: 700; background: #f8fafc;" else "")
      }
      tags$td(style = style_td, as.character(val))
    }))
  }))

  tableau_html <- tags$div(
    style = "width: 100%;",
    tags$table(
      style = "border-collapse: collapse; width: 100%; table-layout: fixed;",
      header, body)
  )

  if (limite_atteinte) {
    reste <- nrow(tableau) - max_rows
    return(tagList(tableau_html,
      tags$div(style = "font-size: 0.72rem; color: #64748b; font-style: italic; margin-top: 4px;",
               sprintf("... %d ligne(s) supplementaire(s)", reste))))
  }

  tableau_html
}