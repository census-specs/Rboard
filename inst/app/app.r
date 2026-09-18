library(shiny)

ui <- bslib::page_navbar(
  title = logo_rboard(),
  window_title = "Rboard",
  theme = theme_rboard(),
  id = "onglets_principaux",
  selected = "Projet - Vue d'ensemble",

  bslib::nav_menu(
    title = "Projet",
    icon = icone_menu("projet"),
    bslib::nav_panel("Vue d'ensemble", value = "Projet - Vue d'ensemble",
      icon = bsicons::bs_icon("info-circle"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_projet_vue"),
        mod_projet_vue_ui("projet_vue"))),
    bslib::nav_panel("Sauvegarder",
      icon = bsicons::bs_icon("save"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_projet_save"),
        mod_projet_save_ui("projet_save"))),
    bslib::nav_panel("Charger",
      icon = bsicons::bs_icon("folder2-open"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_projet_load"),
        mod_projet_load_ui("projet_load"))),
    bslib::nav_panel("Nouveau projet",
      icon = bsicons::bs_icon("file-earmark-plus"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_projet_new"),
        mod_projet_new_ui("projet_new")))
  ),

  bslib::nav_panel(
    title = "Import",
    icon = icone_menu("import"),
    tags$div(style = "padding: 28px 36px;",
      mod_bandeau_ui("bandeau_import"),
      mod_import_ui("import"))),

  # NOUVEAU : onglet Donnees
  bslib::nav_panel(
    title = "Donnees",
    icon = bsicons::bs_icon("database"),
    tags$div(style = "padding: 28px 36px;",
      mod_bandeau_ui("bandeau_donnees"),
      mod_donnees_ui("donnees"))),

  bslib::nav_menu(
    title = "KPI",
    icon = icone_menu("kpi"),
    bslib::nav_panel("Stats descriptives",
      icon = bsicons::bs_icon("bar-chart-fill"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_kpi_desc"),
        mod_kpi_desc_ui("kpi_desc"))),
    bslib::nav_panel("Comparaison de groupes",
      icon = bsicons::bs_icon("distribute-horizontal"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_kpi_compa"),
        mod_kpi_compa_ui("kpi_compa"))),
    bslib::nav_panel("Association / correlation",
      icon = bsicons::bs_icon("link-45deg"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_kpi_assoc"),
        mod_kpi_assoc_ui("kpi_assoc"))),
    bslib::nav_panel("Tableaux croises",
      icon = bsicons::bs_icon("grid-3x3"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_kpi_tableaux"),
        mod_kpi_tableaux_ui("kpi_tableaux"))),
    bslib::nav_panel("Statistiques sur 1 variable",
      icon = bsicons::bs_icon("file-bar-graph"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_kpi_une_var"),
        mod_kpi_une_var_ui("kpi_une_var"))),
    bslib::nav_panel("Saisie manuelle",
      icon = bsicons::bs_icon("pencil-square"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_kpi_manuel"),
        mod_kpi_manuel_ui("kpi_manuel"))),
    bslib::nav_panel("Liste des KPI",
      icon = bsicons::bs_icon("grid-fill"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_kpi_liste"),
        mod_kpi_liste_ui("kpi_liste"))),
    bslib::nav_panel("Bibliotheque de textes",
      icon = bsicons::bs_icon("journal-text"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_texte_liste"),
        mod_texte_liste_ui("texte_liste")))
  ),

  bslib::nav_menu(
    title = "Graphiques",
    icon = icone_menu("graphiques"),
    bslib::nav_panel("Creer un graphique",
      icon = bsicons::bs_icon("plus-circle"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_graph_creer"),
        mod_graphique_creer_ui("graphique_creer"))),
    bslib::nav_panel("Ma bibliotheque",
      icon = bsicons::bs_icon("grid-fill"),
      tags$div(style = "padding: 28px 36px;",
        mod_bandeau_ui("bandeau_graph_biblio"),
        mod_graphique_biblio_ui("graphique_biblio")))
  ),

  bslib::nav_panel(
    title = "Script R",
    icon = icone_menu("script"),
    tags$div(style = "padding: 28px 36px;",
      mod_bandeau_ui("bandeau_script"),
      mod_script_ui("script"))),

  bslib::nav_panel(
    title = "Export",
    icon = icone_menu("export"),
    tags$div(style = "padding: 28px 36px;",
      mod_bandeau_ui("bandeau_export"),
      mod_export_ui("export"))),

  bslib::nav_panel(
    title = "Dashboard",
    icon = icone_menu("dashboard"),
    tags$div(
      style = "padding: 28px 36px;",
      mod_bandeau_ui("bandeau_dashboard"),
      uiOutput("bandeau_format_a4"),
      mod_dash_page_ui("dash_page"),
      tags$div(style = "height: 24px;"),
      uiOutput("zone_lignes")
    )
  )
)

server <- function(input, output, session) {

  projet_r          <- reactiveVal(nouveau_projet())
  page_courante_r   <- reactiveVal(NULL)
  present_visible_r <- reactiveVal(FALSE)
  element_edition_r <- reactiveVal(NULL)

  observe({
    p <- projet_r()
    if (is.null(p$dashboard) || is.null(p$dashboard$pages) || length(p$dashboard$pages) == 0) {
      p$dashboard <- creer_dashboard()
      p$dashboard <- ajouter_page(p$dashboard, creer_page("Page 1"))
      projet_r(p)
      page_courante_r(p$dashboard$pages[[1]]$id)
    } else {
      ids <- sapply(p$dashboard$pages, function(x) x$id)
      if (is.null(page_courante_r()) || !(page_courante_r() %in% ids)) {
        page_courante_r(p$dashboard$pages[[1]]$id)
      }
    }
  })

  mod_bandeau_server("bandeau_projet_vue",   projet_r)
  mod_bandeau_server("bandeau_projet_save",  projet_r)
  mod_bandeau_server("bandeau_projet_load",  projet_r)
  mod_bandeau_server("bandeau_projet_new",   projet_r)
  mod_bandeau_server("bandeau_import",       projet_r)
  mod_bandeau_server("bandeau_donnees",      projet_r)   # NOUVEAU
  mod_bandeau_server("bandeau_kpi_desc",     projet_r)
  mod_bandeau_server("bandeau_kpi_compa",    projet_r)
  mod_bandeau_server("bandeau_kpi_assoc",    projet_r)
  mod_bandeau_server("bandeau_kpi_tableaux", projet_r)
  mod_bandeau_server("bandeau_kpi_une_var",  projet_r)
  mod_bandeau_server("bandeau_kpi_manuel",   projet_r)
  mod_bandeau_server("bandeau_kpi_liste",    projet_r)
  mod_bandeau_server("bandeau_texte_liste",  projet_r)
  mod_bandeau_server("bandeau_graph_creer",  projet_r)
  mod_bandeau_server("bandeau_graph_biblio", projet_r)
  mod_bandeau_server("bandeau_script",       projet_r)
  mod_bandeau_server("bandeau_export",       projet_r)
  mod_bandeau_server("bandeau_dashboard",    projet_r)

  mod_projet_vue_server("projet_vue", projet_r)
  mod_projet_save_server("projet_save", projet_r)
  mod_projet_load_server("projet_load", projet_r)
  mod_projet_new_server("projet_new", projet_r)

  mod_import_server("import", projet_r)
  mod_donnees_server("donnees", projet_r)   # NOUVEAU

  mod_kpi_desc_server("kpi_desc", projet_r)
  mod_kpi_compa_server("kpi_compa", projet_r)
  mod_kpi_assoc_server("kpi_assoc", projet_r)
  mod_kpi_tableaux_server("kpi_tableaux", projet_r)
  mod_kpi_une_var_server("kpi_une_var", projet_r)
  mod_kpi_manuel_server("kpi_manuel", projet_r)
  mod_kpi_liste_server("kpi_liste", projet_r)
  mod_texte_liste_server("texte_liste", projet_r)

  mod_graphique_creer_server("graphique_creer", projet_r)
  mod_graphique_biblio_server("graphique_biblio", projet_r)

  mod_script_server("script", projet_r)
  mod_export_server("export", projet_r)

  mod_dash_page_server("dash_page", projet_r, page_courante_r, present_visible_r)
  mod_dash_present_server("present", projet_r, page_courante_r, present_visible_r)
  mod_dash_element_edit_server("element_edit", projet_r, element_edition_r)

  output$bandeau_format_a4 <- renderUI({
    p <- projet_r()
    id_page <- page_courante_r()
    if (is.null(id_page)) return(NULL)
    page <- obtenir_page(p$dashboard, id_page)
    if (is.null(page)) return(NULL)

    nb_lignes <- if (is.null(page$lignes)) 0L else length(page$lignes)

    hauteurs_lignes <- if (nb_lignes == 0L) integer(0) else {
      vapply(page$lignes, function(l) rboard_max_hauteurs(l$elements), integer(1))
    }

    h_utilisee <- as.integer(sum(hauteurs_lignes))
    h_max      <- as.integer(RBOARD_GRILLE_HAUTEUR)

    largeurs_lignes <- if (nb_lignes == 0L) integer(0) else {
      vapply(page$lignes, function(l) rboard_somme_largeurs(l$elements), integer(1))
    }
    l_utilisee <- if (length(largeurs_lignes) == 0L) 0L else max(largeurs_lignes)

    ratio <- if (h_max == 0L) 0 else min(1, h_utilisee / h_max)

    palette <- if (h_utilisee > h_max) {
      list(bg = "#fee2e2", border = "#fca5a5", texte = "#991b1b",
           icone = "exclamation-octagon-fill")
    } else if (ratio >= 1) {
      list(bg = "#dcfce7", border = "#86efac", texte = "#166534",
           icone = "check-circle-fill")
    } else if (ratio >= 0.75) {
      list(bg = "#fef9c3", border = "#fde047", texte = "#854d0e",
           icone = "exclamation-triangle-fill")
    } else {
      list(bg = "#eff6ff", border = "#bfdbfe", texte = "#1e40af",
           icone = "info-circle-fill")
    }

    segments <- if (length(hauteurs_lignes) == 0L) {
      list(tags$div(style = "flex: 1;"))
    } else {
      lapply(seq_along(hauteurs_lignes), function(i) {
        h <- hauteurs_lignes[i]
        if (h == 0L) return(NULL)
        couleur <- if (h >= 4L) "#be185d"
                   else if (h >= 3L) "#7c3aed"
                   else if (h == 2L) "#0891b2"
                   else "#2563eb"
        tags$div(
          title = sprintf("Ligne %d : %d unite(s)", i, h),
          style = sprintf("flex: %d 0 0; background: %s; height: 100%%; min-width: 4px;",
                          h, couleur))
      })
    }

    libre <- max(0L, h_max - h_utilisee)
    segment_libre <- if (libre > 0L) {
      list(tags$div(
        title = sprintf("%d unite(s) libre(s)", libre),
        style = sprintf("flex: %d 0 0; background: transparent; height: 100%%; min-width: 4px;",
                        libre)))
    } else NULL

    statut_texte <- if (h_utilisee > h_max) {
      sprintf("Depassement : %d / %d unites (%+d)",
              h_utilisee, h_max, h_utilisee - h_max)
    } else if (h_utilisee == h_max) {
      sprintf("Page pleine : %d / %d unites", h_utilisee, h_max)
    } else {
      sprintf("%d / %d unites utilisees - %d libre(s)",
              h_utilisee, h_max, libre)
    }

    badge_largeur <- tags$span(
      style = "font-size: 0.78rem; color: #64748b; font-weight: 500;",
      bsicons::bs_icon("arrows-angle-expand"),
      sprintf(" Largeur max ligne : %d / 12 col", l_utilisee))

    tags$div(
      style = sprintf(
        paste0("margin-bottom: 16px; padding: 12px 16px; ",
               "background: %s; border: 1px solid %s; border-radius: 10px; ",
               "color: %s;"),
        palette$bg, palette$border, palette$texte),

      tags$div(
        style = "display: flex; align-items: center; justify-content: space-between; gap: 12px; flex-wrap: wrap; margin-bottom: 8px;",
        tags$div(
          style = "display: flex; align-items: center; gap: 8px; font-size: 0.88rem; font-weight: 500;",
          bsicons::bs_icon(palette$icone),
          tags$span(statut_texte),
          tags$span(style = "color: #94a3b8;", "-"),
          tags$span(style = "font-size: 0.82rem; color: #64748b;",
                    sprintf("%d ligne(s)", as.integer(nb_lignes)))),
        badge_largeur),

      tags$div(
        style = paste0(
          "height: 14px; background: #ffffff; border: 1px solid #e2e8f0; ",
          "border-radius: 8px; overflow: hidden; display: flex; gap: 2px; padding: 2px; ",
          "box-sizing: border-box;"),
        tagList(segments, segment_libre)),

      if (length(hauteurs_lignes) >= 2L) {
        tags$div(
          style = "margin-top: 6px; font-size: 0.72rem; color: #64748b; display: flex; gap: 14px; flex-wrap: wrap;",
          tags$span(bsicons::bs_icon("square-fill", style = "color: #2563eb;"), " 1 unite"),
          tags$span(bsicons::bs_icon("square-fill", style = "color: #0891b2;"), " 2 unites"),
          tags$span(bsicons::bs_icon("square-fill", style = "color: #7c3aed;"), " 3 unites"),
          tags$span(bsicons::bs_icon("square-fill", style = "color: #be185d;"), " 4 unites (tableau etendu / graphique grand)"))
      }
    )
  })

  lignes_inscrites <- reactiveVal(character(0))

  observe({
    id_page <- page_courante_r()
    if (is.null(id_page)) return()

    p <- projet_r()
    page <- obtenir_page(p$dashboard, id_page)
    if (is.null(page) || is.null(page$lignes)) return()

    actuels <- sapply(page$lignes, function(l) l$id)
    deja <- lignes_inscrites()
    nouveaux <- setdiff(actuels, deja)

    for (lid in nouveaux) {
      local({
        my_lid <- lid
        my_id_page <- id_page
        mod_dash_ligne_server(
          id = paste0("ligne_", my_lid),
          projet_r = projet_r,
          id_page = my_id_page,
          id_ligne = my_lid,
          element_edition_r = element_edition_r
        )
      })
    }
    lignes_inscrites(union(deja, actuels))
  })

  output$zone_lignes <- renderUI({
    if (!is.null(element_edition_r())) {
      return(mod_dash_element_edit_ui("element_edit"))
    }

    id_page <- page_courante_r()
    if (is.null(id_page)) {
      return(rboard_etat_vide("grid-1x2", "Aucune page selectionnee"))
    }

    p <- projet_r()
    page <- obtenir_page(p$dashboard, id_page)
    if (is.null(page)) return(NULL)

    tagList(
      if (is.null(page$lignes) || length(page$lignes) == 0) {
        rboard_etat_vide(
          "layout-three-columns",
          "Aucune ligne",
          "Cliquez sur 'Ajouter une ligne' pour commencer."
        )
      } else {
        lapply(page$lignes, function(l) {
          mod_dash_ligne_ui(paste0("ligne_", l$id))
        })
      },
      tags$div(
        style = "margin-top: 16px;",
        actionButton(
          "btn_ajouter_ligne",
          "Ajouter une ligne",
          icon = bsicons::bs_icon("plus-lg"),
          class = "btn-outline-primary"
        )
      )
    )
  })

  observeEvent(input$btn_ajouter_ligne, {
    id_page <- page_courante_r()
    req(id_page)
    p <- projet_r()
    p$dashboard <- ajouter_ligne(p$dashboard, id_page)
    p$date_modification <- Sys.Date()
    projet_r(p)
  })
}

ui_final <- tagList(
  ui,
  mod_dash_present_ui("present")
)

shinyApp(ui = ui_final, server = server)