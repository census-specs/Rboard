#' ==============================================================================
#' Rboard - Module Graphiques (2 sections independantes)
#' ==============================================================================

rboard_or_g <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.character(a) && length(a) == 1 && is.na(a))) b else a
}

rboard_etape_graph <- function(numero, titre, icone = NULL) {
  tags$div(
    style = "display: flex; align-items: center; gap: 12px; margin-bottom: 16px;",
    tags$span(style = paste0("display: inline-flex; align-items: center; justify-content: center; ",
                              "width: 28px; height: 28px; border-radius: 50%; ",
                              "background: #2563eb; color: #ffffff; font-weight: 700; font-size: 0.9rem;"),
              numero),
    tags$div(style = "display: flex; align-items: center; gap: 6px;",
      if (!is.null(icone)) bsicons::bs_icon(icone, class = "text-primary"),
      tags$span(style = "font-weight: 600; font-size: 1.05rem; color: #0f172a;", titre)))
}

rboard_icone_type_graph <- function(type) {
  if (is.null(type) || !nzchar(type)) return(bsicons::bs_icon("graph-up"))
  t <- tolower(type)
  if (grepl("camembert", t)) return(bsicons::bs_icon("pie-chart-fill"))
  if (grepl("histogramme", t)) return(bsicons::bs_icon("bar-chart-steps"))
  if (grepl("boxplot", t)) return(bsicons::bs_icon("box"))
  if (grepl("violon", t)) return(bsicons::bs_icon("music-note-beamed"))
  if (grepl("barplot", t)) return(bsicons::bs_icon("bar-chart-fill"))
  if (grepl("nuage", t)) return(bsicons::bs_icon("scatter-chart"))
  if (grepl("densite", t)) return(bsicons::bs_icon("wave"))
  if (grepl("mosaique", t)) return(bsicons::bs_icon("grid-3x3"))
  if (grepl("heatmap", t)) return(bsicons::bs_icon("grid-3x3-gap"))
  if (grepl("aire", t)) return(bsicons::bs_icon("activity"))
  if (grepl("frise", t)) return(bsicons::bs_icon("calendar-event"))
  if (grepl("points", t)) return(bsicons::bs_icon("circle-fill"))
  if (grepl("ligne", t)) return(bsicons::bs_icon("graph-up"))
  bsicons::bs_icon("graph-up")
}

# ==============================================================================
# SECTION 1 : Creer un graphique
# ==============================================================================

mod_graphique_creer_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 1200px; margin: 0 auto;",
    uiOutput(ns("alerte")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      tags$div(
        bslib::card(
          bslib::card_body(
            style = "padding: 24px 26px;",
            rboard_etape_graph(1, "Variables", "diagram-3-fill"),
            selectInput(ns("var_x"), "Variable X (obligatoire) :", choices = NULL, width = "100%"),
            selectInput(ns("var_y"), "Variable Y (optionnelle) :", choices = NULL, width = "100%"),
            tags$p(style = "font-size: 0.85rem; color: #64748b; margin: 0;",
                   bsicons::bs_icon("info-circle"),
                   " Laissez Y vide pour un graphique a une seule variable."))),
        bslib::card(
          bslib::card_body(
            style = "padding: 24px 26px;",
            rboard_etape_graph(2, "Type de graphique", "graph-up"),
            uiOutput(ns("ui_type_graph")))),
        bslib::card(
          bslib::card_body(
            style = "padding: 24px 26px;",
            rboard_etape_graph(3, "Personnalisation", "sliders"),
            textInput(ns("titre"), "Titre du graphique (optionnel) :",
                      value = "", placeholder = "Ex: Distribution des ages", width = "100%"),
            textInput(ns("nom_biblio"), "Nom pour la bibliotheque (obligatoire) :",
                      value = "", placeholder = "Ex: Age selon le sexe", width = "100%"),
            selectInput(ns("palette"), "Palette de couleurs :",
                        choices = c("Defaut" = "defaut", "Brasserie (Set2)" = "brasserie",
                                    "Gris" = "gris", "Manuelle (Rboard)" = "manuel"),
                        selected = "brasserie", width = "100%"),
            tags$div(style = "margin-top: 14px;"),
            bslib::accordion(
              open = FALSE,
              bslib::accordion_panel(
                title = tags$span(bsicons::bs_icon("sliders"), " Options avancees"),
                value = "options_avancees",
                uiOutput(ns("ui_options_avancees")))))),
        tags$div(style = "margin-top: 24px; display: flex; gap: 10px;",
          actionButton(ns("btn_creer"), "Ajouter a ma bibliotheque",
                       icon = bsicons::bs_icon("check-lg"), class = "btn-primary",
                       style = "flex: 1; padding: 14px; font-size: 1.05rem; font-weight: 600;"),
          actionButton(ns("btn_voir_code"), "Voir le code",
                       icon = bsicons::bs_icon("code-slash"),
                       class = "btn-outline-secondary", style = "padding: 14px;"))),
      tags$div(
        style = "position: sticky; top: 20px;",
        tags$div(style = "font-weight: 600; color: #0f172a; margin-bottom: 12px; display: flex; align-items: center; gap: 8px;",
                 bsicons::bs_icon("eye-fill", class = "text-primary"), "Apercu en direct"),
        tags$div(style = "background: #ffffff; border: 1px solid #e2e8f0; border-radius: 10px; padding: 14px;",
                 plotOutput(ns("apercu_plot"), height = "520px")),
        tags$div(style = "margin-top: 16px; padding: 14px 16px; background: #f8fafc; border: 1px dashed #cbd5e1; border-radius: 10px; font-size: 0.85rem; color: #64748b;",
                 bsicons::bs_icon("info-circle"), " L'apercu se met a jour automatiquement."))))
}

mod_graphique_creer_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    message_creation_r <- reactiveVal(NULL)

    output$alerte <- renderUI({
      msg <- message_creation_r(); if (is.null(msg)) return(NULL)
      rboard_alerte(msg$type, msg$texte)
    })

    donnees_r <- reactive({
      p <- projet_r()
      if (is.null(p$donnees) || !is.data.frame(p$donnees) || nrow(p$donnees) == 0) return(NULL)
      p$donnees
    })

    observe({
      df <- donnees_r()
      if (is.null(df)) {
        updateSelectInput(session, "var_x", choices = c("(Aucune donnee)" = ""))
        updateSelectInput(session, "var_y", choices = c("(Aucune donnee)" = ""))
        return()
      }
      vars <- names(df)
      types <- sapply(vars, function(v) detecter_type_variable(df[[v]]))
      labels <- sprintf("%s (%s)", vars, types)
      choix <- setNames(vars, labels)
      choix_y <- c("(Aucune - graphique a 1 variable)" = "", choix)
      updateSelectInput(session, "var_x", choices = choix,
                        selected = if (length(vars) > 0) vars[1] else NULL)
      updateSelectInput(session, "var_y", choices = choix_y, selected = "")
    })

    types_suggeres_r <- reactive({
      df <- donnees_r()
      if (is.null(df)) return(c("Barplot (effectifs)"))
      if (is.null(input$var_x) || !nzchar(input$var_x)) return(c("Barplot (effectifs)"))
      if (!input$var_x %in% names(df)) return(c("Barplot (effectifs)"))
      tx <- detecter_type_variable(df[[input$var_x]])
      ty <- NULL
      if (!is.null(input$var_y) && nzchar(input$var_y) && input$var_y %in% names(df)) {
        ty <- detecter_type_variable(df[[input$var_y]])
      }
      suggerer_graphiques(tx, ty)
    })

    output$ui_type_graph <- renderUI({
      suggs <- types_suggeres_r()
      if (length(suggs) == 0) suggs <- c("Barplot (effectifs)")
      choices <- setNames(suggs, suggs)
      tagList(
        selectInput(ns("type_graph"), NULL, choices = choices, selected = suggs[1], width = "100%"),
        tags$div(style = "font-size: 0.85rem; color: #64748b; margin-top: 6px;",
                 bsicons::bs_icon("info-circle"),
                 sprintf(" %d type(s) suggere(s) selon vos variables.", length(suggs))))
    })

    output$ui_options_avancees <- renderUI({
      df <- donnees_r(); if (is.null(df)) return(NULL)
      vars <- names(df)
      tagList(
        fluidRow(
          column(6, textInput(ns("nom_x"), "Nom de l'axe X :", value = "", width = "100%")),
          column(6, textInput(ns("nom_y"), "Nom de l'axe Y :", value = "", width = "100%"))),
        checkboxInput(ns("afficher_libelles"), "Afficher les libelles des axes", value = TRUE),
        fluidRow(
          column(4, selectInput(ns("var_couleur"), "Couleur par :",
                                 choices = c("(Aucune)" = "", setNames(vars, vars)), width = "100%")),
          column(4, selectInput(ns("var_taille"), "Taille par :",
                                 choices = c("(Aucune)" = "", setNames(vars, vars)), width = "100%")),
          column(4, selectInput(ns("var_facette"), "Facette par :",
                                 choices = c("(Aucune)" = "", setNames(vars, vars)), width = "100%"))),
        uiOutput(ns("ui_stat_barplot")))
    })

    output$ui_stat_barplot <- renderUI({
      t <- input$type_graph
      if (is.null(t) || !grepl("Barplot", t)) return(NULL)
      if (is.null(input$var_y) || !nzchar(input$var_y)) return(NULL)
      if (grepl("effectifs|frequences|empile|groupe|100%", t)) return(NULL)
      selectInput(ns("stat_barplot"), "Statistique a calculer :",
                  choices = c("Moyenne" = "moyenne", "Mediane" = "mediane",
                              "Somme" = "somme", "Ecart-type" = "ecart_type",
                              "Effectif" = "effectif", "Minimum" = "minimum", "Maximum" = "maximum"),
                  width = "100%")
    })

    graphique_actuel_r <- reactive({
      df <- donnees_r(); if (is.null(df)) return(NULL)
      x <- input$var_x
      if (is.null(x) || !nzchar(x) || !x %in% names(df)) return(NULL)
      y <- input$var_y
      y <- if (!is.null(y) && nzchar(y) && y %in% names(df)) y else NULL
      type <- input$type_graph
      if (is.null(type) || !nzchar(type)) return(NULL)
      tryCatch({
        construire_graphique(
          donnees = df, x = x, y = y, type = type,
          couleur = rboard_or_g(input$var_couleur, NULL),
          taille = rboard_or_g(input$var_taille, NULL),
          facette = rboard_or_g(input$var_facette, NULL),
          titre = if (nzchar(rboard_or_g(input$titre, ""))) input$titre else NULL,
          nom_x = if (nzchar(rboard_or_g(input$nom_x, ""))) input$nom_x else NULL,
          nom_y = if (nzchar(rboard_or_g(input$nom_y, ""))) input$nom_y else NULL,
          afficher_libelles = isTRUE(input$afficher_libelles),
          palette = rboard_or_g(input$palette, "defaut"),
          stat_barplot = rboard_or_g(input$stat_barplot, "moyenne"))
      }, error = function(e) structure(list(erreur = conditionMessage(e)), class = "graphique_erreur"))
    })

    output$apercu_plot <- renderPlot({
      df <- donnees_r()
      if (is.null(df)) { graphics::plot.new(); graphics::text(0.5, 0.5, "Aucune donnee chargee", col = "#64748b"); return() }
      x <- input$var_x
      if (is.null(x) || !nzchar(x)) { graphics::plot.new(); graphics::text(0.5, 0.5, "Selectionnez la variable X", col = "#64748b"); return() }
      g <- graphique_actuel_r()
      if (is.null(g)) { graphics::plot.new(); graphics::text(0.5, 0.5, "Generation...", col = "#64748b"); return() }
      if (inherits(g, "graphique_erreur")) { graphics::plot.new(); graphics::text(0.5, 0.5, paste("Erreur :", g$erreur), col = "#dc2626"); return() }
      print(g)
    })

    observeEvent(input$btn_voir_code, {
      g <- graphique_actuel_r()
      if (is.null(g) || inherits(g, "graphique_erreur")) {
        showModal(modalDialog(title = "Code R", tags$p(style = "color: #64748b;", "Aucun graphique valide."), easyClose = TRUE)); return()
      }
      code <- attr(g, "code") %||% "# indisponible"
      showModal(modalDialog(
        title = tags$div(style = "display: flex; align-items: center; gap: 8px;",
          bsicons::bs_icon("code-slash", class = "text-primary"), " Code R du graphique"),
        size = "l", easyClose = TRUE,
        tags$pre(style = "background: #f1f5f9; border: 1px solid #cbd5e1; border-radius: 8px; padding: 18px; font-size: 0.85rem; overflow-x: auto; max-height: 500px;", code),
        footer = tagList(modalButton("Fermer"))))
    })

    observeEvent(input$btn_creer, {
      message_creation_r(NULL)
      nom <- trimws(rboard_or_g(input$nom_biblio, ""))
      if (!nzchar(nom)) { message_creation_r(list(type = "danger", texte = "Veuillez renseigner un nom.")); return() }
      g <- graphique_actuel_r()
      if (is.null(g) || inherits(g, "graphique_erreur")) {
        message_creation_r(list(type = "danger", texte = "Le graphique contient une erreur.")); return()
      }
      tryCatch({
        kpi_g <- creer_kpi_graphique(nom = nom, objet_ggplot = g, code = attr(g, "code"),
                                      commentaire = if (nzchar(rboard_or_g(input$titre, ""))) input$titre else NULL,
                                      type_graphique = input$type_graph)
        projet_r(ajouter_graphique(projet_r(), kpi_g))
        message_creation_r(list(type = "success",
          texte = sprintf("Graphique '%s' ajoute (ID : %s).", nom, kpi_g$id)))
      }, error = function(e) message_creation_r(list(type = "danger",
        texte = sprintf("Erreur : %s", conditionMessage(e)))))
    })
  })
}

# ==============================================================================
# SECTION 2 : Ma bibliotheque
# ==============================================================================

mod_graphique_biblio_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 1200px; margin: 0 auto;",
    uiOutput(ns("contenu")))
}

mod_graphique_biblio_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    message_liste_r <- reactiveVal(NULL)
    id_a_supprimer_r <- reactiveVal(NULL)
    graph_edition_r <- reactiveVal(NULL)

    # ======================================================================
    # Contenu : liste OU formulaire d'edition
    # ======================================================================
    output$contenu <- renderUI({
      g <- graph_edition_r()
      if (!is.null(g)) {
        return(ecran_edition_graphique(g))
      }

      tagList(
        uiOutput(ns("alerte")),
        bslib::card(
          style = "margin-bottom: 24px;",
          bslib::card_body(
            style = "padding: 18px 22px;",
            fluidRow(
              column(7, textInput(ns("filtre_recherche"), NULL,
                                   placeholder = "Rechercher par nom...", width = "100%")),
              column(5, selectInput(ns("filtre_type"), NULL,
                                     choices = c("Tous les types" = "tous",
                                                 "Boxplot" = "Boxplot", "Histogramme" = "Histogramme",
                                                 "Camembert" = "Camembert", "Barplot (effectifs)" = "Barplot (effectifs)",
                                                 "Nuage de points" = "Nuage de points", "Ligne" = "Ligne",
                                                 "Violon" = "Violon", "Mosaique" = "Mosaique",
                                                 "Autre" = "autre"), width = "100%"))))),
        uiOutput(ns("grille_cartes"))
      )
    })

    # ======================================================================
    # Ecran d'edition d'un graphique
    # ======================================================================
    ecran_edition_graphique <- function(g) {
      tg <- rboard_or_g(g$type_graphique, "Graphique")
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
                          " Modifier le graphique"),
                  tags$div(style = "font-size: 0.85rem; color: #64748b; margin-top: 4px;",
                           "ID : ", tags$code(g$id)))),
              tags$span(class = "badge bg-light text-dark border",
                        style = "font-size: 0.85rem; padding: 8px 12px;", tg)),

            textInput(ns("modal_nom"), "Nom :",
                      value = g$nom, width = "100%"),
            textAreaInput(ns("modal_commentaire"), "Commentaire :",
                          value = rboard_or_g(g$commentaire, ""),
                          rows = 4, width = "100%"),

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
        ),

        bslib::card(
          bslib::card_body(
            style = "padding: 16px;",
            tags$div(style = "font-weight: 600; color: #0f172a; margin-bottom: 12px;",
                     bsicons::bs_icon("eye-fill", class = "text-primary"),
                     " Apercu"),
            plotOutput(ns("apercu_edit_graph"), height = "400px")))
      )
    }

    output$apercu_edit_graph <- renderPlot({
      g <- graph_edition_r()
      if (is.null(g) || is.null(g$objet)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Apercu indisponible", col = "#94a3b8")
        return()
      }
      tryCatch(print(g$objet), error = function(e) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Erreur de rendu", col = "#dc2626")
      })
    })

    observeEvent(input$btn_retour,     { graph_edition_r(NULL) })
    observeEvent(input$btn_annuler_edit, { graph_edition_r(NULL) })

    # ======================================================================
    # Message
    # ======================================================================
    output$alerte <- renderUI({
      msg <- message_liste_r(); if (is.null(msg)) return(NULL)
      rboard_alerte(msg$type, msg$texte)
    })

    graphs_filtres_r <- reactive({
      p <- projet_r(); graphs <- p$graphiques
      if (is.null(graphs) || length(graphs) == 0) return(list())
      type_f <- rboard_or_g(input$filtre_type, "tous")
      rech <- tolower(trimws(rboard_or_g(input$filtre_recherche, "")))
      res <- list()
      for (id in names(graphs)) {
        g <- graphs[[id]]; if (is.null(g)) next
        tg <- rboard_or_g(g$type_graphique, "Graphique")
        ok_type <- (type_f == "tous") ||
                   (type_f == "autre" && !tg %in% c("Boxplot","Histogramme","Camembert",
                                                     "Barplot (effectifs)","Nuage de points",
                                                     "Ligne","Violon","Mosaique")) ||
                   identical(tg, type_f)
        ok_rech <- TRUE
        if (nzchar(rech)) {
          com <- rboard_or_g(g$commentaire, "")
          txt <- tolower(paste(g$nom, g$id, tg, com))
          ok_rech <- grepl(rech, txt, fixed = TRUE)
        }
        if (ok_type && ok_rech) res[[id]] <- g
      }
      res
    })

    output$grille_cartes <- renderUI({
      p <- projet_r()
      total <- if (!is.null(p$graphiques)) length(p$graphiques) else 0
      if (total == 0) {
        return(rboard_etat_vide("bar-chart", "Aucun graphique dans la bibliotheque",
          "Creez votre premier graphique dans 'Creer un graphique'."))
      }
      graphs <- graphs_filtres_r()
      if (length(graphs) == 0) return(rboard_etat_vide("search", "Aucun resultat", "Ajustez vos criteres."))

      for (g in graphs) {
        local({
          my_g <- g; my_id <- my_g$id
          output[[paste0("mini_plot_", my_id)]] <- renderPlot({
            tryCatch({
              if (is.null(my_g$objet)) { graphics::plot.new(); graphics::text(0.5, 0.5, "Indisponible", col = "#94a3b8") }
              else print(my_g$objet)
            }, error = function(e) { graphics::plot.new(); graphics::text(0.5, 0.5, "Erreur de rendu", col = "#dc2626") })
          })
        })
      }

      cartes <- lapply(graphs, function(g) {
        tg <- rboard_or_g(g$type_graphique, "Graphique")
        icone <- rboard_icone_type_graph(tg)

        onclick_modif <- sprintf(
          "Shiny.setInputValue('%s', {id: '%s', t: Date.now()}, {priority: 'event'});",
          ns("click_modifier"), gsub("'", "\\\\'", g$id))
        onclick_suppr <- sprintf(
          "Shiny.setInputValue('%s', {id: '%s', t: Date.now()}, {priority: 'event'});",
          ns("click_supprimer"), gsub("'", "\\\\'", g$id))

        bslib::card(
          style = "width: 100%; height: 100%; min-width: 0; box-sizing: border-box;",
          bslib::card_header(
            style = "display: flex; justify-content: space-between; align-items: center; gap: 10px; padding: 12px 16px; min-width: 0;",
            tags$div(style = "display: flex; align-items: center; gap: 8px; min-width: 0; flex: 1;",
              tags$code(style = "font-size: 0.82rem; font-weight: 700; background: #e2e8f0; color: #1e293b; padding: 2px 8px; border-radius: 5px; flex-shrink: 0;", g$id),
              tags$span(class = "badge bg-light text-dark border",
                        style = "display: inline-flex; align-items: center; gap: 4px; font-weight: 500; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 100%;",
                        icone, tg))),
          bslib::card_body(
            style = "padding: 12px 14px;",
            plotOutput(ns(paste0("mini_plot_", g$id)), height = "180px"),
            tags$h6(style = "font-weight: 600; font-size: 0.94rem; color: #0f172a; margin: 12px 0 6px 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
                    title = g$nom, g$nom),
            tags$p(style = "font-size: 0.8rem; color: #94a3b8; margin: 0;",
                   "Cree le ", format(g$date_creation, "%d/%m/%Y")),
            if (!is.null(g$commentaire) && nzchar(g$commentaire))
              tags$p(style = "font-size: 0.84rem; color: #64748b; font-style: italic; margin: 10px 0 0 0;", g$commentaire)),
          bslib::card_footer(
            style = "padding: 12px 16px; background: #ffffff; border-top: 1px solid #f1f5f9; display: flex; gap: 8px;",
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

      tags$div(style = "display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 18px; width: 100%;", cartes)
    })

    # ======================================================================
    # Clic Modifier (via JS)
    # ======================================================================
    observeEvent(input$click_modifier, {
      info <- input$click_modifier
      if (is.null(info) || is.null(info$id) || !nzchar(info$id)) return()

      p <- projet_r()
      g <- p$graphiques[[info$id]]
      if (is.null(g)) {
        message_liste_r(list(type = "warning", texte = "Graphique introuvable."))
        return()
      }

      message_liste_r(NULL)
      graph_edition_r(g)
    }, ignoreInit = TRUE)

    # ======================================================================
    # Clic Supprimer (via JS)
    # ======================================================================
    observeEvent(input$click_supprimer, {
      info <- input$click_supprimer
      if (is.null(info) || is.null(info$id) || !nzchar(info$id)) return()

      p <- projet_r()
      g <- p$graphiques[[info$id]]
      if (is.null(g)) return()

      id_a_supprimer_r(info$id)
      showModal(modalDialog(
        title = tags$div(style = "display: flex; align-items: center; gap: 8px; color: #dc2626;",
          bsicons::bs_icon("exclamation-triangle-fill"), " Confirmer la suppression"),
        tags$p(sprintf("Supprimer '%s' (ID : %s) ?", g$nom, g$id)),
        footer = tagList(modalButton("Annuler"),
          actionButton(ns("modal_btn_confirm_suppr"), "Supprimer",
                       class = "btn-danger", icon = bsicons::bs_icon("trash-fill"))),
        easyClose = TRUE))
    }, ignoreInit = TRUE)

    observeEvent(input$modal_btn_confirm_suppr, {
      id <- id_a_supprimer_r(); if (is.null(id)) return()
      projet_r(supprimer_graphique(projet_r(), id))
      removeModal(); id_a_supprimer_r(NULL)
      message_liste_r(list(type = "info", texte = sprintf("Graphique %s supprime.", id)))
    })

    # ======================================================================
    # Enregistrer
    # ======================================================================
    observeEvent(input$modal_btn_save, {
      g <- graph_edition_r(); if (is.null(g)) return()
      nom <- trimws(rboard_or_g(input$modal_nom, ""))
      com <- trimws(rboard_or_g(input$modal_commentaire, ""))
      if (nzchar(nom)) g$nom <- nom
      g$commentaire <- if (nzchar(com)) com else NULL
      p <- projet_r(); p$graphiques[[g$id]] <- g; projet_r(p)
      graph_edition_r(NULL)
      message_liste_r(list(type = "success", texte = sprintf("Graphique '%s' mis a jour.", g$nom)))
    })
  })
}

