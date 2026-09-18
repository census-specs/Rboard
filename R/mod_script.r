#' ==============================================================================
#' Rboard - Module Script R
#' ==============================================================================

rboard_table_resultats_script <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tags$div(style = "padding: 20px; text-align: center; color: #94a3b8; font-style: italic;",
                    "Aucun objet statistique exploitable detecte."))
  }
  td <- "border: 1px solid #e2e8f0; padding: 8px 12px; color: #334155; vertical-align: middle; font-size: 0.85rem;"
  tdn <- "border: 1px solid #e2e8f0; padding: 8px 12px; color: #334155; text-align: right; vertical-align: middle; font-size: 0.85rem; font-family: 'JetBrains Mono', monospace;"
  th <- "border: 1px solid #cbd5e1; padding: 8px 12px; background: #f1f5f9; font-weight: 600; color: #0f172a; text-align: left; white-space: nowrap; font-size: 0.85rem;"

  tags$div(style = "border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden;",
    tags$div(style = "max-height: 420px; overflow-y: auto; overflow-x: auto;",
      tags$table(style = "border-collapse: collapse; width: 100%; min-width: 720px;",
        tags$thead(tags$tr(
          tags$th(style = th, "Nom"), tags$th(style = th, "Classe"),
          tags$th(style = th, "Type"), tags$th(style = paste0(th, " text-align: right;"), "Valeur"),
          tags$th(style = th, "Label"))),
        tags$tbody(lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(style = td, df$nom[i]),
            tags$td(style = td, tags$code(style = "font-size: 0.8rem; background: #f1f5f9; padding: 2px 6px; border-radius: 3px;", df$classe[i])),
            tags$td(style = td, tags$span(class = "badge bg-light text-dark border", df$type_kpi[i])),
            tags$td(style = tdn, as.character(df$valeur[i])),
            tags$td(style = td, df$label[i]))
        })))))
}

rboard_table_script_tableaux <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tags$div(style = "padding: 16px; text-align: center; color: #94a3b8; font-style: italic; font-size: 0.88rem;",
                    "Aucun tableau detecte."))
  }
  td <- "border: 1px solid #e2e8f0; padding: 8px 12px; color: #334155; vertical-align: middle; font-size: 0.85rem;"
  th <- "border: 1px solid #cbd5e1; padding: 8px 12px; background: #f1f5f9; font-weight: 600; color: #0f172a; text-align: left; white-space: nowrap; font-size: 0.85rem;"
  tags$div(style = "border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden;",
    tags$table(style = "border-collapse: collapse; width: 100%;",
      tags$thead(tags$tr(
        tags$th(style = th, "Nom"),
        tags$th(style = th, "Dimensions"),
        tags$th(style = th, "Apercu"))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i) {
        tags$tr(
          tags$td(style = td, tags$code(style = "font-size: 0.8rem; background: #ecfdf5; color: #065f46; padding: 2px 6px; border-radius: 3px;", df$nom[i])),
          tags$td(style = td, sprintf("%d x %d", df$n_lignes[i], df$n_colonnes[i])),
          tags$td(style = td, df$apercu[i]))
      }))))
}

rboard_table_script_textes <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tags$div(style = "padding: 16px; text-align: center; color: #94a3b8; font-style: italic; font-size: 0.88rem;",
                    "Aucun texte detecte."))
  }
  td <- "border: 1px solid #e2e8f0; padding: 8px 12px; color: #334155; vertical-align: middle; font-size: 0.85rem;"
  th <- "border: 1px solid #cbd5e1; padding: 8px 12px; background: #f1f5f9; font-weight: 600; color: #0f172a; text-align: left; white-space: nowrap; font-size: 0.85rem;"
  tags$div(style = "border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden;",
    tags$table(style = "border-collapse: collapse; width: 100%;",
      tags$thead(tags$tr(
        tags$th(style = th, "Nom"),
        tags$th(style = th, "Taille"),
        tags$th(style = th, "Style suggere"),
        tags$th(style = th, "Apercu"))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i) {
        tags$tr(
          tags$td(style = td, tags$code(style = "font-size: 0.8rem; background: #eff6ff; color: #1e40af; padding: 2px 6px; border-radius: 3px;", df$nom[i])),
          tags$td(style = td, sprintf("%d car.", df$n_caracteres[i])),
          tags$td(style = td, tags$span(class = "badge bg-light text-dark border", df$style_suggere[i])),
          tags$td(style = paste0(td, " max-width: 400px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;"), df$apercu[i]))
      }))))
}

rboard_table_script_graphiques <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tags$div(style = "padding: 16px; text-align: center; color: #94a3b8; font-style: italic; font-size: 0.88rem;",
                    "Aucun graphique detecte."))
  }
  td <- "border: 1px solid #e2e8f0; padding: 8px 12px; color: #334155; vertical-align: middle; font-size: 0.85rem;"
  th <- "border: 1px solid #cbd5e1; padding: 8px 12px; background: #f1f5f9; font-weight: 600; color: #0f172a; text-align: left; white-space: nowrap; font-size: 0.85rem;"
  tags$div(style = "border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden;",
    tags$table(style = "border-collapse: collapse; width: 100%;",
      tags$thead(tags$tr(
        tags$th(style = th, "Nom"),
        tags$th(style = th, "Classe"),
        tags$th(style = th, "Couches"))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i) {
        tags$tr(
          tags$td(style = td, tags$code(style = "font-size: 0.8rem; background: #fff7ed; color: #9a3412; padding: 2px 6px; border-radius: 3px;", df$nom[i])),
          tags$td(style = td, df$classe[i]),
          tags$td(style = td, as.character(df$nb_couches[i])))
      }))))
}

#' Interface
#' @export
mod_script_ui <- function(id) {
  ns <- NS(id)
  tags$div(style = "max-width: 1000px; margin: 0 auto; padding: 8px 0;",
    uiOutput(ns("contenu")))
}

#' Serveur
#' @export
mod_script_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    chemin_r <- reactiveVal(NULL)
    resultat_r <- reactiveVal(NULL)
    detectes_r <- reactiveVal(NULL)
    message_exec_r <- reactiveVal(NULL)
    message_global_r <- reactiveVal(NULL)
    show_browser_r <- reactiveVal(FALSE)
    selected_r <- reactiveVal(NULL)

    mod_file_browser_server("browser", show_browser_r, selected_r,
                             mode = "open", filetypes = c("R", "r"))

    observeEvent(selected_r(), {
      p <- selected_r()
      if (!is.null(p) && nzchar(p)) { chemin_r(p); message_exec_r(NULL) }
      selected_r(NULL)
    })

    output$contenu <- renderUI({
      if (isTRUE(show_browser_r())) {
        return(mod_file_browser_ui(ns("browser")))
      }

      tagList(
        tags$div(class = "alert alert-info", style = "border-radius: 10px;",
          tags$div(style = "display: flex; align-items: flex-start; gap: 10px;",
            bsicons::bs_icon("info-circle-fill", class = "fs-5"),
            tags$div(
              tags$strong("Votre script peut acceder au jeu de donnees du projet"),
              tags$br(),
              "via la variable ", tags$code("donnees"), " (si l'option est cochee).",
              tags$br(),
              tags$span(style = "font-style: italic; color: #475569;",
                        "Exemple : res_t <- t.test(age ~ sexe, data = donnees)")))),

        bslib::card(
          bslib::card_body(
            style = "padding: 20px 24px;",
            tags$div(
              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 14px;",
              tags$span(style = "display: inline-flex; align-items: center; justify-content: center; width: 26px; height: 26px; border-radius: 50%; background: #2563eb; color: #ffffff; font-weight: 700; font-size: 0.85rem;", "1"),
              tags$span(style = "font-weight: 600; font-size: 1.05rem; color: #0f172a;", "Choisir et executer un script")),

            tags$div(style = "display: flex; gap: 12px; align-items: center; flex-wrap: wrap; margin-bottom: 12px;",
              actionButton(ns("btn_parcourir"), "Parcourir...",
                           icon = bsicons::bs_icon("folder-fill"), class = "btn-primary"),
              uiOutput(ns("nom_fichier"), inline = TRUE)),

            checkboxInput(ns("utiliser_donnees"),
                          "Utiliser les donnees du projet dans le script (variable 'donnees')",
                          value = TRUE),

            tags$div(style = "margin-top: 14px;",
              actionButton(ns("btn_executer"), "Executer le script",
                           icon = bsicons::bs_icon("play-fill"), class = "btn-primary")),

            uiOutput(ns("message_exec"), style = "margin-top: 14px;"))),

        uiOutput(ns("ui_etape_resultats")),
        uiOutput(ns("zone_message"))
      )
    })

    observeEvent(input$btn_parcourir, { show_browser_r(TRUE) })

    output$nom_fichier <- renderUI({
      c <- chemin_r()
      if (is.null(c)) return(tags$span(style = "color: #94a3b8; font-size: 0.85rem; font-style: italic;",
                                        "Aucun script selectionne"))
      tags$span(style = "color: #2563eb; font-size: 0.88rem; font-weight: 500;",
                bsicons::bs_icon("file-earmark-code-fill"), " ", basename(c))
    })

    observeEvent(input$btn_executer, {
      message_exec_r(NULL); message_global_r(NULL)
      chemin <- chemin_r()
      if (is.null(chemin) || !nzchar(chemin)) {
        message_exec_r(list(type = "danger", texte = "Veuillez selectionner un script.")); return()
      }
      p <- projet_r(); donnees_injectees <- NULL
      if (isTRUE(input$utiliser_donnees)) {
        if (is.null(p$donnees) || !is.data.frame(p$donnees)) {
          message_exec_r(list(type = "warning",
            texte = "Aucun jeu de donnees. Importez des donnees ou decochez l'option.")); return()
        }
        donnees_injectees <- p$donnees
      }
      tryCatch({
        res <- executer_script(chemin, donnees = donnees_injectees)
        resultat_r(res)

        detectes <- detecter_tous_objets(res$objets)
        detectes_r(detectes)

        n_kpi   <- nrow(detectes$kpis)
        n_tab   <- nrow(detectes$tableaux)
        n_txt   <- nrow(detectes$textes)
        n_graph <- nrow(detectes$graphiques)

        if (res$succes) {
          message_exec_r(list(type = "success", texte = sprintf(
            "Script execute. Objets detectes : %d KPI, %d tableau(x), %d texte(s), %d graphique(s).",
            n_kpi, n_tab, n_txt, n_graph)))
        } else {
          message_exec_r(list(type = "danger",
            texte = "Le script a rencontre une erreur (voir onglet 'Avertissements')."))
        }
      }, error = function(e) message_exec_r(list(type = "danger",
        texte = sprintf("Erreur critique : %s", conditionMessage(e)))))
    })

    output$message_exec <- renderUI({
      m <- message_exec_r(); if (is.null(m)) return(NULL)
      rboard_alerte(m$type, m$texte)
    })

    output$ui_etape_resultats <- renderUI({
      res <- resultat_r(); if (is.null(res)) return(NULL)

      bslib::card(
        style = "margin-top: 16px;",
        bslib::card_body(
          style = "padding: 20px 24px;",
          tags$div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 14px;",
            tags$span(style = "display: inline-flex; align-items: center; justify-content: center; width: 26px; height: 26px; border-radius: 50%; background: #2563eb; color: #ffffff; font-weight: 700; font-size: 0.85rem;", "2"),
            tags$span(style = "font-weight: 600; font-size: 1.05rem; color: #0f172a;", "Resultats d'execution")),

          bslib::navset_card_tab(
            bslib::nav_panel("Sortie", icon = bsicons::bs_icon("terminal"),
              tags$pre(style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 14px; max-height: 340px; overflow: auto; font-size: 0.85rem;",
                if (nzchar(trimws(res$sortie_texte))) res$sortie_texte else "(aucune sortie texte)")),

            bslib::nav_panel(tags$span("Avertissements ",
                if (length(res$warnings) + length(res$erreurs) > 0)
                  tags$span(class = "badge bg-warning", length(res$warnings) + length(res$erreurs))),
              icon = bsicons::bs_icon("exclamation-triangle"),
              tags$pre(style = "background: #fffbeb; border: 1px solid #fef3c7; border-radius: 8px; padding: 14px; max-height: 340px; overflow: auto; font-size: 0.85rem;",
                if (length(res$erreurs) == 0 && length(res$warnings) == 0) "Aucun avertissement."
                else paste(c(
                  if (length(res$erreurs) > 0) paste0("[ERREUR] ", res$erreurs) else character(0),
                  if (length(res$warnings) > 0) paste0("[AVERTISSEMENT] ", res$warnings) else character(0)
                ), collapse = "\n"))),

            bslib::nav_panel("KPI detectes", icon = bsicons::bs_icon("calculator-fill"),
              uiOutput(ns("table_kpi"))),

            bslib::nav_panel("Tableaux", icon = bsicons::bs_icon("table"),
              uiOutput(ns("table_tableaux"))),

            bslib::nav_panel("Textes", icon = bsicons::bs_icon("text-paragraph"),
              uiOutput(ns("table_textes"))),

            bslib::nav_panel("Graphiques", icon = bsicons::bs_icon("bar-chart-fill"),
              uiOutput(ns("table_graphiques")))),

          tags$hr(style = "margin: 24px 0 16px 0;"),

          tags$div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 14px;",
            tags$span(style = "display: inline-flex; align-items: center; justify-content: center; width: 26px; height: 26px; border-radius: 50%; background: #2563eb; color: #ffffff; font-weight: 700; font-size: 0.85rem;", "3"),
            tags$span(style = "font-weight: 600; font-size: 1.05rem; color: #0f172a;", "Ajouter a la bibliotheque")),

          uiOutput(ns("zone_ajout"))
        )
      )
    })

    output$table_kpi <- renderUI({
      det <- detectes_r()
      if (is.null(det)) return(NULL)
      rboard_table_resultats_script(det$kpis)
    })
    output$table_tableaux <- renderUI({
      det <- detectes_r()
      if (is.null(det)) return(NULL)
      rboard_table_script_tableaux(det$tableaux)
    })
    output$table_textes <- renderUI({
      det <- detectes_r()
      if (is.null(det)) return(NULL)
      rboard_table_script_textes(det$textes)
    })
    output$table_graphiques <- renderUI({
      det <- detectes_r()
      if (is.null(det)) return(NULL)
      rboard_table_script_graphiques(det$graphiques)
    })

    output$zone_ajout <- renderUI({
      det <- detectes_r()
      if (is.null(det)) return(NULL)

      n_kpi <- nrow(det$kpis); n_tab <- nrow(det$tableaux)
      n_txt <- nrow(det$textes); n_graph <- nrow(det$graphiques)
      total <- n_kpi + n_tab + n_txt + n_graph

      if (total == 0) {
        return(tags$div(style = "padding: 12px 16px; background: #f1f5f9; border-radius: 8px; color: #64748b; font-size: 0.9rem;",
                        bsicons::bs_icon("info-circle"), " Aucun objet exploitable detecte dans ce script."))
      }

      choix_kpi <- if (n_kpi > 0) {
        setNames(as.character(seq_len(n_kpi)),
                 paste0(det$kpis$nom, " | ", det$kpis$label))
      } else character(0)

      choix_tab <- if (n_tab > 0) {
        setNames(as.character(seq_len(n_tab)),
                 paste0(det$tableaux$nom, " | ", det$tableaux$apercu))
      } else character(0)

      choix_txt <- if (n_txt > 0) {
        setNames(as.character(seq_len(n_txt)),
                 paste0(det$textes$nom, " | ", det$textes$apercu))
      } else character(0)

      choix_gr <- if (n_graph > 0) {
        setNames(as.character(seq_len(n_graph)), det$graphiques$nom)
      } else character(0)

      tagList(
        tags$div(style = "padding: 12px 16px; background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 8px; margin-bottom: 14px; font-size: 0.88rem; color: #1e40af;",
          bsicons::bs_icon("info-circle-fill"),
          sprintf(" %d objet(s) detecte(s) au total.", total)),

        if (n_kpi > 0) {
          tags$div(style = "padding: 14px 16px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; margin-bottom: 12px;",
            tags$div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
              bsicons::bs_icon("calculator-fill", class = "text-primary"),
              tags$strong(sprintf("KPI (%d)", n_kpi)),
              tags$span(style = "color: #64748b; font-size: 0.85rem;", "cartes statistiques simples")),
            selectInput(ns("select_kpi"), NULL,
                        choices = choix_kpi, multiple = TRUE,
                        selectize = TRUE, width = "100%"),
            actionButton(ns("btn_ajouter_kpi"), "Ajouter les KPI selectionnes",
                         icon = bsicons::bs_icon("plus-circle-fill"),
                         class = "btn-sm btn-primary"))
        },

        if (n_tab > 0) {
          tags$div(style = "padding: 14px 16px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; margin-bottom: 12px;",
            tags$div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
              bsicons::bs_icon("table", class = "text-success"),
              tags$strong(sprintf("Tableaux (%d)", n_tab)),
              tags$span(style = "color: #64748b; font-size: 0.85rem;", "data.frame convertibles en KPI tableau")),
            selectInput(ns("select_tableau"), NULL,
                        choices = choix_tab, multiple = TRUE,
                        selectize = TRUE, width = "100%"),
            actionButton(ns("btn_ajouter_tableau"), "Ajouter les tableaux selectionnes",
                         icon = bsicons::bs_icon("plus-circle-fill"),
                         class = "btn-sm btn-success"))
        },

        if (n_txt > 0) {
          tags$div(style = "padding: 14px 16px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; margin-bottom: 12px;",
            tags$div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
              bsicons::bs_icon("text-paragraph", class = "text-info"),
              tags$strong(sprintf("Textes (%d)", n_txt)),
              tags$span(style = "color: #64748b; font-size: 0.85rem;", "chaines markdown ajoutables au dashboard")),
            selectInput(ns("select_texte"), NULL,
                        choices = choix_txt, multiple = TRUE,
                        selectize = TRUE, width = "100%"),
            actionButton(ns("btn_ajouter_texte"), "Ajouter les textes selectionnes",
                         icon = bsicons::bs_icon("plus-circle-fill"),
                         class = "btn-sm btn-info"))
        },

        if (n_graph > 0) {
          tags$div(style = "padding: 14px 16px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; margin-bottom: 12px;",
            tags$div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
              bsicons::bs_icon("bar-chart-fill", class = "text-warning"),
              tags$strong(sprintf("Graphiques (%d)", n_graph)),
              tags$span(style = "color: #64748b; font-size: 0.85rem;", "objets ggplot ajoutables a la bibliotheque")),
            selectInput(ns("select_graphique"), NULL,
                        choices = choix_gr, multiple = TRUE,
                        selectize = TRUE, width = "100%"),
            actionButton(ns("btn_ajouter_graphique"), "Ajouter les graphiques selectionnes",
                         icon = bsicons::bs_icon("plus-circle-fill"),
                         class = "btn-sm btn-warning"))
        },

        tags$div(style = "margin-top: 16px; padding-top: 16px; border-top: 2px solid #e2e8f0;",
          actionButton(ns("btn_ajouter_tout"), "Tout ajouter a la bibliotheque",
                       icon = bsicons::bs_icon("download"),
                       class = "btn-primary",
                       style = "padding: 10px 24px; font-weight: 600;"))
      )
    })

    # ==========================================================================
    # Actions d'ajout
    # ==========================================================================

    observeEvent(input$btn_ajouter_kpi, {
      sel <- input$select_kpi
      det <- detectes_r()
      if (is.null(det) || nrow(det$kpis) == 0) return()
      if (is.null(sel) || length(sel) == 0) {
        message_global_r(list(type = "warning", texte = "Selectionnez au moins un KPI.")); return()
      }
      idx <- as.numeric(sel); idx <- idx[!is.na(idx) & idx >= 1 & idx <= nrow(det$kpis)]
      if (length(idx) == 0) return()

      p <- projet_r(); nb <- 0
      for (i in idx) {
        r <- det$kpis[i, ]
        k <- tryCatch(creer_kpi_depuis_objet(nom_objet = r$nom, valeur = r$valeur,
            type = r$type_kpi, label = r$label, source = "script"),
          error = function(e) NULL)
        if (!is.null(k)) { p <- ajouter_kpi_au_projet(p, k); nb <- nb + 1 }
      }
      projet_r(p)
      message_global_r(list(type = "success", texte = sprintf("%d KPI ajoute(s).", nb)))
    })

    observeEvent(input$btn_ajouter_tableau, {
      sel <- input$select_tableau
      det <- detectes_r()
      res <- resultat_r()
      if (is.null(det) || nrow(det$tableaux) == 0 || is.null(res)) return()
      if (is.null(sel) || length(sel) == 0) {
        message_global_r(list(type = "warning", texte = "Selectionnez au moins un tableau.")); return()
      }
      idx <- as.numeric(sel); idx <- idx[!is.na(idx) & idx >= 1 & idx <= nrow(det$tableaux)]
      if (length(idx) == 0) return()

      p <- projet_r(); nb <- 0
      for (i in idx) {
        nom_tab <- det$tableaux$nom[i]
        obj <- res$objets[[nom_tab]]
        if (is.null(obj)) next
        tab <- if (is.data.frame(obj)) obj else as.data.frame(obj, stringsAsFactors = FALSE)
        p <- tryCatch(ajouter_tableau_au_projet(p, nom = nom_tab, tableau = tab),
                      error = function(e) { message_global_r(list(type = "danger",
                        texte = sprintf("Erreur tableau '%s' : %s", nom_tab, conditionMessage(e)))); p })
        nb <- nb + 1
      }
      projet_r(p)
      if (nb > 0) message_global_r(list(type = "success", texte = sprintf("%d tableau(x) ajoute(s).", nb)))
    })

    observeEvent(input$btn_ajouter_texte, {
      sel <- input$select_texte
      det <- detectes_r()
      res <- resultat_r()
      if (is.null(det) || nrow(det$textes) == 0 || is.null(res)) return()
      if (is.null(sel) || length(sel) == 0) {
        message_global_r(list(type = "warning", texte = "Selectionnez au moins un texte.")); return()
      }
      idx <- as.numeric(sel); idx <- idx[!is.na(idx) & idx >= 1 & idx <= nrow(det$textes)]
      if (length(idx) == 0) return()

      p <- projet_r(); nb <- 0
      for (i in idx) {
        nom_txt <- det$textes$nom[i]
        obj <- res$objets[[nom_txt]]
        if (is.null(obj) || !is.character(obj)) next
        style <- det$textes$style_suggere[i]
        p <- tryCatch(ajouter_texte_au_projet(p, nom = nom_txt, contenu = obj,
                        style = style),
                      error = function(e) { message_global_r(list(type = "danger",
                        texte = sprintf("Erreur texte '%s' : %s", nom_txt, conditionMessage(e)))); p })
        nb <- nb + 1
      }
      projet_r(p)
      if (nb > 0) message_global_r(list(type = "success", texte = sprintf("%d texte(s) ajoute(s).", nb)))
    })

    observeEvent(input$btn_ajouter_graphique, {
      sel <- input$select_graphique
      det <- detectes_r()
      res <- resultat_r()
      if (is.null(det) || nrow(det$graphiques) == 0 || is.null(res)) return()
      if (is.null(sel) || length(sel) == 0) {
        message_global_r(list(type = "warning", texte = "Selectionnez au moins un graphique.")); return()
      }
      idx <- as.numeric(sel); idx <- idx[!is.na(idx) & idx >= 1 & idx <= nrow(det$graphiques)]
      if (length(idx) == 0) return()

      p <- projet_r(); nb <- 0
      for (i in idx) {
        nom_g <- det$graphiques$nom[i]
        obj <- res$objets[[nom_g]]
        if (is.null(obj) || !inherits(obj, "ggplot")) next
        p <- tryCatch(ajouter_graphique_depuis_script(p, nom = nom_g, objet_ggplot = obj),
                      error = function(e) { message_global_r(list(type = "danger",
                        texte = sprintf("Erreur graphique '%s' : %s", nom_g, conditionMessage(e)))); p })
        nb <- nb + 1
      }
      projet_r(p)
      if (nb > 0) message_global_r(list(type = "success", texte = sprintf("%d graphique(s) ajoute(s).", nb)))
    })

    observeEvent(input$btn_ajouter_tout, {
      det <- detectes_r()
      res <- resultat_r()
      if (is.null(det) || is.null(res)) return()

      p <- projet_r()
      nk <- nt <- nx <- ng <- 0

      if (nrow(det$kpis) > 0) {
        for (i in seq_len(nrow(det$kpis))) {
          r <- det$kpis[i, ]
          k <- tryCatch(creer_kpi_depuis_objet(nom_objet = r$nom, valeur = r$valeur,
              type = r$type_kpi, label = r$label, source = "script"),
            error = function(e) NULL)
          if (!is.null(k)) { p <- ajouter_kpi_au_projet(p, k); nk <- nk + 1 }
        }
      }

      if (nrow(det$tableaux) > 0) {
        for (i in seq_len(nrow(det$tableaux))) {
          nom_tab <- det$tableaux$nom[i]
          obj <- res$objets[[nom_tab]]
          if (is.null(obj)) next
          tab <- if (is.data.frame(obj)) obj else as.data.frame(obj, stringsAsFactors = FALSE)
          p <- tryCatch(ajouter_tableau_au_projet(p, nom = nom_tab, tableau = tab),
                        error = function(e) p)
          nt <- nt + 1
        }
      }

      if (nrow(det$textes) > 0) {
        for (i in seq_len(nrow(det$textes))) {
          nom_txt <- det$textes$nom[i]
          obj <- res$objets[[nom_txt]]
          if (is.null(obj) || !is.character(obj)) next
          p <- tryCatch(ajouter_texte_au_projet(p, nom = nom_txt, contenu = obj,
                          style = det$textes$style_suggere[i]),
                        error = function(e) p)
          nx <- nx + 1
        }
      }

      if (nrow(det$graphiques) > 0) {
        for (i in seq_len(nrow(det$graphiques))) {
          nom_g <- det$graphiques$nom[i]
          obj <- res$objets[[nom_g]]
          if (is.null(obj) || !inherits(obj, "ggplot")) next
          p <- tryCatch(ajouter_graphique_depuis_script(p, nom = nom_g, objet_ggplot = obj),
                        error = function(e) p)
          ng <- ng + 1
        }
      }

      projet_r(p)
      message_global_r(list(type = "success",
        texte = sprintf("Ajout termine : %d KPI, %d tableau(x), %d texte(s), %d graphique(s).",
                        nk, nt, nx, ng)))
    })

    output$zone_message <- renderUI({
      m <- message_global_r(); if (is.null(m)) return(NULL)
      tags$div(style = "margin-top: 16px;", rboard_alerte(m$type, m$texte))
    })
  })
}