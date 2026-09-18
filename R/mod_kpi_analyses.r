#' ==============================================================================
#' Rboard - Modules d'analyse KPI (formes compactes, sans scroll)
#' ==============================================================================

# Style commun
KPI_CARD_STYLE <- "padding: 22px 26px;"

# ==============================================================================
# 1. STATISTIQUES DESCRIPTIVES
# ==============================================================================

mod_kpi_desc_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 900px; margin: 0 auto;",
    uiOutput(ns("alerte")),
    bslib::card(
      bslib::card_body(
        style = KPI_CARD_STYLE,
        selectInput(ns("variables"), "Variables numeriques a analyser :",
                    choices = NULL, multiple = TRUE, selectize = TRUE, width = "100%"),
        tags$div(style = "margin-top: 18px;",
          tags$label(style = "font-weight: 600; color: #334155; margin-bottom: 8px;",
                     "Statistiques :"),
          checkboxGroupInput(ns("stats"), NULL,
            choices = c("Moyenne", "Mediane", "Ecart-type", "Variance",
                        "Minimum", "Maximum", "Effectif", "Q1", "Q3"),
            selected = c("Moyenne", "Ecart-type", "Effectif"),
            inline = TRUE)),
        tags$div(style = "margin-top: 20px;",
          actionButton(ns("valider"), "Calculer et creer les KPI",
                       icon = bsicons::bs_icon("check-lg"), class = "btn-primary",
                       style = "padding: 10px 22px; font-weight: 600;"))))
  )
}

mod_kpi_desc_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    msg_r <- reactiveVal(NULL)

    observe({
      p <- projet_r()
      updateSelectInput(session, "variables",
                        choices = rboard_choix_variables(p$donnees, filtre = c("numerique")))
    })

    output$alerte <- renderUI({
      m <- msg_r(); if (is.null(m)) return(NULL); rboard_alerte(m$type, m$texte)
    })

    observeEvent(input$valider, {
      msg_r(NULL)
      p <- projet_r(); df <- p$donnees
      if (is.null(df)) { msg_r(list(type = "warning", texte = "Aucun jeu de donnees.")); return() }
      vars <- input$variables
      if (is.null(vars) || length(vars) == 0) { msg_r(list(type = "warning", texte = "Selectionnez au moins une variable.")); return() }
      stats_sel <- input$stats
      if (is.null(stats_sel) || length(stats_sel) == 0) { msg_r(list(type = "warning", texte = "Selectionnez au moins une statistique.")); return() }

      mapping <- list(
        "Moyenne" = list(cle = "moyenne", fmt = "decimal_2", type = "moyenne"),
        "Mediane" = list(cle = "mediane", fmt = "decimal_2", type = "mediane"),
        "Ecart-type" = list(cle = "ecart_type", fmt = "decimal_2", type = "ecart_type"),
        "Variance" = list(cle = "variance", fmt = "decimal_2", type = "generique"),
        "Minimum" = list(cle = "minimum", fmt = "decimal_2", type = "generique"),
        "Maximum" = list(cle = "maximum", fmt = "decimal_2", type = "generique"),
        "Effectif" = list(cle = "effectif", fmt = "entier", type = "effectif"),
        "Q1" = list(cle = "q1", fmt = "decimal_2", type = "generique"),
        "Q3" = list(cle = "q3", fmt = "decimal_2", type = "generique"))

      nb <- 0
      for (v in vars) {
        vec <- df[[v]]; vec <- vec[!is.na(vec)]
        for (s in stats_sel) {
          mp <- mapping[[s]]
          val <- switch(mp$cle,
            "moyenne" = mean(vec), "mediane" = stats::median(vec),
            "ecart_type" = stats::sd(vec), "variance" = stats::var(vec),
            "minimum" = min(vec), "maximum" = max(vec),
            "effectif" = length(vec),
            "q1" = as.numeric(stats::quantile(vec, 0.25)),
            "q3" = as.numeric(stats::quantile(vec, 0.75)), NA_real_)
          k <- tryCatch(creer_kpi(nom = sprintf("%s (%s)", s, v),
                                   valeur = val, type = mp$type, format = mp$fmt,
                                   source = "stat_desc"), error = function(e) NULL)
          if (!is.null(k)) { p <- ajouter_kpi(p, k); nb <- nb + 1 }
        }
      }
      projet_r(p)
      msg_r(list(type = "success", texte = sprintf("%d KPI cree(s).", nb)))
    })
  })
}

# ==============================================================================
# 2. COMPARAISON DE GROUPES
# ==============================================================================

mod_kpi_compa_ui <- function(id) {
  ns <- NS(id)
  tags$div(style = "max-width: 900px; margin: 0 auto;",
    uiOutput(ns("alerte")),
    bslib::card(
      bslib::card_body(
        style = KPI_CARD_STYLE,
        radioButtons(ns("test"), "Test :",
                     choices = c("Test t" = "test_t", "Wilcoxon" = "wilcoxon",
                                 "ANOVA" = "anova", "Kruskal-Wallis" = "kruskal"),
                     selected = "test_t", inline = TRUE),
        tags$div(style = "margin-top: 14px;"),
        fluidRow(
          column(6, selectInput(ns("var"), "Variable dependante :", choices = NULL, width = "100%")),
          column(6, selectInput(ns("groupe"), "Variable de groupe :", choices = NULL, width = "100%"))),
        tags$div(style = "margin-top: 12px;",
          checkboxGroupInput(ns("options"), NULL,
            choices = c("Moyennes par groupe" = "moyennes", "Taille d'effet" = "effet"),
            selected = c("moyennes", "effet"), inline = TRUE)),
        tags$div(style = "margin-top: 18px;",
          actionButton(ns("valider"), "Calculer et creer les KPI",
                       icon = bsicons::bs_icon("check-lg"), class = "btn-primary",
                       style = "padding: 10px 22px; font-weight: 600;"))))
  )
}

mod_kpi_compa_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    msg_r <- reactiveVal(NULL)

    observe({
      p <- projet_r()
      updateSelectInput(session, "var",
                        choices = rboard_choix_variables(p$donnees, filtre = c("numerique")))
      updateSelectInput(session, "groupe",
                        choices = rboard_choix_variables(p$donnees, filtre = c("facteur", "logique", "autre")))
    })

    output$alerte <- renderUI({
      m <- msg_r(); if (is.null(m)) return(NULL); rboard_alerte(m$type, m$texte)
    })

    observeEvent(input$valider, {
      msg_r(NULL)
      p <- projet_r(); df <- p$donnees
      if (is.null(df)) { msg_r(list(type = "warning", texte = "Aucun jeu de donnees.")); return() }
      v <- input$var; g <- input$groupe
      if (is.null(v) || !nzchar(v) || is.null(g) || !nzchar(g)) {
        msg_r(list(type = "warning", texte = "Selectionnez variable et groupe.")); return()
      }
      test <- input$test; nb <- 0
      tryCatch({
        if (test == "test_t") {
          kpis <- kpi_test_t(df, v, g)
          for (k in kpis) { p <- ajouter_kpi(p, k); nb <- nb + 1 }
        } else if (test == "wilcoxon") {
          mods <- unique(df[[g]][!is.na(df[[g]])])
          if (length(mods) != 2) stop("Wilcoxon necessite 2 modalites.", call. = FALSE)
          x1 <- df[df[[g]] == mods[1], v]; x1 <- x1[!is.na(x1)]
          x2 <- df[df[[g]] == mods[2], v]; x2 <- x2[!is.na(x2)]
          res <- suppressWarnings(stats::wilcox.test(x1, x2))
          p <- ajouter_kpi(p, creer_kpi(sprintf("Wilcoxon (%s par %s)", v, g),
                                         valeur = res$p.value, type = "p_value",
                                         format = "decimal_3", source = "wilcoxon"))
          nb <- 1
        } else if (test == "kruskal") {
          res <- stats::kruskal.test(df[[v]] ~ df[[g]])
          p <- ajouter_kpi(p, creer_kpi(sprintf("Kruskal-Wallis (%s par %s)", v, g),
                                         valeur = res$p.value, type = "p_value",
                                         format = "decimal_3", source = "kruskal"))
          nb <- 1
        } else if (test == "anova") {
          s <- summary(stats::aov(df[[v]] ~ df[[g]]))[[1]]
          p <- ajouter_kpi(p, creer_kpi(sprintf("ANOVA p-value (%s par %s)", v, g),
                                         valeur = s[["Pr(>F)"]][1], type = "p_value",
                                         format = "decimal_3", source = "anova"))
          p <- ajouter_kpi(p, creer_kpi(sprintf("ANOVA F (%s par %s)", v, g),
                                         valeur = s[["F value"]][1], type = "generique",
                                         format = "decimal_2", source = "anova"))
          nb <- 2
        }
        projet_r(p)
        msg_r(list(type = "success", texte = sprintf("%d KPI cree(s).", nb)))
      }, error = function(e) msg_r(list(type = "danger", texte = sprintf("Erreur : %s", conditionMessage(e)))))
    })
  })
}

# ==============================================================================
# 3. ASSOCIATION / CORRELATION
# ==============================================================================

mod_kpi_assoc_ui <- function(id) {
  ns <- NS(id)
  tags$div(style = "max-width: 900px; margin: 0 auto;",
    uiOutput(ns("alerte")),
    bslib::card(
      bslib::card_body(
        style = KPI_CARD_STYLE,
        radioButtons(ns("methode"), "Methode :",
                     choices = c("Pearson" = "pearson", "Spearman" = "spearman",
                                 "Chi-deux" = "chisq", "Fisher" = "fisher"),
                     selected = "pearson", inline = TRUE),
        tags$div(style = "margin-top: 14px;"),
        fluidRow(
          column(6, selectInput(ns("var1"), "Variable 1 :", choices = NULL, width = "100%")),
          column(6, selectInput(ns("var2"), "Variable 2 :", choices = NULL, width = "100%"))),
        tags$div(style = "margin-top: 18px;",
          actionButton(ns("valider"), "Calculer et creer les KPI",
                       icon = bsicons::bs_icon("check-lg"), class = "btn-primary",
                       style = "padding: 10px 22px; font-weight: 600;"))))
  )
}

mod_kpi_assoc_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    msg_r <- reactiveVal(NULL)

    observe({
      p <- projet_r()
      updateSelectInput(session, "var1", choices = rboard_choix_variables(p$donnees))
      updateSelectInput(session, "var2", choices = rboard_choix_variables(p$donnees))
    })

    output$alerte <- renderUI({
      m <- msg_r(); if (is.null(m)) return(NULL); rboard_alerte(m$type, m$texte)
    })

    observeEvent(input$valider, {
      msg_r(NULL)
      p <- projet_r(); df <- p$donnees
      if (is.null(df)) { msg_r(list(type = "warning", texte = "Aucun jeu de donnees.")); return() }
      v1 <- input$var1; v2 <- input$var2
      if (is.null(v1) || !nzchar(v1) || is.null(v2) || !nzchar(v2) || v1 == v2) {
        msg_r(list(type = "warning", texte = "Selectionnez 2 variables differentes.")); return()
      }
      methode <- input$methode; nb <- 0
      tryCatch({
        if (methode %in% c("pearson", "spearman")) {
          kpis <- kpi_correlation(df, v1, v2, methode)
          for (k in kpis) { p <- ajouter_kpi(p, k); nb <- nb + 1 }
        } else if (methode == "chisq") {
          kpis <- kpi_test_chisq(df, v1, v2)
          for (k in kpis) { p <- ajouter_kpi(p, k); nb <- nb + 1 }
        } else if (methode == "fisher") {
          tab <- table(df[[v1]], df[[v2]])
          res <- stats::fisher.test(tab)
          p <- ajouter_kpi(p, creer_kpi(sprintf("Fisher (%s x %s)", v1, v2),
                                         valeur = res$p.value, type = "p_value",
                                         format = "decimal_3", source = "fisher.test"))
          nb <- 1
        }
        projet_r(p)
        msg_r(list(type = "success", texte = sprintf("%d KPI cree(s).", nb)))
      }, error = function(e) msg_r(list(type = "danger", texte = sprintf("Erreur : %s", conditionMessage(e)))))
    })
  })
}

# ==============================================================================
# 4. TABLEAUX CROISES
# ==============================================================================

mod_kpi_tableaux_ui <- function(id) {
  ns <- NS(id)
  tags$div(style = "max-width: 900px; margin: 0 auto;",
    uiOutput(ns("alerte")),
    bslib::card(
      bslib::card_body(
        style = KPI_CARD_STYLE,
        fluidRow(
          column(6, selectInput(ns("ligne"), "Variable 1 (lignes) :", choices = NULL, width = "100%")),
          column(6, selectInput(ns("colonne"), "Variable 2 (colonnes) :", choices = NULL, width = "100%"))),
        tags$div(style = "margin-top: 14px;",
          radioButtons(ns("affichage"), "Affichage :",
                       choices = c("Effectifs" = "effectifs", "% total" = "pourcent_total",
                                   "% ligne" = "pourcent_ligne", "% colonne" = "pourcent_colonne"),
                       selected = "effectifs", inline = TRUE)),
        tags$div(style = "margin-top: 12px;",
          checkboxGroupInput(ns("options"), NULL,
            choices = c("Totaux" = "totaux", "p-value et V de Cramer" = "stats"),
            selected = c("totaux", "stats"), inline = TRUE)),
        tags$div(style = "margin-top: 18px;",
          actionButton(ns("valider"), "Calculer et creer le KPI",
                       icon = bsicons::bs_icon("check-lg"), class = "btn-primary",
                       style = "padding: 10px 22px; font-weight: 600;"))))
  )
}

mod_kpi_tableaux_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    msg_r <- reactiveVal(NULL)

    observe({
      p <- projet_r()
      choix <- rboard_choix_variables(p$donnees, filtre = c("facteur", "logique", "autre"))
      updateSelectInput(session, "ligne", choices = choix)
      updateSelectInput(session, "colonne", choices = choix)
    })

    output$alerte <- renderUI({
      m <- msg_r(); if (is.null(m)) return(NULL); rboard_alerte(m$type, m$texte)
    })

    observeEvent(input$valider, {
      msg_r(NULL)
      p <- projet_r(); df <- p$donnees
      if (is.null(df)) { msg_r(list(type = "warning", texte = "Aucun jeu de donnees.")); return() }
      v1 <- input$ligne; v2 <- input$colonne
      if (is.null(v1) || !nzchar(v1) || is.null(v2) || !nzchar(v2) || v1 == v2) {
        msg_r(list(type = "warning", texte = "Selectionnez 2 variables differentes.")); return()
      }
      tryCatch({
        k <- kpi_tableau_croise(df, v1, v2, input$affichage,
                                 totaux = "totaux" %in% input$options,
                                 stats_annexes = "stats" %in% input$options)
        projet_r(ajouter_kpi(p, k))
        msg_r(list(type = "success", texte = sprintf("Tableau cree (ID : %s).", k$id)))
      }, error = function(e) msg_r(list(type = "danger", texte = sprintf("Erreur : %s", conditionMessage(e)))))
    })
  })
}

# ==============================================================================
# 5. STATISTIQUES SUR UNE VARIABLE
# ==============================================================================

mod_kpi_une_var_ui <- function(id) {
  ns <- NS(id)
  tags$div(style = "max-width: 900px; margin: 0 auto;",
    uiOutput(ns("alerte")),
    bslib::card(
      bslib::card_body(
        style = KPI_CARD_STYLE,
        radioButtons(ns("analyse"), "Analyse :",
                     choices = c("Frequence d'une modalite" = "frequence",
                                 "Effectif d'une modalite" = "effectif_mod",
                                 "Normalite (Shapiro)" = "shapiro"),
                     selected = "frequence", inline = TRUE),
        tags$div(style = "margin-top: 14px;"),
        fluidRow(
          column(6, selectInput(ns("var"), "Variable :", choices = NULL, width = "100%")),
          column(6, uiOutput(ns("ui_modalite")))),
        tags$div(style = "margin-top: 18px;",
          actionButton(ns("valider"), "Calculer et creer le KPI",
                       icon = bsicons::bs_icon("check-lg"), class = "btn-primary",
                       style = "padding: 10px 22px; font-weight: 600;"))))
  )
}

mod_kpi_une_var_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    msg_r <- reactiveVal(NULL)

    observe({
      p <- projet_r()
      updateSelectInput(session, "var", choices = rboard_choix_variables(p$donnees))
    })

    output$ui_modalite <- renderUI({
      p <- projet_r(); v <- input$var
      if (is.null(p$donnees) || is.null(v) || !nzchar(v) || !v %in% names(p$donnees)) return(NULL)
      if (!input$analyse %in% c("frequence", "effectif_mod")) return(NULL)
      mods <- sort(unique(as.character(p$donnees[[v]][!is.na(p$donnees[[v]])])))
      selectInput(ns("modalite"), "Modalite :", choices = mods, width = "100%")
    })

    output$alerte <- renderUI({
      m <- msg_r(); if (is.null(m)) return(NULL); rboard_alerte(m$type, m$texte)
    })

    observeEvent(input$valider, {
      msg_r(NULL)
      p <- projet_r(); df <- p$donnees
      if (is.null(df)) { msg_r(list(type = "warning", texte = "Aucun jeu de donnees.")); return() }
      v <- input$var
      if (is.null(v) || !nzchar(v)) { msg_r(list(type = "warning", texte = "Selectionnez une variable.")); return() }
      a <- input$analyse
      tryCatch({
        if (a == "frequence") {
          k <- kpi_frequence(df, v, input$modalite); p <- ajouter_kpi(p, k)
        } else if (a == "effectif_mod") {
          n <- sum(df[[v]] == input$modalite, na.rm = TRUE)
          k <- creer_kpi(sprintf("Effectif (%s = %s)", v, input$modalite),
                         valeur = n, type = "effectif", format = "entier",
                         source = "effectif_modalite")
          p <- ajouter_kpi(p, k)
        } else if (a == "shapiro") {
          vec <- df[[v]]; vec <- vec[!is.na(vec)]
          if (length(vec) < 3 || length(vec) > 5000) stop("Shapiro : 3 a 5000 obs.", call. = FALSE)
          res <- stats::shapiro.test(vec)
          k <- creer_kpi(sprintf("Shapiro (%s)", v),
                         valeur = res$p.value, type = "p_value",
                         format = "decimal_3", source = "shapiro.test")
          p <- ajouter_kpi(p, k)
        }
        projet_r(p)
        msg_r(list(type = "success", texte = "KPI cree."))
      }, error = function(e) msg_r(list(type = "danger", texte = sprintf("Erreur : %s", conditionMessage(e)))))
    })
  })
}

# ==============================================================================
# 6. SAISIE MANUELLE
# ==============================================================================

mod_kpi_manuel_ui <- function(id) {
  ns <- NS(id)
  tags$div(style = "max-width: 900px; margin: 0 auto;",
    uiOutput(ns("alerte")),
    bslib::card(
      bslib::card_body(
        style = KPI_CARD_STYLE,
        fluidRow(
          column(6, textInput(ns("nom"), "Nom :", value = "", width = "100%")),
          column(6, textInput(ns("valeur"), "Valeur :", value = "", width = "100%"))),
        fluidRow(
          column(6, selectInput(ns("type"), "Type :",
                                 choices = c("generique", "p_value", "d_cohen",
                                             "moyenne", "mediane", "ecart_type",
                                             "effectif", "frequence", "correlation", "texte"),
                                 width = "100%")),
          column(6, selectInput(ns("couleur"), "Couleur forcee :",
                                 choices = c("Automatique" = "", rboard_choix_couleurs()),
                                 width = "100%"))),
        textInput(ns("label"), "Label (optionnel) :",
                  value = "", placeholder = "Auto si vide", width = "100%"),
        textAreaInput(ns("commentaire"), "Commentaire (optionnel) :",
                      rows = 2, width = "100%"),
        tags$div(style = "margin-top: 14px;",
          actionButton(ns("valider"), "Creer le KPI",
                       icon = bsicons::bs_icon("check-lg"), class = "btn-primary",
                       style = "padding: 10px 22px; font-weight: 600;"))))
  )
}

mod_kpi_manuel_server <- function(id, projet_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    msg_r <- reactiveVal(NULL)

    output$alerte <- renderUI({
      m <- msg_r(); if (is.null(m)) return(NULL); rboard_alerte(m$type, m$texte)
    })

    observeEvent(input$valider, {
      msg_r(NULL)
      nom <- trimws(input$nom %||% "")
      if (!nzchar(nom)) { msg_r(list(type = "warning", texte = "Le nom est obligatoire.")); return() }
      val_txt <- trimws(input$valeur %||% "")
      if (!nzchar(val_txt)) { msg_r(list(type = "warning", texte = "La valeur est obligatoire.")); return() }
      val <- suppressWarnings(as.numeric(val_txt))
      if (is.na(val)) val <- val_txt
      label <- trimws(input$label %||% "")
      com <- trimws(input$commentaire %||% "")
      coul <- input$couleur %||% ""
      tryCatch({
        k <- creer_kpi(nom = nom, valeur = val, type = input$type,
                        label = if (nzchar(label)) label else NULL,
                        commentaire = if (nzchar(com)) com else NULL,
                        source = "manuel",
                        couleur_manuelle = if (nzchar(coul)) coul else NULL)
        projet_r(ajouter_kpi(projet_r(), k))
        msg_r(list(type = "success", texte = sprintf("KPI '%s' cree (ID : %s).", nom, k$id)))
      }, error = function(e) msg_r(list(type = "danger", texte = sprintf("Erreur : %s", conditionMessage(e)))))
    })
  })
}