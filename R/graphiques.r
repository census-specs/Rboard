# ==============================================================================
# Rboard - Moteur graphique ggplot2
# ==============================================================================

#' Suggerer les types de graphiques adaptes
#'
#' @param type_x Caractere. Type de la variable x.
#' @param type_y Caractere ou NULL. Type de la variable y (NULL si X seul).
#' @return Un vecteur de chaines de caracteres.
#' @export
suggerer_graphiques <- function(type_x, type_y = NULL) {
  tx <- if (identical(type_x, "logique")) "facteur" else type_x

  if (is.null(type_y) || !nzchar(type_y)) {
    return(switch(tx,
      "facteur"   = c("Barplot (effectifs)", "Barplot (frequences)", "Camembert"),
      "numerique" = c("Boxplot", "Histogramme"),
      "date"      = c("Barplot temporel"),
      c("Barplot (effectifs)")
    ))
  }

  ty <- if (identical(type_y, "logique")) "facteur" else type_y
  cle <- paste(tx, ty, sep = "_")
  switch(cle,
    "numerique_numerique" = c("Nuage de points", "Nuage + tendance", "Ligne", "Aire", "Densite 2D"),
    "facteur_numerique"   = c("Boxplot", "Violon", "Barplot", "Points + jitter", "Barplot avec erreurs"),
    "numerique_facteur"   = c("Boxplot horizontal", "Violon horizontal", "Densite par groupe", "Points + jitter horizontal"),
    "facteur_facteur"     = c("Barplot (effectifs)", "Barplot empile", "Barplot groupe", "Barplot 100% empile", "Mosaique", "Heatmap de contingence"),
    "date_numerique"      = c("Ligne", "Aire", "Points", "Barplot temporel"),
    "numerique_date"      = c("Ligne inversee", "Points inverses"),
    "facteur_date"        = c("Boxplot temporel", "Barplot temporel"),
    "date_facteur"        = c("Boxplot temporel inverse", "Barplot empile temporel"),
    "date_date"           = c("Nuage temporel", "Frise chronologique"),
    c("Barplot (effectifs)")
  )
}

#' Construire un mapping aes() en toute securite
#' @keywords internal
rboard_aes <- function(x = NULL, y = NULL, couleur = NULL, taille = NULL,
                        couleur_aes = "color", groupe = NULL) {
  parts <- character(0)
  if (!is.null(x) && nzchar(x)) parts <- c(parts, sprintf("x = .data[['%s']]", x))
  if (!is.null(y) && nzchar(y)) parts <- c(parts, sprintf("y = .data[['%s']]", y))
  if (!is.null(couleur) && nzchar(couleur))
    parts <- c(parts, sprintf("%s = .data[['%s']]", couleur_aes, couleur))
  if (!is.null(taille) && nzchar(taille))
    parts <- c(parts, sprintf("size = .data[['%s']]", taille))
  if (!is.null(groupe) && nzchar(groupe))
    parts <- c(parts, sprintf("group = .data[['%s']]", groupe))
  if (length(parts) == 0) return(ggplot2::aes())
  eval(parse(text = sprintf("ggplot2::aes(%s)", paste(parts, collapse = ", "))))
}

#' Genere le code aes() correspondant pour l'affichage
#' @keywords internal
rboard_aes_code <- function(x = NULL, y = NULL, couleur = NULL, taille = NULL,
                             couleur_aes = "color") {
  parts <- character(0)
  if (!is.null(x) && nzchar(x)) parts <- c(parts, sprintf("x = %s", x))
  if (!is.null(y) && nzchar(y)) parts <- c(parts, sprintf("y = %s", y))
  if (!is.null(couleur) && nzchar(couleur))
    parts <- c(parts, sprintf("%s = %s", couleur_aes, couleur))
  if (!is.null(taille) && nzchar(taille))
    parts <- c(parts, sprintf("size = %s", taille))
  sprintf("aes(%s)", paste(parts, collapse = ", "))
}

#' Construire un graphique ggplot2
#'
#' @param donnees Data.frame.
#' @param x Caractere. Variable X.
#' @param y Caractere ou NULL. Variable Y.
#' @param type Caractere. Type de graphique.
#' @param couleur Caractere ou NULL.
#' @param taille Caractere ou NULL.
#' @param facette Caractere ou NULL.
#' @param titre Caractere ou NULL.
#' @param nom_x Caractere ou NULL.
#' @param nom_y Caractere ou NULL.
#' @param afficher_libelles Logique.
#' @param palette Caractere.
#' @param stat_barplot Caractere.
#' @param frequence_mode Caractere.
#' @return Un objet ggplot2 avec attribut "code".
#' @export
construire_graphique <- function(donnees, x, y = NULL, type,
                                  couleur = NULL, taille = NULL, facette = NULL,
                                  titre = NULL, nom_x = NULL, nom_y = NULL,
                                  afficher_libelles = TRUE,
                                  palette = "defaut",
                                  stat_barplot = "moyenne",
                                  frequence_mode = "effectifs") {

  # --- Validation des entrees ---
  valider_colonne(donnees, x, "le graphique")
  y_null <- is.null(y) || !nzchar(y)
  if (!y_null) {
    valider_colonne(donnees, y, "le graphique")
    if (identical(x, y)) {
      stop("Les variables X et Y doivent etre differentes.", call. = FALSE)
    }
  }
  if (is.null(type) || !nzchar(type)) {
    stop("Aucun type de graphique specifie.", call. = FALSE)
  }

  # --- Normalisation ---
  if (y_null) y <- NULL
  if (is.null(couleur) || !nzchar(couleur)) couleur <- NULL
  if (is.null(taille) || !nzchar(taille)) taille <- NULL
  if (is.null(facette) || !nzchar(facette)) facette <- NULL

  type_fill <- c(
    "Aire", "Boxplot", "Boxplot horizontal", "Violon", "Violon horizontal",
    "Barplot", "Barplot (effectifs)", "Barplot (frequences)",
    "Barplot empile", "Barplot groupe", "Barplot 100% empile",
    "Barplot avec erreurs", "Barplot temporel", "Barplot empile temporel",
    "Densite par groupe", "Densite 2D", "Histogramme", "Camembert",
    "Mosaique", "Heatmap de contingence"
  )
  couleur_aes <- if (type %in% type_fill) "fill" else "color"

  stat_fun <- switch(stat_barplot,
    "moyenne" = "mean", "mediane" = "median", "somme" = "sum",
    "ecart_type" = "sd", "effectif" = "length",
    "minimum" = "min", "maximum" = "max", "mean")

  type_sans_y <- c("Barplot (effectifs)", "Barplot empile", "Barplot groupe",
                    "Barplot 100% empile", "Barplot empile temporel")

  code_parts <- c()
  g <- NULL

  # ================== CONSTRUCTION ==================
  if (type %in% c("Barplot (effectifs)", "Barplot (frequences)", "Camembert",
                  "Boxplot", "Histogramme") && y_null) {
    if (type == "Barplot (effectifs)") {
      fill_var <- if (!is.null(couleur)) couleur else x
      m <- rboard_aes(x = x, couleur = fill_var, taille = taille, couleur_aes = "fill")
      g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_bar()
      code_parts <- c(
        sprintf("ggplot(donnees, %s)",
                rboard_aes_code(x = x, couleur = fill_var, taille = taille, couleur_aes = "fill")),
        "geom_bar()")
    } else if (type == "Barplot (frequences)") {
      fill_var <- if (!is.null(couleur)) couleur else x
      m <- rboard_aes(x = x, couleur = fill_var, taille = taille, couleur_aes = "fill")
      g <- ggplot2::ggplot(donnees, m) +
        ggplot2::geom_bar() +
        ggplot2::scale_y_continuous(labels = function(v) paste0(round(v * 100), " %"))
      code_parts <- c(
        sprintf("ggplot(donnees, %s)",
                rboard_aes_code(x = x, couleur = fill_var, taille = taille, couleur_aes = "fill")),
        "geom_bar()",
        "scale_y_continuous(labels = function(v) paste0(round(v * 100), ' %'))")
    } else if (type == "Camembert") {
      m <- eval(parse(text = sprintf(
        "ggplot2::aes(x = factor(1), fill = .data[['%s']])", x)))
      g <- ggplot2::ggplot(donnees, m) +
        ggplot2::geom_bar(width = 1) +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::theme_void()
      code_parts <- c(
        sprintf("ggplot(donnees, aes(x = factor(1), fill = %s))", x),
        "geom_bar(width = 1)", "coord_polar('y', start = 0)", "theme_void()")
    } else if (type == "Boxplot") {
      m <- eval(parse(text = sprintf(
        "ggplot2::aes(x = factor(1), y = .data[['%s']])", x)))
      g <- ggplot2::ggplot(donnees, m) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(x = "") +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank())
      code_parts <- c(
        sprintf("ggplot(donnees, aes(x = factor(1), y = %s))", x),
        "geom_boxplot()", "labs(x = '')",
        "theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())")
    } else if (type == "Histogramme") {
      fill_c <- if (!is.null(couleur)) couleur else NULL
      if (!is.null(fill_c)) {
        m <- rboard_aes(x = x, couleur = fill_c, couleur_aes = "fill")
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_histogram(bins = 30, color = "white")
        code_parts <- c(
          sprintf("ggplot(donnees, %s)",
                  rboard_aes_code(x = x, couleur = fill_c, couleur_aes = "fill")),
          "geom_histogram(bins = 30, color = 'white')")
      } else {
        m <- rboard_aes(x = x)
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_histogram(bins = 30, fill = "#2563eb", color = "white")
        code_parts <- c(
          sprintf("ggplot(donnees, %s)", rboard_aes_code(x = x)),
          "geom_histogram(bins = 30, fill = '#2563eb', color = 'white')")
      }
    }
  } else {
    # Cas X + Y
    if (type %in% type_sans_y) {
      m <- rboard_aes(x = x, couleur = couleur, taille = taille, couleur_aes = "fill")
      aes_code <- rboard_aes_code(x = x, couleur = couleur, taille = taille, couleur_aes = "fill")
    } else {
      m <- rboard_aes(x = x, y = y, couleur = couleur, taille = taille,
                      couleur_aes = couleur_aes)
      aes_code <- rboard_aes_code(x = x, y = y, couleur = couleur,
                                   taille = taille, couleur_aes = couleur_aes)
    }
    code_parts <- c(sprintf("ggplot(donnees, %s)", aes_code))

    switch(type,
      "Nuage de points" = {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_point(alpha = 0.7)
        code_parts <- c(code_parts, "geom_point(alpha = 0.7)")
      },
      "Nuage + tendance" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_point(alpha = 0.7) +
          ggplot2::geom_smooth(method = "lm", se = TRUE)
        code_parts <- c(code_parts, "geom_point(alpha = 0.7)",
                        "geom_smooth(method = 'lm', se = TRUE)")
      },
      "Ligne" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_line() + ggplot2::geom_point()
        code_parts <- c(code_parts, "geom_line()", "geom_point()")
      },
      "Ligne inversee" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_line() + ggplot2::geom_point() + ggplot2::coord_flip()
        code_parts <- c(code_parts, "geom_line()", "geom_point()", "coord_flip()")
      },
      "Points" = {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_point(alpha = 0.7)
        code_parts <- c(code_parts, "geom_point(alpha = 0.7)")
      },
      "Points inverses" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_point(alpha = 0.7) + ggplot2::coord_flip()
        code_parts <- c(code_parts, "geom_point(alpha = 0.7)", "coord_flip()")
      },
      "Aire" = {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_area(alpha = 0.6)
        code_parts <- c(code_parts, "geom_area(alpha = 0.6)")
      },
      "Densite 2D" = {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_hex(bins = 30)
        code_parts <- c(code_parts, "geom_hex(bins = 30)")
      },
      "Boxplot" = {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_boxplot()
        code_parts <- c(code_parts, "geom_boxplot()")
      },
      "Boxplot horizontal" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_boxplot() + ggplot2::coord_flip()
        code_parts <- c(code_parts, "geom_boxplot()", "coord_flip()")
      },
      "Boxplot temporel" = {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_boxplot()
        code_parts <- c(code_parts, "geom_boxplot()")
      },
      "Boxplot temporel inverse" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_boxplot() + ggplot2::coord_flip()
        code_parts <- c(code_parts, "geom_boxplot()", "coord_flip()")
      },
      "Violon" = {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_violin(trim = FALSE)
        code_parts <- c(code_parts, "geom_violin(trim = FALSE)")
      },
      "Violon horizontal" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_violin(trim = FALSE) + ggplot2::coord_flip()
        code_parts <- c(code_parts, "geom_violin(trim = FALSE)", "coord_flip()")
      },
      "Densite par groupe" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_density(alpha = 0.5) + ggplot2::coord_flip()
        code_parts <- c(code_parts, "geom_density(alpha = 0.5)", "coord_flip()")
      },
      "Points + jitter" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_jitter(width = 0.2, alpha = 0.7)
        code_parts <- c(code_parts, "geom_jitter(width = 0.2, alpha = 0.7)")
      },
      "Points + jitter horizontal" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_jitter(width = 0.2, alpha = 0.7) + ggplot2::coord_flip()
        code_parts <- c(code_parts, "geom_jitter(width = 0.2, alpha = 0.7)", "coord_flip()")
      },
      "Barplot" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::stat_summary(fun = stat_fun, geom = "bar")
        code_parts <- c(code_parts,
                        sprintf("stat_summary(fun = '%s', geom = 'bar')", stat_fun))
      },
      "Barplot avec erreurs" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::stat_summary(fun = stat_fun, geom = "bar") +
          ggplot2::stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2)
        code_parts <- c(code_parts,
                        sprintf("stat_summary(fun = '%s', geom = 'bar')", stat_fun),
                        "stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2)")
      },
      "Barplot (effectifs)" = {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_bar()
        code_parts <- c(code_parts, "geom_bar()")
      },
      "Barplot empile" = {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_bar(position = "stack")
        code_parts <- c(code_parts, "geom_bar(position = 'stack')")
      },
      "Barplot groupe" = {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_bar(position = "dodge")
        code_parts <- c(code_parts, "geom_bar(position = 'dodge')")
      },
      "Barplot 100% empile" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::geom_bar(position = "fill") +
          ggplot2::scale_y_continuous(labels = function(v) paste0(round(v * 100), " %"))
        code_parts <- c(code_parts, "geom_bar(position = 'fill')",
                        "scale_y_continuous(labels = function(v) paste0(round(v * 100), ' %'))")
      },
      "Barplot temporel" = {
        g <- ggplot2::ggplot(donnees, m) +
          ggplot2::stat_summary(fun = stat_fun, geom = "bar")
        code_parts <- c(code_parts,
                        sprintf("stat_summary(fun = '%s', geom = 'bar')", stat_fun))
      },
      "Barplot empile temporel" = {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_bar(position = "stack")
        code_parts <- c(code_parts, "geom_bar(position = 'stack')")
      },
      "Mosaique" = {
        m_mos <- rboard_aes(x = x, couleur = y, couleur_aes = "fill")
        g <- ggplot2::ggplot(donnees, m_mos) +
          ggplot2::geom_bar(position = "fill") +
          ggplot2::scale_y_continuous(labels = function(v) paste0(round(v * 100), " %"))
        code_parts <- c(
          sprintf("ggplot(donnees, %s)",
                  rboard_aes_code(x = x, couleur = y, couleur_aes = "fill")),
          "geom_bar(position = 'fill')",
          "scale_y_continuous(labels = function(v) paste0(round(v * 100), ' %'))")
      },
      "Heatmap de contingence" = {
        d_agg <- as.data.frame(table(donnees[[x]], donnees[[y]]))
        names(d_agg) <- c("VarX", "VarY", "Freq")
        g <- ggplot2::ggplot(d_agg, ggplot2::aes(x = .data[["VarX"]],
                                                  y = .data[["VarY"]],
                                                  fill = .data[["Freq"]])) +
          ggplot2::geom_tile() +
          ggplot2::scale_fill_viridis_c()
        code_parts <- c(
          sprintf("d_agg <- as.data.frame(table(donnees[['%s']], donnees[['%s']]))", x, y),
          "names(d_agg) <- c('VarX', 'VarY', 'Freq')",
          "ggplot(d_agg, aes(x = VarX, y = VarY, fill = Freq))",
          "geom_tile()", "scale_fill_viridis_c()")
      },
      "Nuage temporel" = {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_point(alpha = 0.7)
        code_parts <- c(code_parts, "geom_point(alpha = 0.7)")
      },
      "Frise chronologique" = {
        m2 <- eval(parse(text = sprintf(
          "ggplot2::aes(x = .data[['%s']], y = factor(1))", x)))
        g <- ggplot2::ggplot(donnees, m2) +
          ggplot2::geom_point(size = 3, color = "#2563eb", alpha = 0.6) +
          ggplot2::labs(y = "") +
          ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank())
        code_parts <- c(
          sprintf("ggplot(donnees, aes(x = %s, y = factor(1)))", x),
          "geom_point(size = 3, color = '#2563eb', alpha = 0.6)",
          "labs(y = '')",
          "theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())")
      },
      {
        g <- ggplot2::ggplot(donnees, m) + ggplot2::geom_point()
        code_parts <- c(code_parts, "geom_point()")
      })
  }

  # --- Facette ---
  if (!is.null(facette) && nzchar(facette) && !identical(type, "Mosaique")) {
    g <- g + ggplot2::facet_wrap(stats::as.formula(paste("~", facette)))
    code_parts <- c(code_parts, sprintf("facet_wrap(~ %s)", facette))
  }

  # --- Titre et labels ---
  if (!is.null(titre) && nzchar(titre)) {
    g <- g + ggplot2::labs(title = titre)
    code_parts <- c(code_parts, sprintf("labs(title = '%s')", gsub("'", "\\\\'", titre)))
  }
  if (!is.null(nom_x) && nzchar(nom_x)) {
    g <- g + ggplot2::labs(x = nom_x)
    code_parts <- c(code_parts, sprintf("labs(x = '%s')", gsub("'", "\\\\'", nom_x)))
  }
  if (!is.null(nom_y) && nzchar(nom_y)) {
    g <- g + ggplot2::labs(y = nom_y)
    code_parts <- c(code_parts, sprintf("labs(y = '%s')", gsub("'", "\\\\'", nom_y)))
  }

  # --- Libelles ---
  if (!isTRUE(afficher_libelles)) {
    g <- g + ggplot2::theme(axis.text = ggplot2::element_blank())
    code_parts <- c(code_parts, "theme(axis.text = element_blank())")
  }

  # --- Palettes ---
  has_couleur <- !is.null(couleur) && nzchar(couleur)
  if (has_couleur) {
    if (palette == "brasserie") {
      if (couleur_aes == "fill") {
        g <- g + ggplot2::scale_fill_brewer(palette = "Set2")
        code_parts <- c(code_parts, "scale_fill_brewer(palette = 'Set2')")
      } else {
        g <- g + ggplot2::scale_color_brewer(palette = "Set2")
        code_parts <- c(code_parts, "scale_color_brewer(palette = 'Set2')")
      }
    } else if (palette == "gris") {
      if (couleur_aes == "fill") {
        g <- g + ggplot2::scale_fill_grey()
        code_parts <- c(code_parts, "scale_fill_grey()")
      } else {
        g <- g + ggplot2::scale_color_grey()
        code_parts <- c(code_parts, "scale_color_grey()")
      }
    } else if (palette == "manuel") {
      cols <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
                "#EDC948", "#B07AA1", "#FF9DA7")
      if (couleur_aes == "fill") {
        g <- g + ggplot2::scale_fill_manual(values = cols)
        code_parts <- c(code_parts,
                        sprintf("scale_fill_manual(values = c(%s))",
                                paste0("'", cols, "'", collapse = ", ")))
      } else {
        g <- g + ggplot2::scale_color_manual(values = cols)
        code_parts <- c(code_parts,
                        sprintf("scale_color_manual(values = c(%s))",
                                paste0("'", cols, "'", collapse = ", ")))
      }
    }
  }

  # --- Theme sobre ---
  g <- g + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom")

  # --- Code final ---
  code_final <- paste(code_parts, collapse = " +\n  ")
  code_final <- paste0("library(ggplot2)\n\n", code_final,
                       " +\n  theme_minimal(base_size = 12)")
  attr(g, "code") <- code_final

  g
}

#' Creer un KPI de type graphique
#' @export
creer_kpi_graphique <- function(nom, objet_ggplot, code = NULL,
                                 commentaire = NULL, groupe = NULL,
                                 type_graphique = NULL) {
  if (is.null(code) && !is.null(attr(objet_ggplot, "code"))) {
    code <- attr(objet_ggplot, "code")
  }
  list(
    id = generer_id("graphique"),
    nom = nom,
    type = "graphique",
    type_graphique = if (!is.null(type_graphique)) type_graphique else "Graphique",
    objet = objet_ggplot,
    code = code,
    commentaire = commentaire,
    groupe = groupe,
    date_creation = Sys.Date()
  )
}

#' Ajouter un graphique a un projet
#' @export
ajouter_graphique <- function(projet, graphique) {
  if (!is.list(projet$graphiques)) projet$graphiques <- list()
  projet$graphiques[[graphique$id]] <- graphique
  projet$date_modification <- Sys.Date()
  projet
}

#' Supprimer un graphique
#' @export
supprimer_graphique <- function(projet, id) {
  if (is.list(projet$graphiques) && !is.null(projet$graphiques[[id]])) {
    projet$graphiques[[id]] <- NULL
    projet$date_modification <- Sys.Date()
  }
  projet
}

#' Modifier les metadonnees d'un graphique
#' @export
modifier_graphique <- function(projet, id_graphique, nouvelles_valeurs) {
  if (!is.list(projet$graphiques) || is.null(projet$graphiques[[id_graphique]])) {
    return(projet)
  }
  champs_autorises <- c("nom", "commentaire", "groupe")
  for (champ in intersect(names(nouvelles_valeurs), champs_autorises)) {
    projet$graphiques[[id_graphique]][[champ]] <- nouvelles_valeurs[[champ]]
  }
  projet$date_modification <- Sys.Date()
  projet
}