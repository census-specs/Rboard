#' ==============================================================================
#' Rboard - Script Runner et capture statistique
#' ==============================================================================

#' Executer un script R dans un environnement isole
#' @export
executer_script <- function(chemin_script, donnees = NULL) {
  if (!file.exists(chemin_script)) {
    stop(sprintf("Fichier introuvable : %s", chemin_script), call. = FALSE)
  }

  env <- new.env(parent = globalenv())
  if (!is.null(donnees)) env$donnees <- donnees

  warnings_captures <- character(0)
  erreurs_captures <- character(0)
  succes <- TRUE
  sortie <- character(0)

  tryCatch({
    sortie <- capture.output({
      withCallingHandlers({
        source(chemin_script, local = env, echo = FALSE)
      }, warning = function(w) {
        warnings_captures <<- c(warnings_captures, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    }, type = c("output", "message"))
  }, error = function(e) {
    succes <<- FALSE
    erreurs_captures <<- c(erreurs_captures, conditionMessage(e))
  })

  noms_objets <- ls(envir = env, all.names = FALSE)
  noms_objets <- setdiff(noms_objets, "donnees")

  objets_crees <- list()
  for (nm in noms_objets) {
    objets_crees[[nm]] <- get(nm, envir = env)
  }

  list(
    succes = succes,
    sortie_texte = paste(sortie, collapse = "\n"),
    warnings = warnings_captures,
    erreurs = erreurs_captures,
    objets = objets_crees,
    environnement = env
  )
}

# ==============================================================================
# Detection des KPI simples
# ==============================================================================

#' Detecter les objets statistiques transformables en KPI simples
#'
#' Ne detecte QUE :
#' - les objets htest (test t, chi2, correlation, etc.) : p-value, estimate, statistic
#' - les objets lm / glm : R2, F, p-value
#' - les objets aov : F, p-value
#' - les objets table : effectif total
#' - les scalaires numeriques (longueur 1) : valeur brute
#'
#' Les vecteurs numeriques de longueur > 1 sont ignores. Les data.frame,
#' chaines de caracteres et ggplot sont detectes par detecter_tableaux(),
#' detecter_textes() et detecter_graphiques().
#'
#' @export
detecter_objets_statistiques <- function(objets) {
  df_vide <- data.frame(
    nom = character(0), classe = character(0), type_kpi = character(0),
    valeur = numeric(0), label = character(0), source = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(objets) || length(objets) == 0) return(df_vide)

  lignes <- list()
  ajouter <- function(nom, classe, type_kpi, valeur, label) {
    lignes[[length(lignes) + 1]] <<- list(
      nom = nom, classe = classe, type_kpi = type_kpi,
      valeur = as.numeric(valeur), label = label, source = "script"
    )
  }

  for (nom_obj in names(objets)) {
    obj <- objets[[nom_obj]]
    if (is.null(obj)) next
    classe_principale <- class(obj)[1]

    # --- htest ---
    if (inherits(obj, "htest")) {
      if (!is.null(obj$p.value) && is.numeric(obj$p.value) && length(obj$p.value) == 1) {
        ajouter(paste0(nom_obj, " (p-value)"), classe_principale, "p_value",
                obj$p.value, sprintf("p = %s", formater_valeur(obj$p.value, "decimal_3")))
      }
      if (!is.null(obj$estimate) && length(obj$estimate) > 0) {
        val_est <- as.numeric(obj$estimate[1])
        methode <- if (!is.null(obj$method)) tolower(obj$method) else ""
        if (grepl("correlation", methode)) {
          ajouter(paste0(nom_obj, " (correlation)"), classe_principale,
                  "correlation", val_est,
                  sprintf("r = %s", formater_valeur(val_est, "decimal_2")))
        } else {
          nom_param <- if (!is.null(names(obj$estimate)[1]) && nzchar(names(obj$estimate)[1])) {
            names(obj$estimate)[1]
          } else "estimation"
          ajouter(paste0(nom_obj, " (", nom_param, ")"), classe_principale,
                  "generique", val_est,
                  sprintf("%s = %s", nom_param, formater_valeur(val_est, "decimal_3")))
        }
      }
      if (!is.null(obj$statistic) && length(obj$statistic) > 0) {
        val_stat <- as.numeric(obj$statistic[1])
        nom_stat <- if (!is.null(names(obj$statistic)[1]) && nzchar(names(obj$statistic)[1])) {
          names(obj$statistic)[1]
        } else "statistique"
        ajouter(paste0(nom_obj, " (", nom_stat, ")"), classe_principale,
                "generique", val_stat,
                sprintf("%s = %s", nom_stat, formater_valeur(val_stat, "decimal_2")))
      }
    } else if (inherits(obj, "lm") || inherits(obj, "glm")) {
      s <- tryCatch(summary(obj), error = function(e) NULL)
      if (!is.null(s)) {
        if (!is.null(s$r.squared) && is.numeric(s$r.squared)) {
          ajouter(paste0(nom_obj, " (R2)"), classe_principale, "generique",
                  s$r.squared, sprintf("R2 = %s", formater_valeur(s$r.squared, "decimal_3")))
        }
        if (!is.null(s$fstatistic) && length(s$fstatistic) >= 3) {
          val_f <- as.numeric(s$fstatistic[1])
          p_val <- stats::pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)
          ajouter(paste0(nom_obj, " (F)"), classe_principale, "generique",
                  val_f, sprintf("F = %s", formater_valeur(val_f, "decimal_2")))
          ajouter(paste0(nom_obj, " (p-value)"), classe_principale, "p_value",
                  p_val, sprintf("p = %s", formater_valeur(p_val, "decimal_3")))
        }
      }
    } else if (inherits(obj, "aov")) {
      s_aov <- tryCatch(summary(obj)[[1]], error = function(e) NULL)
      if (!is.null(s_aov)) {
        if ("F value" %in% names(s_aov)) {
          vals_f <- na.omit(s_aov[["F value"]])
          if (length(vals_f) > 0) {
            ajouter(paste0(nom_obj, " (F)"), classe_principale, "generique",
                    as.numeric(vals_f[1]),
                    sprintf("F = %s", formater_valeur(vals_f[1], "decimal_2")))
          }
        }
        if ("Pr(>F)" %in% names(s_aov)) {
          vals_p <- na.omit(s_aov[["Pr(>F)"]])
          if (length(vals_p) > 0) {
            ajouter(paste0(nom_obj, " (p-value)"), classe_principale, "p_value",
                    as.numeric(vals_p[1]),
                    sprintf("p = %s", formater_valeur(vals_p[1], "decimal_3")))
          }
        }
      }
    } else if (inherits(obj, "table")) {
      tot <- sum(obj, na.rm = TRUE)
      ajouter(paste0(nom_obj, " (effectif)"), classe_principale, "effectif",
              tot, sprintf("N = %s", formater_valeur(tot, "entier")))
    } else if (is.numeric(obj) && !is.matrix(obj) && !is.array(obj) && length(obj) == 1) {
      if (!is.na(obj)) {
        ajouter(nom_obj, "numeric", "generique",
                obj, sprintf("%s = %s", nom_obj, formater_valeur(obj, "decimal_3")))
      }
    }
  }

  if (length(lignes) == 0) return(df_vide)

  data.frame(
    nom = sapply(lignes, function(x) x$nom),
    classe = sapply(lignes, function(x) x$classe),
    type_kpi = sapply(lignes, function(x) x$type_kpi),
    valeur = sapply(lignes, function(x) x$valeur),
    label = sapply(lignes, function(x) x$label),
    source = sapply(lignes, function(x) x$source),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# Detection des tableaux
# ==============================================================================

#' Detecter les data.frame transformables en KPI tableau
#' @export
detecter_tableaux <- function(objets) {
  df_vide <- data.frame(
    nom = character(0),
    n_lignes = integer(0),
    n_colonnes = integer(0),
    apercu = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(objets) || length(objets) == 0) return(df_vide)

  lignes <- list()
  for (nom_obj in names(objets)) {
    obj <- objets[[nom_obj]]
    if (is.null(obj)) next

    if (is.data.frame(obj) && nrow(obj) > 0 && ncol(obj) > 0) {
      n <- nrow(obj); p <- ncol(obj)
      apercu <- paste0(n, " x ", p, " | ", paste(head(names(obj), 3), collapse = ", "))
      if (p > 3) apercu <- paste0(apercu, ", ...")
      lignes[[length(lignes) + 1]] <- list(
        nom = nom_obj,
        n_lignes = as.integer(n),
        n_colonnes = as.integer(p),
        apercu = apercu
      )
    } else if (is.matrix(obj) && nrow(obj) > 0 && ncol(obj) > 0) {
      df_tmp <- as.data.frame(obj, stringsAsFactors = FALSE)
      n <- nrow(df_tmp); p <- ncol(df_tmp)
      apercu <- paste0(n, " x ", p, " (matrix)")
      lignes[[length(lignes) + 1]] <- list(
        nom = nom_obj,
        n_lignes = as.integer(n),
        n_colonnes = as.integer(p),
        apercu = apercu
      )
    }
  }

  if (length(lignes) == 0) return(df_vide)

  data.frame(
    nom = sapply(lignes, function(x) x$nom),
    n_lignes = sapply(lignes, function(x) x$n_lignes),
    n_colonnes = sapply(lignes, function(x) x$n_colonnes),
    apercu = sapply(lignes, function(x) x$apercu),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# Detection des textes markdown
# ==============================================================================

#' Detecter les chaines de caracteres transformables en blocs de texte
#' @export
detecter_textes <- function(objets) {
  df_vide <- data.frame(
    nom = character(0),
    n_caracteres = integer(0),
    apercu = character(0),
    style_suggere = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(objets) || length(objets) == 0) return(df_vide)

  lignes <- list()
  for (nom_obj in names(objets)) {
    obj <- objets[[nom_obj]]
    if (is.null(obj)) next

    if (is.character(obj) && length(obj) == 1 && nzchar(trimws(obj))) {
      txt <- obj
      n_car <- nchar(txt)
      apercu <- if (n_car > 60) paste0(substr(txt, 1, 57), "...") else txt

      style_suggere <- "normal"
      if (grepl("^###?#?\\s", txt) || grepl("^\\*\\*[^*]+\\*\\*$", txt)) {
        style_suggere <- "titre"
      } else if (grepl("^>\\s", txt) || grepl("(?i)^(note|remarque|info)", txt, perl = TRUE)) {
        style_suggere <- "note"
      } else if (grepl("(?i)(attention|avertissement|danger|warning)", txt, perl = TRUE)) {
        style_suggere <- "avertissement"
      }

      lignes[[length(lignes) + 1]] <- list(
        nom = nom_obj,
        n_caracteres = as.integer(n_car),
        apercu = apercu,
        style_suggere = style_suggere
      )
    }
  }

  if (length(lignes) == 0) return(df_vide)

  data.frame(
    nom = sapply(lignes, function(x) x$nom),
    n_caracteres = sapply(lignes, function(x) x$n_caracteres),
    apercu = sapply(lignes, function(x) x$apercu),
    style_suggere = sapply(lignes, function(x) x$style_suggere),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# Detection des graphiques ggplot
# ==============================================================================

#' Detecter les objets ggplot transformables en KPI graphique
#' @export
detecter_graphiques <- function(objets) {
  df_vide <- data.frame(
    nom = character(0),
    classe = character(0),
    nb_couches = integer(0),
    stringsAsFactors = FALSE
  )
  if (is.null(objets) || length(objets) == 0) return(df_vide)

  lignes <- list()
  for (nom_obj in names(objets)) {
    obj <- objets[[nom_obj]]
    if (is.null(obj)) next

    if (inherits(obj, "ggplot")) {
      nb_couches <- tryCatch(length(obj$layers), error = function(e) 0L)
      lignes[[length(lignes) + 1]] <- list(
        nom = nom_obj,
        classe = "ggplot",
        nb_couches = as.integer(nb_couches)
      )
    }
  }

  if (length(lignes) == 0) return(df_vide)

  data.frame(
    nom = sapply(lignes, function(x) x$nom),
    classe = sapply(lignes, function(x) x$classe),
    nb_couches = sapply(lignes, function(x) x$nb_couches),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# Detection globale
# ==============================================================================

#' Detecter tous les objets exploitables d'un script
#' @export
detecter_tous_objets <- function(objets) {
  list(
    kpis       = detecter_objets_statistiques(objets),
    tableaux   = detecter_tableaux(objets),
    textes     = detecter_textes(objets),
    graphiques = detecter_graphiques(objets)
  )
}

# ==============================================================================
# Helpers d'ajout au projet
# ==============================================================================

#' Creer un KPI a partir d'un resultat detecte
#' @export
creer_kpi_depuis_objet <- function(nom_objet, valeur, type, label,
                                    source = "script", commentaire = NULL) {
  creer_kpi(
    nom = nom_objet, valeur = valeur, type = type, label = label,
    source = source, commentaire = commentaire
  )
}

#' Ajouter un KPI extrait au projet
#' @export
ajouter_kpi_au_projet <- function(projet, kpi) {
  ajouter_kpi(projet, kpi)
}

#' Ajouter un tableau extrait au projet (cree un KPI de type tableau)
#' @export
ajouter_tableau_au_projet <- function(projet, nom, tableau,
                                       commentaire = NULL) {
  if (is.null(tableau) || !is.data.frame(tableau) || nrow(tableau) == 0) {
    stop("Tableau vide ou invalide.", call. = FALSE)
  }
  k <- creer_kpi_tableau(
    nom = nom,
    tableau = tableau,
    sous_type = "par_groupe",
    mode_affichage = "effectifs",
    stats_annexes = list(),
    commentaire = commentaire,
    regles_couleur = list(),
    colonne_valeur_index = if (ncol(tableau) >= 2) 2L else NULL
  )
  ajouter_kpi(projet, k)
}

#' Ajouter un texte extrait au projet (collection dediee)
#' @export
ajouter_texte_au_projet <- function(projet, nom, contenu,
                                     style = "normal",
                                     commentaire = NULL) {
  if (is.null(contenu) || !nzchar(trimws(contenu))) {
    stop("Contenu vide.", call. = FALSE)
  }
  if (is.null(projet$textes) || !is.list(projet$textes)) {
    projet$textes <- list()
  }
  id_txt <- generer_id("texte")
  projet$textes[[id_txt]] <- list(
    id = id_txt,
    nom = as.character(nom)[1],
    contenu = as.character(contenu)[1],
    style = as.character(style)[1],
    commentaire = if (!is.null(commentaire)) as.character(commentaire)[1] else NULL,
    source = "script",
    date_creation = Sys.Date()
  )
  projet$date_modification <- Sys.Date()
  projet
}

#' Ajouter un graphique ggplot extrait au projet
#' @export
ajouter_graphique_depuis_script <- function(projet, nom, objet_ggplot,
                                             commentaire = NULL,
                                             type_graphique = NULL) {
  if (is.null(objet_ggplot) || !inherits(objet_ggplot, "ggplot")) {
    stop("L'objet n'est pas un ggplot.", call. = FALSE)
  }
  k <- creer_kpi_graphique(
    nom = nom,
    objet_ggplot = objet_ggplot,
    code = attr(objet_ggplot, "code"),
    commentaire = commentaire,
    type_graphique = type_graphique
  )
  ajouter_graphique(projet, k)
}