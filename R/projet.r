# ==============================================================================
# Rboard - Gestion du cycle de vie des projets
# ==============================================================================

#' Initialiser un nouveau projet Rboard
#' @export
nouveau_projet <- function(nom = "Nouveau projet") {
  list(
    nom = as.character(nom)[1],
    date_creation = Sys.Date(),
    date_modification = Sys.Date(),
    donnees = NULL,
    kpi = list(),
    graphiques = list(),
    textes = list(),
    dashboard = list(),
    commentaires = list()
  )
}

#' Sauvegarder un projet Rboard
#' @export
sauver_projet <- function(projet, chemin) {
  if (is.null(projet) || !is.list(projet)) {
    stop("L'objet a sauvegarder n'est pas un projet Rboard valide.", call. = FALSE)
  }
  projet$date_modification <- Sys.Date()
  saveRDS(projet, file = chemin)
  invisible(chemin)
}

#' Charger un projet Rboard
#' @export
charger_projet <- function(chemin) {
  if (!file.exists(chemin)) {
    stop(sprintf("Le fichier specifie n'existe pas : %s", chemin), call. = FALSE)
  }

  projet <- readRDS(chemin)

  if (!is.list(projet)) {
    stop("Le fichier charge ne contient pas une structure de projet valide.",
         call. = FALSE)
  }

  champs_attendus <- c("nom", "date_creation", "date_modification",
                       "donnees", "kpi", "graphiques", "dashboard", "commentaires")
  manquants <- setdiff(champs_attendus, names(projet))
  if (length(manquants) > 0) {
    stop(
      sprintf("Structure de projet invalide. Champs manquants : %s",
              paste(manquants, collapse = ", ")),
      call. = FALSE
    )
  }

  projet <- reparer_projet(projet)

  projet
}

# ==============================================================================
# Helpers de nettoyage
# ==============================================================================

#' Detecte si un commentaire est un commentaire automatique du Script Runner
#'
#' Reconnait le motif : "Extrait depuis un script (classe XXX)."
#'
#' @param com Caractere ou NULL.
#' @return Logique.
#' @keywords internal
rboard_est_commentaire_auto_script <- function(com) {
  if (is.null(com) || length(com) == 0) return(FALSE)
  c <- as.character(com)[1]
  if (is.na(c) || !nzchar(c)) return(FALSE)
  grepl("^\\s*Extrait depuis un script \\(classe [^)]+\\)\\.?\\s*$", c)
}

# ==============================================================================
# Migration d'un element de dashboard
# ==============================================================================

#' Reparer / migrer un element de dashboard
#' @keywords internal
reparer_element <- function(el, projet) {
  if (is.null(el) || !is.list(el)) return(el)

  # --- ID ---
  if (is.null(el$id) || !nzchar(as.character(el$id)[1])) {
    el$id <- generer_id("element")
  }

  # --- Type ---
  if (is.null(el$type) || !el$type %in% c("kpi", "graphique", "texte")) {
    el$type <- "texte"
  }
  type_el <- as.character(el$type)[1]

  # --- Style ---
  if (is.null(el$style)) el$style <- "normal"

  # --- Nettoyage commentaire auto (Script Runner) ---
  if (!is.null(el$commentaire) && rboard_est_commentaire_auto_script(el$commentaire)) {
    el$commentaire <- NULL
  }

  # --- Largeur ---
  taille <- suppressWarnings(as.integer(el$taille)[1])
  if (is.na(taille) || !(taille %in% c(3L, 6L, 12L))) {
    if (!is.na(taille) && taille == 4L) {
      taille <- 6L
    } else {
      taille <- 6L
    }
  }
  el$taille <- taille

  # --- Tableau long ---
  if (is.null(el$tableau_long)) {
    tbl_long <- FALSE
    if (identical(type_el, "kpi") && !is.null(projet)) {
      k <- projet$kpi[[el$id_kpi]]
      if (!is.null(k) && identical(as.character(k$type)[1], "tableau")) {
        if (rboard_suggere_tableau_long(k)) {
          tbl_long <- TRUE
        }
      }
    }
    el$tableau_long <- tbl_long
  } else {
    el$tableau_long <- isTRUE(el$tableau_long)
  }

  # --- Tableau etendu ---
  if (is.null(el$tableau_etendu)) {
    el$tableau_etendu <- FALSE
  } else {
    el$tableau_etendu <- isTRUE(el$tableau_etendu)
  }

  # --- Coherence tableau_long / tableau_etendu ---
  if (isTRUE(el$tableau_long) && isTRUE(el$tableau_etendu)) {
    el$tableau_etendu <- FALSE
  }

  # --- Largeur selon le format ---
  if (isTRUE(el$tableau_long)) {
    if (!(el$taille %in% c(6L, 12L))) el$taille <- 12L
  } else if (isTRUE(el$tableau_etendu)) {
    el$taille <- 6L
  }

  # --- Graphique long ---
  if (is.null(el$graphique_long)) {
    el$graphique_long <- FALSE
  } else {
    el$graphique_long <- isTRUE(el$graphique_long)
  }

  # --- Reference vers la bibliotheque de textes ---
  if (is.null(el$id_texte)) {
    el$id_texte <- NULL
  } else {
    idt <- as.character(el$id_texte)[1]
    if (is.na(idt) || !nzchar(idt)) {
      el$id_texte <- NULL
    } else {
      el$id_texte <- idt
    }
  }

  # --- Hauteur ---
  h <- suppressWarnings(as.integer(el$hauteur_unites)[1])
  if (is.na(h) || !(h %in% c(1L, 2L, 3L, 4L))) {
    kpi_ref <- NULL
    if (identical(type_el, "kpi") && !is.null(projet)) {
      kpi_ref <- projet$kpi[[el$id_kpi]]
    }
    h <- rboard_hauteur_element(type_el, kpi_ref,
                                 tableau_long = el$tableau_long,
                                 tableau_etendu = el$tableau_etendu,
                                 graphique_long = el$graphique_long)
  }
  el$hauteur_unites <- as.integer(h)

  el
}

# ==============================================================================
# Reparation d'un projet
# ==============================================================================

#' Reparer un projet apres chargement
#' @export
reparer_projet <- function(projet) {
  if (is.null(projet) || !is.list(projet)) return(nouveau_projet())

  # --- Champs de base manquants ---
  if (!"nom" %in% names(projet) || is.null(projet$nom)) {
    projet["nom"] <- list("Projet sans nom")
  }
  if (!"donnees" %in% names(projet)) {
    projet["donnees"] <- list(NULL)
  }
  if (!"commentaires" %in% names(projet)) {
    projet["commentaires"] <- list(list())
  }
  if (!"kpi" %in% names(projet)) {
    projet["kpi"] <- list(list())
  }
  if (!"graphiques" %in% names(projet)) {
    projet["graphiques"] <- list(list())
  }
  if (!"textes" %in% names(projet)) {
    projet["textes"] <- list(list())
  }
  if (!"dashboard" %in% names(projet)) {
    projet["dashboard"] <- list(list())
  }
  if (!"date_creation" %in% names(projet)) {
    projet["date_creation"] <- list(Sys.Date())
  }
  if (!"date_modification" %in% names(projet)) {
    projet["date_modification"] <- list(Sys.Date())
  }

  # --- Dates ---
  if (is.null(projet$date_creation) || !inherits(projet$date_creation, "Date")) {
    projet$date_creation <- Sys.Date()
  }
  if (is.null(projet$date_modification) || !inherits(projet$date_modification, "Date")) {
    projet$date_modification <- Sys.Date()
  }

  # --- Collections ---
  if (is.null(projet$kpi) || !is.list(projet$kpi)) projet$kpi <- list()
  if (is.null(projet$graphiques) || !is.list(projet$graphiques))
    projet$graphiques <- list()
  if (is.null(projet$textes) || !is.list(projet$textes))
    projet$textes <- list()
  if (is.null(projet$dashboard) || !is.list(projet$dashboard))
    projet$dashboard <- list()
  if (is.null(projet$dashboard$pages)) projet$dashboard$pages <- list()
  if (is.null(projet$commentaires) || !is.list(projet$commentaires))
    projet$commentaires <- list()

  # --- Reparation des KPI ---
  if (length(projet$kpi) > 0) {
    for (i in seq_along(projet$kpi)) {
      k <- projet$kpi[[i]]
      if (is.null(k) || !is.list(k)) next

      if (is.null(k$id)) k$id <- generer_id("kpi")
      if (is.null(k$nom)) k$nom <- "KPI"
      if (is.null(k$type)) k$type <- "generique"
      if (is.null(k$format)) k$format <- "decimal_3"
      if (is.null(k$regles_couleur) || !is.list(k$regles_couleur))
        k$regles_couleur <- list()

      # --- Nettoyage commentaire auto Script Runner ---
      if (!is.null(k$commentaire) && rboard_est_commentaire_auto_script(k$commentaire)) {
        k$commentaire <- NULL
      }

      if (is.null(k$couleur) && !identical(as.character(k$type)[1], "tableau")) {
        k$couleur <- decider_couleur(k)
      }

      projet$kpi[[i]] <- k
    }
  }

  # --- Reparation des graphiques ---
  if (length(projet$graphiques) > 0) {
    for (i in seq_along(projet$graphiques)) {
      g <- projet$graphiques[[i]]
      if (is.null(g)) next

      # --- Nettoyage commentaire auto Script Runner ---
      if (!is.null(g$commentaire) && rboard_est_commentaire_auto_script(g$commentaire)) {
        g$commentaire <- NULL
      }

      if (is.null(g$objet) && !is.null(g$code) && nzchar(g$code)) {
        g_reconstruit <- tryCatch({
          env <- new.env(parent = globalenv())
          eval(parse(text = g$code), envir = env)
        }, error = function(e) NULL)

        if (!is.null(g_reconstruit) && inherits(g_reconstruit, "ggplot")) {
          g$objet <- g_reconstruit
        }
      }

      projet$graphiques[[i]] <- g
    }
  }

  # --- Reparation des textes ---
  if (length(projet$textes) > 0) {
    for (i in seq_along(projet$textes)) {
      t <- projet$textes[[i]]
      if (is.null(t) || !is.list(t)) next

      if (is.null(t$id)) t$id <- generer_id("texte")
      if (is.null(t$nom)) t$nom <- "Texte"
      if (is.null(t$contenu)) t$contenu <- ""
      if (is.null(t$style)) t$style <- "normal"
      if (is.null(t$source)) t$source <- "script"
      if (is.null(t$date_creation)) t$date_creation <- Sys.Date()

      # --- Nettoyage commentaire auto Script Runner ---
      if (!is.null(t$commentaire) && rboard_est_commentaire_auto_script(t$commentaire)) {
        t$commentaire <- NULL
      }

      projet$textes[[i]] <- t
    }
  }

  # --- Reparation des pages / lignes / elements ---
  if (length(projet$dashboard$pages) > 0) {
    for (j in seq_along(projet$dashboard$pages)) {
      pg <- projet$dashboard$pages[[j]]
      if (is.null(pg$id)) pg$id <- generer_id("page")
      if (is.null(pg$titre)) pg$titre <- sprintf("Page %d", j)
      if (is.null(pg$lignes) || !is.list(pg$lignes)) pg$lignes <- list()

      if (length(pg$lignes) > 0) {
        for (k in seq_along(pg$lignes)) {
          lg <- pg$lignes[[k]]
          if (is.null(lg$id)) lg$id <- generer_id("ligne")
          if (is.null(lg$elements) || !is.list(lg$elements)) lg$elements <- list()

          if (length(lg$elements) > 0) {
            for (m in seq_along(lg$elements)) {
              el <- lg$elements[[m]]
              lg$elements[[m]] <- reparer_element(el, projet)
            }
          }
          pg$lignes[[k]] <- lg
        }
      }
      projet$dashboard$pages[[j]] <- pg
    }
  }

  projet
}

# ==============================================================================
# Verification d'un projet
# ==============================================================================

#' @export
verifier_projet <- function(projet) {
  problemes <- character(0)

  if (is.null(projet) || !is.list(projet)) {
    return("Le projet n'est pas une liste valide.")
  }

  champs_obligatoires <- c("nom", "date_creation", "date_modification",
                           "donnees", "kpi", "graphiques", "textes",
                           "dashboard", "commentaires")
  for (ch in champs_obligatoires) {
    if (!ch %in% names(projet)) {
      problemes <- c(problemes, sprintf("Champ manquant : %s", ch))
    }
  }

  if (!is.null(projet$kpi) && is.list(projet$kpi)) {
    for (id in names(projet$kpi)) {
      k <- projet$kpi[[id]]
      if (is.null(k$id)) problemes <- c(problemes, sprintf("KPI %s : pas d'id", id))
      if (is.null(k$nom)) problemes <- c(problemes, sprintf("KPI %s : pas de nom", id))
      if (is.null(k$type)) problemes <- c(problemes, sprintf("KPI %s : pas de type", id))
    }
  }

  if (!is.null(projet$graphiques) && is.list(projet$graphiques)) {
    for (id in names(projet$graphiques)) {
      g <- projet$graphiques[[id]]
      if (is.null(g$objet) || !inherits(g$objet, "ggplot")) {
        problemes <- c(problemes, sprintf("Graphique %s : ggplot manquant", id))
      }
    }
  }

  if (!is.null(projet$textes) && is.list(projet$textes)) {
    for (id in names(projet$textes)) {
      t <- projet$textes[[id]]
      if (is.null(t$id)) problemes <- c(problemes, sprintf("Texte %s : pas d'id", id))
      if (is.null(t$contenu) || !nzchar(trimws(as.character(t$contenu)[1]))) {
        problemes <- c(problemes, sprintf("Texte %s : contenu vide", id))
      }
    }
  }

  if (!is.null(projet$dashboard) && !is.null(projet$dashboard$pages)) {
    for (pg in projet$dashboard$pages) {
      if (is.null(pg$lignes)) next
      for (lg in pg$lignes) {
        if (is.null(lg$elements) || length(lg$elements) == 0) next

        largeurs <- vapply(lg$elements,
                           function(e) as.integer(e$taille %||% 6L),
                           integer(1))
        total_l <- sum(largeurs)
        if (total_l > RBOARD_GRILLE_LARGEUR) {
          problemes <- c(problemes, sprintf(
            "Ligne %s : largeur %d > %d colonnes.",
            lg$id, total_l, RBOARD_GRILLE_LARGEUR))
        }

        hauteurs <- vapply(lg$elements,
                           function(e) as.integer(e$hauteur_unites %||% 1L),
                           integer(1))
        if (any(hauteurs < 1L | hauteurs > 4L, na.rm = TRUE)) {
          problemes <- c(problemes, sprintf(
            "Ligne %s : hauteur d'element hors bornes (1, 2, 3 ou 4).",
            lg$id))
        }

        for (el in lg$elements) {
          if (isTRUE(el$tableau_long) && isTRUE(el$tableau_etendu)) {
            problemes <- c(problemes, sprintf(
              "Element %s : tableau_long et tableau_etendu tous deux actifs.",
              el$id))
          }
          if (!is.null(el$id_texte) && nzchar(as.character(el$id_texte)[1])) {
            if (is.null(projet$textes) || is.null(projet$textes[[el$id_texte]])) {
              problemes <- c(problemes, sprintf(
                "Element %s : id_texte '%s' introuvable (fallback sur contenu).",
                el$id, el$id_texte))
            }
          }
        }
      }
    }
  }

  if (length(problemes) == 0) TRUE else problemes
}

# ==============================================================================
# Resume d'un projet
# ==============================================================================

#' @export
resumer_projet <- function(projet) {
  if (is.null(projet) || !is.list(projet)) {
    return(list(nom = NA_character_, nb_kpi = 0L, nb_graphiques = 0L,
                nb_textes = 0L, nb_pages = 0L, nb_lignes = 0L,
                nb_elements = 0L, hauteur_totale_unites = 0L))
  }

  nb_kpi     <- if (!is.null(projet$kpi)) length(projet$kpi) else 0L
  nb_graph   <- if (!is.null(projet$graphiques)) length(projet$graphiques) else 0L
  nb_txt     <- if (!is.null(projet$textes)) length(projet$textes) else 0L
  nb_pages   <- if (!is.null(projet$dashboard) &&
                    !is.null(projet$dashboard$pages)) {
    length(projet$dashboard$pages)
  } else 0L

  nb_lignes <- 0L
  nb_elements <- 0L
  hauteur_totale <- 0L

  if (nb_pages > 0) {
    for (pg in projet$dashboard$pages) {
      if (is.null(pg$lignes)) next
      nb_lignes <- nb_lignes + length(pg$lignes)
      for (lg in pg$lignes) {
        if (is.null(lg$elements)) next
        nb_elements <- nb_elements + length(lg$elements)
        hs <- vapply(lg$elements,
                     function(e) as.integer(e$hauteur_unites %||% 1L),
                     integer(1))
        hauteur_totale <- hauteur_totale + max(c(0L, hs))
      }
    }
  }

  list(
    nom = as.character(projet$nom %||% NA_character_)[1],
    nb_kpi = as.integer(nb_kpi),
    nb_graphiques = as.integer(nb_graph),
    nb_textes = as.integer(nb_txt),
    nb_pages = as.integer(nb_pages),
    nb_lignes = as.integer(nb_lignes),
    nb_elements = as.integer(nb_elements),
    hauteur_totale_unites = as.integer(hauteur_totale)
  )
}