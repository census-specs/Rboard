# ==============================================================================
# Rboard - Fonctions metier du dashboard
# ==============================================================================

RBOARD_GRILLE_LARGEUR   <- 12L
RBOARD_GRILLE_HAUTEUR   <- 6L

# Hauteurs par type (en unites de 1/6 de page)
HAUTEUR_KPI              <- 1L
HAUTEUR_TEXTE            <- 1L
HAUTEUR_TABLEAU          <- 2L   # tableau compact
HAUTEUR_TABLEAU_LONG     <- 3L   # tableau long (largeur 6 ou 12)
HAUTEUR_TABLEAU_ETENDU   <- 4L   # tableau etendu (largeur 6)
HAUTEUR_GRAPHIQUE        <- 3L
HAUTEUR_GRAPHIQUE_LARGE  <- 4L

# Largeurs autorisees
LARGEURS_STANDARD     <- c(3L, 6L, 12L)
LARGEURS_LARGES       <- c(6L, 12L)
LARGEURS_TABLEAU_LONG <- c(6L, 12L)   # tableau long : 6 ou 12
LARGEURS_TABLEAU_ETENDU <- 6L         # tableau etendu : 6 uniquement

SEUIL_TABLEAU_LONG  <- 7L

# ==============================================================================
# Helpers de dimensions
# ==============================================================================

#' @export
rboard_hauteur_element <- function(type, kpi = NULL,
                                    tableau_long = FALSE,
                                    tableau_etendu = FALSE,
                                    graphique_long = FALSE) {
  type_chr <- as.character(type)[1]

  if (identical(type_chr, "graphique")) {
    return(if (isTRUE(graphique_long)) HAUTEUR_GRAPHIQUE_LARGE else HAUTEUR_GRAPHIQUE)
  }
  if (identical(type_chr, "texte")) return(HAUTEUR_TEXTE)

  if (identical(type_chr, "kpi") && !is.null(kpi)) {
    if (identical(as.character(kpi$type)[1], "tableau")) {
      if (isTRUE(tableau_long))   return(HAUTEUR_TABLEAU_LONG)
      if (isTRUE(tableau_etendu)) return(HAUTEUR_TABLEAU_ETENDU)
      return(HAUTEUR_TABLEAU)
    }
    return(HAUTEUR_KPI)
  }
  HAUTEUR_KPI
}

#' @export
rboard_largeurs_autorisees <- function(type, kpi = NULL,
                                        tableau_long = FALSE,
                                        tableau_etendu = FALSE,
                                        graphique_long = FALSE) {
  type_chr <- as.character(type)[1]

  if (identical(type_chr, "graphique")) return(LARGEURS_LARGES)
  if (identical(type_chr, "texte"))     return(LARGEURS_STANDARD)

  if (identical(type_chr, "kpi") && !is.null(kpi)) {
    if (identical(as.character(kpi$type)[1], "tableau")) {
      if (isTRUE(tableau_long))   return(LARGEURS_TABLEAU_LONG)
      if (isTRUE(tableau_etendu)) return(LARGEURS_TABLEAU_ETENDU)
      return(LARGEURS_STANDARD)
    }
    return(LARGEURS_STANDARD)
  }
  LARGEURS_STANDARD
}

#' @export
rboard_suggere_tableau_long <- function(kpi, seuil = SEUIL_TABLEAU_LONG) {
  if (is.null(kpi) || !identical(as.character(kpi$type)[1], "tableau")) return(FALSE)
  tab <- kpi$tableau
  if (is.null(tab) || nrow(tab) == 0) return(FALSE)
  nrow(tab) > as.integer(seuil)
}

# ==============================================================================
# Structure
# ==============================================================================

#' @export
creer_dashboard <- function() list(pages = list())

#' @export
creer_page <- function(titre = "Nouvelle page") {
  if (is.null(titre) || !nzchar(trimws(as.character(titre)))) {
    titre <- "Nouvelle page"
  }
  list(id = generer_id("page"), titre = as.character(titre), lignes = list())
}

#' @export
creer_ligne <- function() list(id = generer_id("ligne"), elements = list())

#' @export
creer_element <- function(type, id_kpi = NULL, contenu = NULL,
                           style = "normal", taille_largeur = 6,
                           nom_affiche = NULL, commentaire = NULL,
                           couleur_forcee = NULL, niveau_titre = NULL,
                           hauteur_unites = NULL,
                           tableau_long = FALSE,
                           tableau_etendu = FALSE,
                           graphique_long = FALSE,
                           id_texte = NULL) {
  type_chr <- as.character(type)[1]
  if (is.na(type_chr) || !(type_chr %in% c("kpi", "graphique", "texte"))) {
    stop("Type d'element invalide.", call. = FALSE)
  }

  largeur <- suppressWarnings(as.integer(taille_largeur)[1])
  if (is.na(largeur) || !(largeur %in% c(3L, 6L, 12L))) {
    stop("Largeur invalide. Doit valoir 3, 6 ou 12.", call. = FALSE)
  }

  niveau_val <- NULL
  if (identical(type_chr, "texte") && identical(as.character(style)[1], "titre")) {
    n <- suppressWarnings(as.integer(niveau_titre)[1])
    if (is.na(n) || !(n %in% c(1L, 2L, 3L))) n <- 2L
    niveau_val <- n
  }

  # --- Coherence des flags tableau ---
  tbl_long   <- isTRUE(tableau_long)
  tbl_etendu <- isTRUE(tableau_etendu)
  if (tbl_long && tbl_etendu) {
    stop("Un tableau ne peut pas etre a la fois 'long' et 'etendu'.", call. = FALSE)
  }

  # --- Ajustement de la largeur selon le format ---
  if (tbl_long) {
    # Tableau long : 6 ou 12. Fallback sur 12 si non conforme.
    if (!(largeur %in% c(6L, 12L))) largeur <- 12L
  }
  if (tbl_etendu) {
    # Tableau etendu : 6 uniquement
    largeur <- 6L
  }

  # --- Hauteur deduite si non fournie ---
  h <- suppressWarnings(as.integer(hauteur_unites)[1])
  if (is.na(h) || !(h %in% c(1L, 2L, 3L, 4L))) {
    h <- switch(type_chr,
      "graphique" = if (isTRUE(graphique_long)) HAUTEUR_GRAPHIQUE_LARGE else HAUTEUR_GRAPHIQUE,
      "texte"     = HAUTEUR_TEXTE,
      "kpi"       = {
        if (tbl_long)        HAUTEUR_TABLEAU_LONG
        else if (tbl_etendu) HAUTEUR_TABLEAU_ETENDU
        else                 HAUTEUR_KPI
      },
      HAUTEUR_KPI
    )
  }

  list(
    id = generer_id("element"),
    type = type_chr,
    id_kpi = if (!is.null(id_kpi)) as.character(id_kpi)[1] else NULL,
    id_texte = if (!is.null(id_texte)) as.character(id_texte)[1] else NULL,
    contenu = if (!is.null(contenu)) as.character(contenu)[1] else NULL,
    style = if (!is.null(style)) as.character(style)[1] else "normal",
    taille = largeur,
    hauteur_unites = as.integer(h),
    tableau_long = tbl_long,
    tableau_etendu = tbl_etendu,
    graphique_long = isTRUE(graphique_long),
    nom_affiche = if (!is.null(nom_affiche)) as.character(nom_affiche)[1] else NULL,
    commentaire = if (!is.null(commentaire)) as.character(commentaire)[1] else NULL,
    couleur_forcee = if (!is.null(couleur_forcee)) as.character(couleur_forcee)[1] else NULL,
    niveau_titre = niveau_val
  )
}

# ==============================================================================
# Pages
# ==============================================================================

#' @export
ajouter_page <- function(dashboard, page) {
  if (is.null(dashboard) || !is.list(dashboard)) dashboard <- creer_dashboard()
  if (is.null(dashboard$pages)) dashboard$pages <- list()
  dashboard$pages[[length(dashboard$pages) + 1]] <- page
  dashboard
}

#' @export
supprimer_page <- function(dashboard, id_page) {
  if (is.null(dashboard$pages) || length(dashboard$pages) == 0) return(dashboard)
  idx <- which(sapply(dashboard$pages, function(p) identical(p$id, id_page)))
  if (length(idx) == 0) return(dashboard)
  dashboard$pages <- dashboard$pages[-idx]
  dashboard
}

#' @export
renommer_page <- function(dashboard, id_page, nouveau_titre) {
  if (is.null(dashboard$pages) || length(dashboard$pages) == 0) return(dashboard)
  idx <- which(sapply(dashboard$pages, function(p) identical(p$id, id_page)))
  if (length(idx) == 0) return(dashboard)
  dashboard$pages[[idx[1]]]$titre <- as.character(nouveau_titre)
  dashboard
}

#' @export
obtenir_page <- function(dashboard, id_page) {
  if (is.null(dashboard) || is.null(dashboard$pages)) return(NULL)
  idx <- which(sapply(dashboard$pages, function(p) identical(p$id, id_page)))
  if (length(idx) == 0) return(NULL)
  dashboard$pages[[idx[1]]]
}

#' @export
page_existe <- function(dashboard, id_page) {
  if (is.null(dashboard) || is.null(dashboard$pages)) return(FALSE)
  any(sapply(dashboard$pages, function(p) identical(p$id, id_page)))
}

#' @export
deplacer_page <- function(dashboard, id_page, direction) {
  if (is.null(dashboard$pages) || length(dashboard$pages) <= 1) return(dashboard)
  idx <- which(sapply(dashboard$pages, function(p) identical(p$id, id_page)))
  if (length(idx) == 0) return(dashboard)
  pos <- idx[1]
  if (direction == "gauche" && pos > 1) {
    tmp <- dashboard$pages[[pos - 1]]
    dashboard$pages[[pos - 1]] <- dashboard$pages[[pos]]
    dashboard$pages[[pos]] <- tmp
  } else if (direction == "droite" && pos < length(dashboard$pages)) {
    tmp <- dashboard$pages[[pos + 1]]
    dashboard$pages[[pos + 1]] <- dashboard$pages[[pos]]
    dashboard$pages[[pos]] <- tmp
  }
  dashboard
}

# ==============================================================================
# Lignes
# ==============================================================================

#' @export
ajouter_ligne <- function(dashboard, id_page, ligne = NULL) {
  if (is.null(dashboard$pages)) return(dashboard)
  idx <- which(sapply(dashboard$pages, function(p) identical(p$id, id_page)))
  if (length(idx) == 0) return(dashboard)
  if (is.null(ligne)) ligne <- creer_ligne()
  if (is.null(dashboard$pages[[idx[1]]]$lignes)) {
    dashboard$pages[[idx[1]]]$lignes <- list()
  }
  dashboard$pages[[idx[1]]]$lignes[[length(dashboard$pages[[idx[1]]]$lignes) + 1]] <- ligne
  dashboard
}

#' @export
supprimer_ligne <- function(dashboard, id_page, id_ligne) {
  if (is.null(dashboard$pages)) return(dashboard)
  idx_p <- which(sapply(dashboard$pages, function(p) identical(p$id, id_page)))
  if (length(idx_p) == 0) return(dashboard)
  lignes <- dashboard$pages[[idx_p[1]]]$lignes
  if (is.null(lignes) || length(lignes) == 0) return(dashboard)
  idx_l <- which(sapply(lignes, function(l) identical(l$id, id_ligne)))
  if (length(idx_l) == 0) return(dashboard)
  dashboard$pages[[idx_p[1]]]$lignes <- lignes[-idx_l]
  dashboard
}

#' @export
deplacer_ligne <- function(dashboard, id_page, id_ligne, direction) {
  if (is.null(dashboard$pages)) return(dashboard)
  idx_p <- which(sapply(dashboard$pages, function(p) identical(p$id, id_page)))
  if (length(idx_p) == 0) return(dashboard)
  lignes <- dashboard$pages[[idx_p[1]]]$lignes
  if (is.null(lignes) || length(lignes) <= 1) return(dashboard)
  idx_l <- which(sapply(lignes, function(l) identical(l$id, id_ligne)))
  if (length(idx_l) == 0) return(dashboard)
  pos <- idx_l[1]
  if (direction == "haut" && pos > 1) {
    tmp <- lignes[[pos - 1]]; lignes[[pos - 1]] <- lignes[[pos]]; lignes[[pos]] <- tmp
  } else if (direction == "bas" && pos < length(lignes)) {
    tmp <- lignes[[pos + 1]]; lignes[[pos + 1]] <- lignes[[pos]]; lignes[[pos]] <- tmp
  }
  dashboard$pages[[idx_p[1]]]$lignes <- lignes
  dashboard
}

#' @export
obtenir_ligne <- function(dashboard, id_page, id_ligne) {
  page <- obtenir_page(dashboard, id_page)
  if (is.null(page) || is.null(page$lignes)) return(NULL)
  idx <- which(sapply(page$lignes, function(l) identical(l$id, id_ligne)))
  if (length(idx) == 0) return(NULL)
  page$lignes[[idx[1]]]
}

# ==============================================================================
# Calculs de remplissage
# ==============================================================================

#' @export
largeur_utilisee_page <- function(dashboard, id_page) {
  page <- obtenir_page(dashboard, id_page)
  if (is.null(page) || is.null(page$lignes) || length(page$lignes) == 0) return(0L)
  rboard_max_largeurs_lignes(page$lignes)
}

#' @export
hauteur_utilisee_page <- function(dashboard, id_page) {
  page <- obtenir_page(dashboard, id_page)
  if (is.null(page) || is.null(page$lignes) || length(page$lignes) == 0) return(0L)
  rboard_somme_hauteurs_lignes(page$lignes)
}

#' @export
hauteur_ligne <- function(ligne, projet = NULL) {
  if (is.null(ligne) || is.null(ligne$elements) || length(ligne$elements) == 0) return(0L)
  rboard_max_hauteurs(ligne$elements)
}

# ==============================================================================
# Elements
# ==============================================================================

#' @export
verifier_placement <- function(dashboard, id_page, id_ligne, element) {
  page <- obtenir_page(dashboard, id_page)
  if (is.null(page)) stop("Page introuvable.", call. = FALSE)
  ligne <- obtenir_ligne(dashboard, id_page, id_ligne)
  if (is.null(ligne)) stop("Ligne introuvable.", call. = FALSE)

  largeur_el <- as.integer(element$taille)
  hauteur_el <- as.integer(element$hauteur_unites %||% 1L)

  if (largeur_el > RBOARD_GRILLE_LARGEUR) {
    stop(sprintf("Largeur %d depasse la grille (%d colonnes max).",
                 largeur_el, RBOARD_GRILLE_LARGEUR), call. = FALSE)
  }

  largeur_ligne <- rboard_somme_largeurs(ligne$elements)
  if (largeur_ligne + largeur_el > RBOARD_GRILLE_LARGEUR) {
    stop(sprintf("Ligne pleine : %d + %d > %d colonnes.",
                 largeur_ligne, largeur_el, RBOARD_GRILLE_LARGEUR), call. = FALSE)
  }

  h_ligne_actuelle <- rboard_max_hauteurs(ligne$elements)
  h_page_actuelle <- hauteur_utilisee_page(dashboard, id_page)
  h_sans_ligne <- h_page_actuelle - h_ligne_actuelle
  h_nouvelle <- max(h_ligne_actuelle, hauteur_el)

  if (h_sans_ligne + h_nouvelle > RBOARD_GRILLE_HAUTEUR) {
    stop(sprintf("Page pleine : %d unites + %d > %d unites max.",
                 h_sans_ligne, h_nouvelle, RBOARD_GRILLE_HAUTEUR), call. = FALSE)
  }
  TRUE
}

#' @export
ajouter_element <- function(dashboard, id_page, id_ligne, element) {
  if (is.null(dashboard$pages)) return(dashboard)
  idx_p <- which(sapply(dashboard$pages, function(p) identical(p$id, id_page)))
  if (length(idx_p) == 0) return(dashboard)
  lignes <- dashboard$pages[[idx_p[1]]]$lignes
  if (is.null(lignes) || length(lignes) == 0) return(dashboard)
  idx_l <- which(sapply(lignes, function(l) identical(l$id, id_ligne)))
  if (length(idx_l) == 0) return(dashboard)

  elems_actuels <- dashboard$pages[[idx_p[1]]]$lignes[[idx_l[1]]]$elements
  largeur_actuelle <- rboard_somme_largeurs(elems_actuels)
  h_ligne_actuelle <- rboard_max_hauteurs(elems_actuels)

  nouvelle_largeur <- as.integer(element$taille)
  nouvelle_hauteur <- as.integer(element$hauteur_unites %||% 1L)

  if (largeur_actuelle + nouvelle_largeur > RBOARD_GRILLE_LARGEUR) {
    stop(sprintf("Ligne pleine : %d + %d > %d colonnes.",
                 largeur_actuelle, nouvelle_largeur, RBOARD_GRILLE_LARGEUR),
         call. = FALSE)
  }

  h_page_actuelle <- hauteur_utilisee_page(dashboard, id_page)
  h_nouvelle_ligne <- max(h_ligne_actuelle, nouvelle_hauteur)
  h_page_sans_ligne <- h_page_actuelle - h_ligne_actuelle

  if (h_page_sans_ligne + h_nouvelle_ligne > RBOARD_GRILLE_HAUTEUR) {
    stop(sprintf(
      "Page pleine : %d unites utilisees + %d nouvelles > %d unites max. Creez une nouvelle page.",
      h_page_sans_ligne, h_nouvelle_ligne, RBOARD_GRILLE_HAUTEUR), call. = FALSE)
  }

  if (is.null(dashboard$pages[[idx_p[1]]]$lignes[[idx_l[1]]]$elements)) {
    dashboard$pages[[idx_p[1]]]$lignes[[idx_l[1]]]$elements <- list()
  }
  dashboard$pages[[idx_p[1]]]$lignes[[idx_l[1]]]$elements <-
    c(dashboard$pages[[idx_p[1]]]$lignes[[idx_l[1]]]$elements, list(element))
  dashboard
}

#' @export
supprimer_element <- function(dashboard, id_page, id_ligne, id_element) {
  if (is.null(dashboard$pages)) return(dashboard)
  idx_p <- which(sapply(dashboard$pages, function(p) identical(p$id, id_page)))
  if (length(idx_p) == 0) return(dashboard)
  lignes <- dashboard$pages[[idx_p[1]]]$lignes
  if (is.null(lignes) || length(lignes) == 0) return(dashboard)
  idx_l <- which(sapply(lignes, function(l) identical(l$id, id_ligne)))
  if (length(idx_l) == 0) return(dashboard)
  elems <- dashboard$pages[[idx_p[1]]]$lignes[[idx_l[1]]]$elements
  if (is.null(elems) || length(elems) == 0) return(dashboard)
  idx_e <- which(sapply(elems, function(e) identical(e$id, id_element)))
  if (length(idx_e) == 0) return(dashboard)
  dashboard$pages[[idx_p[1]]]$lignes[[idx_l[1]]]$elements <- elems[-idx_e]
  dashboard
}

#' @export
modifier_element <- function(dashboard, id_page, id_ligne, id_element, nouvelles_valeurs) {
  if (is.null(dashboard$pages) || length(nouvelles_valeurs) == 0) return(dashboard)
  idx_p <- which(sapply(dashboard$pages, function(p) identical(p$id, id_page)))
  if (length(idx_p) == 0) return(dashboard)
  lignes <- dashboard$pages[[idx_p[1]]]$lignes
  if (is.null(lignes) || length(lignes) == 0) return(dashboard)
  idx_l <- which(sapply(lignes, function(l) identical(l$id, id_ligne)))
  if (length(idx_l) == 0) return(dashboard)
  elems <- dashboard$pages[[idx_p[1]]]$lignes[[idx_l[1]]]$elements
  if (is.null(elems) || length(elems) == 0) return(dashboard)
  idx_e <- which(sapply(elems, function(e) identical(e$id, id_element)))
  if (length(idx_e) == 0) return(dashboard)

  elem_actuel <- elems[[idx_e[1]]]
  autres <- elems[-idx_e]

  verifier_faisabilite <- function(nv_taille, nv_hauteur) {
    l_autres <- rboard_somme_largeurs(autres)
    if (l_autres + nv_taille > RBOARD_GRILLE_LARGEUR) {
      stop(sprintf(
        "Impossible : la ligne contiendrait %d colonnes (max %d). Reductionnez la largeur ou retirez un element.",
        l_autres + nv_taille, RBOARD_GRILLE_LARGEUR), call. = FALSE)
    }

    h_page_actuelle <- hauteur_utilisee_page(dashboard, id_page)
    h_ligne_actuelle <- rboard_max_hauteurs(elems)
    h_autres_ligne <- rboard_max_hauteurs(autres)
    h_ligne_nouvelle <- max(h_autres_ligne, as.integer(nv_hauteur))
    h_page_sans_ligne <- h_page_actuelle - h_ligne_actuelle
    h_page_nouvelle <- h_page_sans_ligne + h_ligne_nouvelle

    if (h_page_nouvelle > RBOARD_GRILLE_HAUTEUR) {
      stop(sprintf(
        "Impossible : la page ferait %d / %d unites. Il n'y a pas assez d'espace sur cette page.",
        h_page_nouvelle, RBOARD_GRILLE_HAUTEUR), call. = FALSE)
    }
    TRUE
  }

  nv_taille <- rboard_largeur_element(elem_actuel)
  nv_hauteur <- rboard_hauteur_el(elem_actuel)

  # --- Bascule tableau_long ---
  if ("tableau_long" %in% names(nouvelles_valeurs)) {
    tbl_long <- isTRUE(nouvelles_valeurs$tableau_long)
    if (tbl_long) {
      # Tableau long : largeur 6 ou 12, hauteur 3
      if (!(nv_taille %in% c(6L, 12L))) nv_taille <- 12L
      nv_hauteur <- HAUTEUR_TABLEAU_LONG
      nouvelles_valeurs$tableau_etendu <- FALSE
    } else {
      nv_hauteur <- HAUTEUR_TABLEAU
      if (identical(nv_taille, 12L)) nv_taille <- 6L
    }
    nouvelles_valeurs$tableau_long <- tbl_long
  }

  # --- Bascule tableau_etendu ---
  if ("tableau_etendu" %in% names(nouvelles_valeurs)) {
    tbl_etendu <- isTRUE(nouvelles_valeurs$tableau_etendu)
    if (tbl_etendu) {
      nv_taille <- 6L
      nv_hauteur <- HAUTEUR_TABLEAU_ETENDU
      nouvelles_valeurs$tableau_long <- FALSE
    } else {
      if (isTRUE(elem_actuel$tableau_etendu)) {
        nv_hauteur <- HAUTEUR_TABLEAU
      }
    }
    nouvelles_valeurs$tableau_etendu <- tbl_etendu
  }

  # --- Bascule graphique_long ---
  if ("graphique_long" %in% names(nouvelles_valeurs)) {
    gl <- isTRUE(nouvelles_valeurs$graphique_long)
    nv_hauteur <- if (gl) HAUTEUR_GRAPHIQUE_LARGE else HAUTEUR_GRAPHIQUE
    nouvelles_valeurs$graphique_long <- gl
  }

  # --- Largeur explicite ---
  if ("taille" %in% names(nouvelles_valeurs)) {
    nl <- suppressWarnings(as.integer(nouvelles_valeurs$taille)[1])
    if (is.na(nl) || !(nl %in% c(3L, 6L, 12L))) {
      stop("Largeur invalide. Doit valoir 3, 6 ou 12.", call. = FALSE)
    }
    nv_taille <- nl
  }

  # --- Coherence finale tableau_long + largeur ---
  tbl_long_final <- if ("tableau_long" %in% names(nouvelles_valeurs)) {
    isTRUE(nouvelles_valeurs$tableau_long)
  } else {
    isTRUE(elem_actuel$tableau_long)
  }
  if (tbl_long_final && !(nv_taille %in% c(6L, 12L))) {
    nv_taille <- 12L
  }

  # --- Hauteur explicite ---
  if ("hauteur_unites" %in% names(nouvelles_valeurs)) {
    nh <- suppressWarnings(as.integer(nouvelles_valeurs$hauteur_unites)[1])
    if (is.na(nh) || !(nh %in% c(1L, 2L, 3L, 4L))) {
      stop("Hauteur invalide. Doit valoir 1, 2, 3 ou 4 unites.", call. = FALSE)
    }
    nv_hauteur <- nh
  }

  verifier_faisabilite(nv_taille, nv_hauteur)

  # --- Niveau titre ---
  if ("niveau_titre" %in% names(nouvelles_valeurs)) {
    nt <- nouvelles_valeurs$niveau_titre
    if (is.null(nt)) {
      nouvelles_valeurs$niveau_titre <- NULL
    } else {
      nt_num <- suppressWarnings(as.integer(nt)[1])
      if (is.na(nt_num) || !(nt_num %in% c(1L, 2L, 3L))) nt_num <- 2L
      nouvelles_valeurs$niveau_titre <- nt_num
    }
  }

  nouvelles_valeurs$taille <- nv_taille
  nouvelles_valeurs$hauteur_unites <- nv_hauteur

  elem <- elem_actuel
  for (nm in names(nouvelles_valeurs)) {
    if (is.null(nouvelles_valeurs[[nm]])) {
      elem[[nm]] <- NULL
    } else {
      elem[[nm]] <- nouvelles_valeurs[[nm]]
    }
  }

  dashboard$pages[[idx_p[1]]]$lignes[[idx_l[1]]]$elements[[idx_e[1]]] <- elem
  dashboard
}

#' @export
deplacer_element <- function(dashboard, id_page, id_ligne, id_element, direction) {
  if (is.null(dashboard$pages)) return(dashboard)
  idx_p <- which(sapply(dashboard$pages, function(p) identical(p$id, id_page)))
  if (length(idx_p) == 0) return(dashboard)
  lignes <- dashboard$pages[[idx_p[1]]]$lignes
  if (is.null(lignes)) return(dashboard)
  idx_l <- which(sapply(lignes, function(l) identical(l$id, id_ligne)))
  if (length(idx_l) == 0) return(dashboard)
  elems <- dashboard$pages[[idx_p[1]]]$lignes[[idx_l[1]]]$elements
  if (is.null(elems) || length(elems) <= 1) return(dashboard)
  idx_e <- which(sapply(elems, function(e) identical(e$id, id_element)))
  if (length(idx_e) == 0) return(dashboard)
  pos <- idx_e[1]
  if (direction == "gauche" && pos > 1) {
    tmp <- elems[[pos - 1]]; elems[[pos - 1]] <- elems[[pos]]; elems[[pos]] <- tmp
  } else if (direction == "droite" && pos < length(elems)) {
    tmp <- elems[[pos + 1]]; elems[[pos + 1]] <- elems[[pos]]; elems[[pos]] <- tmp
  }
  dashboard$pages[[idx_p[1]]]$lignes[[idx_l[1]]]$elements <- elems
  dashboard
}

#' @export
resumer_dashboard <- function(dashboard) {
  if (is.null(dashboard) || is.null(dashboard$pages) || length(dashboard$pages) == 0) {
    return(data.frame(id = character(0), titre = character(0),
                      nb_lignes = integer(0), nb_elements = integer(0),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    id = sapply(dashboard$pages, function(p) p$id),
    titre = sapply(dashboard$pages, function(p) p$titre),
    nb_lignes = sapply(dashboard$pages, function(p) {
      if (is.null(p$lignes)) 0L else as.integer(length(p$lignes))
    }),
    nb_elements = sapply(dashboard$pages, function(p) {
      if (is.null(p$lignes)) return(0L)
      as.integer(sum(sapply(p$lignes, function(l) {
        if (is.null(l$elements)) 0L else length(l$elements)
      })))
    }),
    stringsAsFactors = FALSE
  )
}