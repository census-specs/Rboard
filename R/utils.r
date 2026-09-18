# ==============================================================================
# Rboard - Fonctions utilitaires de base
# ==============================================================================

#' Operateur de repli (null-coalescing)
#' @keywords internal
#' @export
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 ||
      (is.character(a) && length(a) == 1 && is.na(a))) {
    b
  } else {
    a
  }
}

#' Generer un identifiant unique court
#' @export
generer_id <- function(prefixe = "id", projet = NULL) {
  lettre <- switch(
    tolower(as.character(prefixe)),
    "kpi"       = "K",
    "tableau"   = "T",
    "graphique" = "G",
    "page"      = "P",
    "ligne"     = "L",
    "element"   = "E",
    "elem"      = "E",
    {
      p_clean <- gsub("[^a-zA-Z]", "", as.character(prefixe))
      if (nzchar(p_clean)) toupper(substr(p_clean, 1, 1)) else "X"
    }
  )
  if (!nzchar(lettre)) lettre <- "X"

  cars <- c("2","3","4","5","6","7","8","9",
            "A","B","C","D","E","F","G","H","J","K",
            "M","N","P","Q","R","S","T","U","V","W","X","Y","Z")

  repeat {
    suffixe <- paste0(sample(cars, 3, replace = TRUE), collapse = "")
    id <- paste0(lettre, suffixe)
    if (is.null(projet) || !id_existe_dans_projet(id, projet)) break
  }

  id
}

#' Verifie si un ID existe deja dans un projet
#' @keywords internal
id_existe_dans_projet <- function(id, projet) {
  if (is.null(projet) || !is.list(projet)) return(FALSE)

  ids <- character(0)
  if (!is.null(projet$kpi) && is.list(projet$kpi))
    ids <- c(ids, names(projet$kpi))
  if (!is.null(projet$graphiques) && is.list(projet$graphiques))
    ids <- c(ids, names(projet$graphiques))
  if (!is.null(projet$dashboard) && !is.null(projet$dashboard$pages)) {
    for (pg in projet$dashboard$pages) {
      if (!is.null(pg$id)) ids <- c(ids, pg$id)
      for (lg in pg$lignes %||% list()) {
        if (!is.null(lg$id)) ids <- c(ids, lg$id)
        for (el in lg$elements %||% list()) {
          if (!is.null(el$id)) ids <- c(ids, el$id)
        }
      }
    }
  }

  id %in% ids
}

#' Formater une valeur selon un format statistique
#' @export
formater_valeur <- function(valeur, format = "decimal_3") {
  if (is.null(valeur) || length(valeur) == 0) return("")
  if (is.na(valeur)) return(NA_character_)

  switch(
    format,
    "decimal_3" = {
      v <- suppressWarnings(as.numeric(valeur))
      if (is.na(v)) return(as.character(valeur))
      formatC(v, format = "f", digits = 3, decimal.mark = ",")
    },
    "decimal_2" = {
      v <- suppressWarnings(as.numeric(valeur))
      if (is.na(v)) return(as.character(valeur))
      formatC(v, format = "f", digits = 2, decimal.mark = ",")
    },
    "pourcent" = {
      v <- suppressWarnings(as.numeric(valeur))
      if (is.na(v)) return(as.character(valeur))
      paste0(formatC(v * 100, format = "f", digits = 1, decimal.mark = ","), " %")
    },
    "entier" = {
      v <- suppressWarnings(as.numeric(valeur))
      if (is.na(v)) return(as.character(valeur))
      formatC(round(v), format = "d")
    },
    "texte" = as.character(valeur),
    as.character(valeur)
  )
}

# ==============================================================================
# Helpers robustes pour manipuler les elements du dashboard
# ==============================================================================

#' Largeur (colonne) d'un element, avec fallback sur 6
#' @keywords internal
rboard_largeur_element <- function(e) {
  if (is.null(e) || !is.list(e)) return(6L)
  t <- suppressWarnings(as.integer(e$taille)[1])
  if (is.na(t) || !(t %in% c(3L, 6L, 12L))) return(6L)
  as.integer(t)
}

#' Hauteur (unites) d'un element, avec fallback sur 1
#' @keywords internal
rboard_hauteur_el <- function(e) {
  if (is.null(e) || !is.list(e)) return(1L)
  h <- suppressWarnings(as.integer(e$hauteur_unites)[1])
  if (is.na(h) || !(h %in% c(1L, 2L, 3L, 4L))) return(1L)
  as.integer(h)
}

#' Somme securisee des largeurs d'une liste d'elements
#' @keywords internal
rboard_somme_largeurs <- function(elements) {
  if (is.null(elements) || length(elements) == 0) return(0L)
  tailles <- vapply(elements, rboard_largeur_element, integer(1))
  as.integer(sum(tailles))
}

#' Max securise des hauteurs d'une liste d'elements
#' @keywords internal
rboard_max_hauteurs <- function(elements) {
  if (is.null(elements) || length(elements) == 0) return(0L)
  hs <- vapply(elements, rboard_hauteur_el, integer(1))
  as.integer(max(hs))
}

#' Max securise des hauteurs d'une liste de lignes
#' @keywords internal
rboard_max_hauteurs_lignes <- function(lignes) {
  if (is.null(lignes) || length(lignes) == 0) return(0L)
  hs <- vapply(lignes, function(l) rboard_max_hauteurs(l$elements), integer(1))
  as.integer(max(c(0L, hs)))
}

#' Somme securisee des hauteurs d'une liste de lignes
#' (utilise pour la hauteur totale d'une page)
#' @keywords internal
rboard_somme_hauteurs_lignes <- function(lignes) {
  if (is.null(lignes) || length(lignes) == 0) return(0L)
  hs <- vapply(lignes, function(l) rboard_max_hauteurs(l$elements), integer(1))
  as.integer(sum(hs))
}

#' Max securise des largeurs d'une liste de lignes
#' @keywords internal
rboard_max_largeurs_lignes <- function(lignes) {
  if (is.null(lignes) || length(lignes) == 0) return(0L)
  ls <- vapply(lignes, function(l) rboard_somme_largeurs(l$elements), integer(1))
  as.integer(max(c(0L, ls)))
}

# ==============================================================================
# Helpers d'erreur / succes
# ==============================================================================

#' Construire un message de succes
#' @export
rboard_ok <- function(texte) {
  list(type = "success", texte = as.character(texte)[1])
}

#' Construire un message d'avertissement
#' @export
rboard_warn <- function(texte) {
  list(type = "warning", texte = as.character(texte)[1])
}

#' Construire un message d'erreur
#' @export
rboard_ko <- function(texte) {
  list(type = "danger", texte = as.character(texte)[1])
}

#' Construire un message d'information
#' @export
rboard_info <- function(texte) {
  list(type = "info", texte = as.character(texte)[1])
}

#' Executer une expression en capturant proprement les erreurs
#' @export
rboard_try <- function(expr, contexte = "operation") {
  tryCatch(
    list(ok = TRUE, valeur = expr, message = NULL),
    error = function(e) {
      list(
        ok = FALSE,
        valeur = NULL,
        message = rboard_ko(sprintf("Erreur lors de %s : %s",
                                    contexte, conditionMessage(e)))
      )
    },
    warning = function(w) {
      list(
        ok = TRUE,
        valeur = suppressWarnings(expr),
        message = rboard_warn(sprintf("Avertissement lors de %s : %s",
                                      contexte, conditionMessage(w)))
      )
    }
  )
}