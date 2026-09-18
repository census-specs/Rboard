# ==============================================================================
# Rboard - Validations des entrees
# ==============================================================================

#' Verifier qu'une colonne existe dans un data.frame
#'
#' @param donnees Data.frame.
#' @param colonne Caractere. Nom de la colonne.
#' @param contexte Caractere. Contexte de l'appel (pour le message).
#' @return TRUE si valide, sinon stop().
#' @export
valider_colonne <- function(donnees, colonne, contexte = "analyse") {
  if (is.null(donnees)) {
    stop(sprintf("Aucun jeu de donnees pour %s.", contexte), call. = FALSE)
  }
  if (!is.data.frame(donnees)) {
    stop(sprintf("Les donnees pour %s ne sont pas un data.frame.", contexte),
         call. = FALSE)
  }
  if (is.null(colonne) || !nzchar(colonne)) {
    stop(sprintf("Aucune variable specifiee pour %s.", contexte), call. = FALSE)
  }
  if (!colonne %in% names(donnees)) {
    stop(sprintf("La variable '%s' n'existe pas dans les donnees.", colonne),
         call. = FALSE)
  }
  TRUE
}

#' Verifier qu'une variable numerique contient au moins une valeur
#'
#' @param vec Vecteur a verifier.
#' @param nom Caractere. Nom de la variable.
#' @return TRUE si valide, sinon stop().
#' @export
valider_numerique <- function(vec, nom = "variable") {
  if (!is.numeric(vec)) {
    stop(sprintf("La variable '%s' n'est pas numerique.", nom), call. = FALSE)
  }
  if (all(is.na(vec))) {
    stop(sprintf("La variable '%s' ne contient aucune valeur exploitable.", nom),
         call. = FALSE)
  }
  TRUE
}

#' Verifier qu'une variable de groupe a un nombre de modalites acceptable
#'
#' @param vec Vecteur.
#' @param nom Caractere.
#' @param min_modalites Entier.
#' @param max_modalites Entier ou NULL.
#' @return TRUE si valide, sinon stop().
#' @export
valider_groupe <- function(vec, nom = "groupe", min_modalites = 2,
                            max_modalites = NULL) {
  modalites <- unique(vec[!is.na(vec)])
  n <- length(modalites)

  if (n < min_modalites) {
    stop(sprintf("La variable '%s' doit contenir au moins %d modalites (trouvees : %d).",
                 nom, min_modalites, n), call. = FALSE)
  }
  if (!is.null(max_modalites) && n > max_modalites) {
    stop(sprintf("La variable '%s' doit contenir au maximum %d modalites (trouvees : %d).",
                 nom, max_modalites, n), call. = FALSE)
  }
  TRUE
}

#' Verifier qu'une valeur est un entier valide
#'
#' @param val Valeur a tester.
#' @param nom Caractere.
#' @param min Numerique.
#' @param max Numerique.
#' @return TRUE si valide, sinon stop().
#' @export
valider_entier <- function(val, nom = "valeur", min = NULL, max = NULL) {
  n <- suppressWarnings(as.numeric(val)[1])
  if (is.na(n)) {
    stop(sprintf("'%s' doit etre un nombre valide.", nom), call. = FALSE)
  }
  if (n != round(n)) {
    stop(sprintf("'%s' doit etre un entier.", nom), call. = FALSE)
  }
  if (!is.null(min) && n < min) {
    stop(sprintf("'%s' doit etre superieur ou egal a %s.", nom, min), call. = FALSE)
  }
  if (!is.null(max) && n > max) {
    stop(sprintf("'%s' doit etre inferieur ou egal a %s.", nom, max), call. = FALSE)
  }
  TRUE
}

#' Verifier qu'une chaine de caracteres n'est pas vide
#'
#' @param txt Caractere.
#' @param nom Caractere.
#' @return TRUE si valide, sinon stop().
#' @export
valider_texte <- function(txt, nom = "champ") {
  if (is.null(txt) || !is.character(txt) || !nzchar(trimws(txt))) {
    stop(sprintf("Le champ '%s' est obligatoire.", nom), call. = FALSE)
  }
  TRUE
}