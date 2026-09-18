# ==============================================================================
# Rboard - Import et diagnostic des donnees
# ==============================================================================

#' Importer un fichier de donnees
#'
#' Detecte l'extension et charge les donnees dans un data.frame.
#' Extensions supportees : .csv, .xlsx, .xls, .rds.
#'
#' @param chemin Caractere. Chemin d'acces au fichier.
#' @return Un data.frame.
#' @export
importer_fichier <- function(chemin) {
  if (!file.exists(chemin)) {
    stop(sprintf("Le fichier specifie n'existe pas : %s", chemin), call. = FALSE)
  }

  ext <- tolower(tools::file_ext(chemin))

  donnees <- switch(
    ext,
    "csv"  = read.csv(chemin, stringsAsFactors = FALSE),
    "xlsx" = ,
    "xls"  = as.data.frame(readxl::read_excel(chemin)),
    "rds"  = {
      obj <- readRDS(chemin)
      if (!is.data.frame(obj)) {
        stop("Le fichier .rds ne contient pas un data.frame.", call. = FALSE)
      }
      as.data.frame(obj)
    },
    stop(
      sprintf("Format de fichier non supporte : '.%s'. Formats acceptes : .csv, .xlsx, .xls, .rds.",
              ext),
      call. = FALSE
    )
  )

  donnees
}

#' Detecter le type fonctionnel d'une variable
#'
#' Identifie le type d'une colonne pour orienter les traitements.
#'
#' @param x Vecteur. La colonne a analyser.
#' @return Une chaine parmi : "numerique", "facteur", "date", "logique",
#'   "autre".
#' @export
detecter_type_variable <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXct")) {
    "date"
  } else if (is.numeric(x)) {
    "numerique"
  } else if (is.factor(x) || is.character(x)) {
    "facteur"
  } else if (is.logical(x)) {
    "logique"
  } else {
    "autre"
  }
}

#' Resumer les variables d'un jeu de donnees
#'
#' Calcule un tableau descriptif : une ligne par variable avec son type,
#' son nombre de valeurs manquantes, son nombre d'uniques et un exemple.
#'
#' @param donnees Data.frame. Le tableau a resumer.
#' @return Un data.frame avec les colonnes : variable, type, nb_na,
#'   nb_uniques, exemple.
#' @export
resumer_variables <- function(donnees) {
  valider_donnees(donnees)
  noms <- names(donnees)

  data.frame(
    variable = noms,
    type = sapply(donnees, detecter_type_variable),
    nb_na = sapply(donnees, function(x) sum(is.na(x))),
    nb_uniques = sapply(donnees, function(x) length(unique(x))),
    exemple = sapply(donnees, function(x) {
      v <- x[!is.na(x)]
      if (length(v) > 0) as.character(v[1]) else "-"
    }),
    stringsAsFactors = FALSE
  )
}

#' Valider la conformite d'un jeu de donnees
#'
#' Verifie que l'objet est un data.frame non vide (>= 1 ligne, >= 1 colonne).
#'
#' @param donnees Objet a verifier.
#' @return TRUE si valide, sinon stop() avec message explicite.
#' @export
valider_donnees <- function(donnees) {
  if (!is.data.frame(donnees)) {
    stop("L'objet fourni n'est pas un tableau de donnees (data.frame attendu).",
         call. = FALSE)
  }
  if (ncol(donnees) == 0) {
    stop("Le tableau de donnees ne contient aucune colonne.", call. = FALSE)
  }
  if (nrow(donnees) == 0) {
    stop("Le tableau de donnees ne contient aucune ligne (fichier vide).",
         call. = FALSE)
  }
  TRUE
}