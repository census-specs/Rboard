#' ==============================================================================
#' Rboard - Moteur de base des indicateurs statistiques (KPI)
#' ==============================================================================

#' Creer un indicateur statistique (KPI) simple
#' @export
creer_kpi <- function(nom, valeur, type = "generique", label = NULL,
                      commentaire = NULL, source = "manuel",
                      format = "decimal_3", groupe = NULL,
                      couleur_manuelle = NULL, regles_couleur = list()) {
  id_kpi <- generer_id("kpi")

  if (is.null(label)) {
    label <- paste0(nom, " = ", formater_valeur(valeur, format))
  }

  kpi_temp <- list(
    id = id_kpi, nom = nom, valeur = valeur, label = label, type = type,
    couleur_manuelle = couleur_manuelle,
    regles_couleur = if (is.null(regles_couleur)) list() else regles_couleur,
    commentaire = commentaire, source = source, format = format,
    groupe = groupe, date_creation = Sys.Date()
  )
  kpi_temp$couleur <- decider_couleur(kpi_temp)

  list(
    id = kpi_temp$id, nom = kpi_temp$nom, valeur = kpi_temp$valeur,
    label = kpi_temp$label, type = kpi_temp$type, couleur = kpi_temp$couleur,
    couleur_manuelle = kpi_temp$couleur_manuelle,
    regles_couleur = kpi_temp$regles_couleur,
    commentaire = kpi_temp$commentaire, source = kpi_temp$source,
    format = kpi_temp$format, groupe = kpi_temp$groupe,
    date_creation = kpi_temp$date_creation
  )
}

#' Creer un KPI de type tableau
#'
#' @param nom Caractere. Nom du KPI.
#' @param tableau data.frame. Donnees du tableau (precalculees).
#' @param sous_type Caractere. "par_groupe" ou "croise".
#' @param mode_affichage Caractere. Pour "croise" : "effectifs",
#'   "pourcent_total", "pourcent_ligne", "pourcent_colonne".
#' @param stats_annexes Liste optionnelle. Ex : list(p_value = 0.03, v_cramer = 0.2).
#' @param commentaire Caractere ou NULL.
#' @param regles_couleur Liste. Regles de coloration.
#' @param colonne_valeur_index Entier. Index de colonne a colorer.
#'
#' @return Un objet KPI de type "tableau".
#' @export
creer_kpi_tableau <- function(nom, tableau, sous_type = "par_groupe",
                               mode_affichage = "effectifs",
                               stats_annexes = list(),
                               commentaire = NULL,
                               regles_couleur = list(),
                               colonne_valeur_index = 2) {
  id_kpi <- generer_id("tableau")

  kpi_temp <- list(
    id = id_kpi,
    nom = nom,
    type = "tableau",
    sous_type = sous_type,
    mode_affichage = mode_affichage,
    tableau = tableau,
    colonne_valeur_index = colonne_valeur_index,
    stats_annexes = stats_annexes,
    commentaire = commentaire,
    couleur = "primary",
    couleur_manuelle = NULL,
    regles_couleur = if (is.null(regles_couleur)) list() else regles_couleur,
    source = "tableau",
    format = "texte",
    groupe = NULL,
    date_creation = Sys.Date()
  )

  kpi_temp
}

#' Appliquer une regle conditionnelle a une valeur
#' @export
appliquer_regle <- function(valeur, regle) {
  if (is.null(regle) || !is.list(regle) || is.null(regle$signe) || is.null(regle$valeur)) {
    return(FALSE)
  }

  signe <- as.character(regle$signe)
  val_num <- suppressWarnings(as.numeric(valeur))
  seuil_num <- suppressWarnings(as.numeric(regle$valeur))

  if (!is.na(val_num) && !is.na(seuil_num)) {
    return(switch(
      signe,
      "<"  = val_num < seuil_num,
      "<=" = val_num <= seuil_num,
      ">"  = val_num > seuil_num,
      ">=" = val_num >= seuil_num,
      "="  = ,
      "==" = val_num == seuil_num,
      FALSE
    ))
  }

  v_str <- as.character(valeur)
  s_str <- as.character(regle$valeur)
  switch(signe, "=" = , "==" = v_str == s_str, FALSE)
}

#' Decider de la couleur d'un KPI
#' @export
decider_couleur <- function(kpi) {
  if (!is.null(kpi$couleur_manuelle) && nzchar(as.character(kpi$couleur_manuelle))) {
    return(as.character(kpi$couleur_manuelle))
  }

  if (!is.null(kpi$regles_couleur) && is.list(kpi$regles_couleur) && length(kpi$regles_couleur) > 0) {
    if (!is.null(kpi$valeur)) {
      for (r in kpi$regles_couleur) {
        if (isTRUE(appliquer_regle(kpi$valeur, r))) {
          if (!is.null(r$couleur) && nzchar(as.character(r$couleur))) {
            return(as.character(r$couleur))
          }
        }
      }
    }
  }

  val_num <- suppressWarnings(as.numeric(kpi$valeur))

  switch(
    kpi$type,
    "p_value" = {
      if (is.na(val_num)) "secondary"
      else if (val_num < 0.05) "success"
      else if (val_num < 0.10) "warning"
      else "danger"
    },
    "d_cohen" = {
      if (is.na(val_num)) "secondary"
      else if (abs(val_num) >= 0.8) "success"
      else if (abs(val_num) >= 0.5) "info"
      else if (abs(val_num) >= 0.2) "warning"
      else "secondary"
    },
    "correlation" = {
      if (is.na(val_num)) "secondary"
      else if (abs(val_num) >= 0.7) "success"
      else if (abs(val_num) >= 0.4) "info"
      else "secondary"
    },
    "primary"
  )
}

#' Ajouter un KPI a un projet
#' @export
ajouter_kpi <- function(projet, kpi) {
  if (!is.list(projet$kpi)) projet$kpi <- list()
  projet$kpi[[kpi$id]] <- kpi
  projet$date_modification <- Sys.Date()
  projet
}

#' Supprimer un KPI d'un projet
#' @export
supprimer_kpi <- function(projet, id) {
  if (is.list(projet$kpi) && !is.null(projet$kpi[[id]])) {
    projet$kpi[[id]] <- NULL
    projet$date_modification <- Sys.Date()
  }
  projet
}