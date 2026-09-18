# ==============================================================================
# Rboard - Moteur de calculs statistiques produisant des KPI
# ==============================================================================

# --- KPI descriptifs univaries ----------------------------------------------

#' KPI de moyenne
#'
#' Calcule la moyenne d'une variable numerique, globalement ou par
#' modalite d'une variable de groupe.
#'
#' @param donnees Data.frame. Le tableau de donnees.
#' @param variable Caractere. Nom de la colonne numerique.
#' @param groupe Caractere ou NULL. Nom d'une variable de groupe optionnelle.
#' @param regles_couleur Liste. Regles de coloration optionnelles.
#' @return Un KPI unique ou une liste de KPI (un par modalite).
#' @export
kpi_moyenne <- function(donnees, variable, groupe = NULL, regles_couleur = list()) {
  valider_colonne(donnees, variable, "la moyenne")
  valider_numerique(donnees[[variable]], variable)

  if (is.null(groupe) || !nzchar(groupe)) {
    val <- mean(donnees[[variable]], na.rm = TRUE)
    return(creer_kpi(
      nom = paste0("Moyenne (", variable, ")"),
      valeur = val, type = "moyenne", format = "decimal_2",
      source = "moyenne", regles_couleur = regles_couleur
    ))
  }

  valider_colonne(donnees, groupe, "la moyenne")
  valider_groupe(donnees[[groupe]], groupe, 2)

  modalites <- unique(donnees[[groupe]][!is.na(donnees[[groupe]])])
  kpis <- list()
  for (mod in modalites) {
    sous <- donnees[donnees[[groupe]] == mod, variable]
    val <- mean(sous, na.rm = TRUE)
    k <- creer_kpi(
      nom = paste0("Moyenne ", variable, " [", mod, "]"),
      valeur = val, type = "moyenne", format = "decimal_2",
      groupe = as.character(mod), source = "moyenne",
      regles_couleur = regles_couleur
    )
    kpis[[k$id]] <- k
  }
  kpis
}

#' KPI de mediane
#'
#' @param donnees Data.frame.
#' @param variable Caractere. Nom de la colonne numerique.
#' @param groupe Caractere ou NULL.
#' @param regles_couleur Liste.
#' @return Un KPI ou une liste de KPI.
#' @export
kpi_mediane <- function(donnees, variable, groupe = NULL, regles_couleur = list()) {
  valider_colonne(donnees, variable, "la mediane")
  valider_numerique(donnees[[variable]], variable)

  if (is.null(groupe) || !nzchar(groupe)) {
    val <- stats::median(donnees[[variable]], na.rm = TRUE)
    return(creer_kpi(
      nom = paste0("Mediane (", variable, ")"),
      valeur = val, type = "mediane", format = "decimal_2",
      source = "mediane", regles_couleur = regles_couleur
    ))
  }

  valider_colonne(donnees, groupe, "la mediane")
  valider_groupe(donnees[[groupe]], groupe, 2)

  modalites <- unique(donnees[[groupe]][!is.na(donnees[[groupe]])])
  kpis <- list()
  for (mod in modalites) {
    sous <- donnees[donnees[[groupe]] == mod, variable]
    val <- stats::median(sous, na.rm = TRUE)
    k <- creer_kpi(
      nom = paste0("Mediane ", variable, " [", mod, "]"),
      valeur = val, type = "mediane", format = "decimal_2",
      groupe = as.character(mod), source = "mediane",
      regles_couleur = regles_couleur
    )
    kpis[[k$id]] <- k
  }
  kpis
}

#' KPI d'ecart-type
#'
#' @param donnees Data.frame.
#' @param variable Caractere. Nom de la colonne numerique.
#' @param groupe Caractere ou NULL.
#' @param regles_couleur Liste.
#' @return Un KPI ou une liste de KPI.
#' @export
kpi_ecart_type <- function(donnees, variable, groupe = NULL, regles_couleur = list()) {
  valider_colonne(donnees, variable, "l'ecart-type")
  valider_numerique(donnees[[variable]], variable)

  if (is.null(groupe) || !nzchar(groupe)) {
    val <- stats::sd(donnees[[variable]], na.rm = TRUE)
    return(creer_kpi(
      nom = paste0("Ecart-type (", variable, ")"),
      valeur = val, type = "ecart_type", format = "decimal_2",
      source = "ecart_type", regles_couleur = regles_couleur
    ))
  }

  valider_colonne(donnees, groupe, "l'ecart-type")
  valider_groupe(donnees[[groupe]], groupe, 2)

  modalites <- unique(donnees[[groupe]][!is.na(donnees[[groupe]])])
  kpis <- list()
  for (mod in modalites) {
    sous <- donnees[donnees[[groupe]] == mod, variable]
    val <- stats::sd(sous, na.rm = TRUE)
    k <- creer_kpi(
      nom = paste0("Ecart-type ", variable, " [", mod, "]"),
      valeur = val, type = "ecart_type", format = "decimal_2",
      groupe = as.character(mod), source = "ecart_type",
      regles_couleur = regles_couleur
    )
    kpis[[k$id]] <- k
  }
  kpis
}

#' KPI d'effectif
#'
#' @param donnees Data.frame.
#' @param variable Caractere.
#' @param regles_couleur Liste.
#' @return Un KPI.
#' @export
kpi_effectif <- function(donnees, variable, regles_couleur = list()) {
  valider_colonne(donnees, variable, "l'effectif")
  val <- sum(!is.na(donnees[[variable]]))
  creer_kpi(
    nom = paste0("Effectif (", variable, ")"),
    valeur = val, type = "effectif", format = "entier",
    source = "effectif", regles_couleur = regles_couleur
  )
}

#' KPI de frequence
#'
#' @param donnees Data.frame.
#' @param variable Caractere.
#' @param modalite Caractere ou valeur.
#' @param regles_couleur Liste.
#' @return Un KPI.
#' @export
kpi_frequence <- function(donnees, variable, modalite, regles_couleur = list()) {
  valider_colonne(donnees, variable, "la frequence")
  if (is.null(modalite) || length(modalite) == 0) {
    stop("Aucune modalite specifiee pour la frequence.", call. = FALSE)
  }

  vec <- donnees[[variable]]
  vec_valide <- vec[!is.na(vec)]
  total <- length(vec_valide)
  pct <- if (total > 0) sum(vec_valide == modalite) / total else 0

  creer_kpi(
    nom = paste0("Frequence (", variable, " = ", modalite, ")"),
    valeur = pct, type = "frequence", format = "pourcent",
    source = "frequence", regles_couleur = regles_couleur
  )
}

# --- Tests statistiques -----------------------------------------------------

#' KPI d'un test t de Student
#'
#' Effectue un test t comparant une variable numerique entre deux
#' modalites d'un groupe.
#'
#' @param donnees Data.frame.
#' @param variable Caractere. Variable numerique dependante.
#' @param groupe Caractere. Variable de regroupement (2 modalites).
#' @param regles_couleur Liste.
#' @return Une liste de 4 KPI : p-value, d de Cohen, moyennes par groupe.
#' @export
kpi_test_t <- function(donnees, variable, groupe, regles_couleur = list()) {
  valider_colonne(donnees, variable, "le test t")
  valider_colonne(donnees, groupe, "le test t")
  valider_numerique(donnees[[variable]], variable)
  valider_groupe(donnees[[groupe]], groupe, 2, 2)

  modalites <- unique(donnees[[groupe]][!is.na(donnees[[groupe]])])
  g1 <- modalites[1]; g2 <- modalites[2]

  x1 <- donnees[donnees[[groupe]] == g1, variable]; x1 <- x1[!is.na(x1)]
  x2 <- donnees[donnees[[groupe]] == g2, variable]; x2 <- x2[!is.na(x2)]

  if (length(x1) < 2 || length(x2) < 2) {
    stop("Chaque groupe doit contenir au moins 2 observations.", call. = FALSE)
  }

  res_t <- stats::t.test(x1, x2)
  moy1 <- mean(x1); moy2 <- mean(x2)
  sd1 <- stats::sd(x1); sd2 <- stats::sd(x2)
  n1 <- length(x1); n2 <- length(x2)

  sd_pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  d_cohen <- (moy1 - moy2) / sd_pooled

  kpi_p <- creer_kpi(
    nom = paste0("p-value test t (", variable, ")"),
    valeur = res_t$p.value, type = "p_value", format = "decimal_3",
    source = "t.test", regles_couleur = regles_couleur
  )
  kpi_d <- creer_kpi(
    nom = paste0("d de Cohen (", variable, ")"),
    valeur = d_cohen, type = "d_cohen", format = "decimal_2",
    source = "t.test"
  )
  kpi_m1 <- creer_kpi(
    nom = paste0("Moyenne ", variable, " [", g1, "]"),
    valeur = moy1, type = "moyenne", format = "decimal_2",
    groupe = as.character(g1), source = "t.test"
  )
  kpi_m2 <- creer_kpi(
    nom = paste0("Moyenne ", variable, " [", g2, "]"),
    valeur = moy2, type = "moyenne", format = "decimal_2",
    groupe = as.character(g2), source = "t.test"
  )

  res <- list()
  res[[kpi_p$id]]  <- kpi_p
  res[[kpi_d$id]]  <- kpi_d
  res[[kpi_m1$id]] <- kpi_m1
  res[[kpi_m2$id]] <- kpi_m2
  res
}

#' KPI d'un test du Chi-deux
#'
#' Effectue un test d'independance du Chi-deux sur deux variables
#' categorielles.
#'
#' @param donnees Data.frame.
#' @param variable1 Caractere.
#' @param variable2 Caractere.
#' @param regles_couleur Liste.
#' @return Une liste de 3 KPI : p-value, V de Cramer, effectif total.
#' @export
kpi_test_chisq <- function(donnees, variable1, variable2, regles_couleur = list()) {
  valider_colonne(donnees, variable1, "le chi-deux")
  valider_colonne(donnees, variable2, "le chi-deux")
  if (identical(variable1, variable2)) {
    stop("Les deux variables doivent etre differentes.", call. = FALSE)
  }

  tab <- table(donnees[[variable1]], donnees[[variable2]])
  if (any(dim(tab) < 2)) {
    stop("Chaque variable doit avoir au moins 2 modalites.", call. = FALSE)
  }

  res_chi <- suppressWarnings(stats::chisq.test(tab))

  chi2 <- as.numeric(res_chi$statistic)
  n <- sum(tab)
  k <- nrow(tab); r <- ncol(tab)
  min_dim <- min(k, r)
  cramer_v <- if (min_dim > 1 && n > 0) sqrt(chi2 / (n * (min_dim - 1))) else 0

  kpi_p <- creer_kpi(
    nom = paste0("p-value Chi2 (", variable1, " x ", variable2, ")"),
    valeur = res_chi$p.value, type = "p_value", format = "decimal_3",
    source = "chisq.test", regles_couleur = regles_couleur
  )
  kpi_v <- creer_kpi(
    nom = paste0("V de Cramer (", variable1, " x ", variable2, ")"),
    valeur = cramer_v, type = "d_cohen", format = "decimal_2",
    source = "chisq.test"
  )
  kpi_n <- creer_kpi(
    nom = paste0("Effectif total (", variable1, " x ", variable2, ")"),
    valeur = n, type = "effectif", format = "entier",
    source = "chisq.test"
  )

  res <- list()
  res[[kpi_p$id]] <- kpi_p
  res[[kpi_v$id]] <- kpi_v
  res[[kpi_n$id]] <- kpi_n
  res
}

#' KPI de correlation
#'
#' Calcule le coefficient de correlation lineaire ou de rang et son test.
#'
#' @param donnees Data.frame.
#' @param variable1 Caractere. Premiere variable numerique.
#' @param variable2 Caractere. Seconde variable numerique.
#' @param methode Caractere. "pearson" (defaut) ou "spearman".
#' @param regles_couleur Liste.
#' @return Une liste de 2 KPI : coefficient r et p-value.
#' @export
kpi_correlation <- function(donnees, variable1, variable2, methode = "pearson",
                             regles_couleur = list()) {
  valider_colonne(donnees, variable1, "la correlation")
  valider_colonne(donnees, variable2, "la correlation")
  valider_numerique(donnees[[variable1]], variable1)
  valider_numerique(donnees[[variable2]], variable2)
  if (identical(variable1, variable2)) {
    stop("Les deux variables doivent etre differentes.", call. = FALSE)
  }

  res_cor <- stats::cor.test(
    donnees[[variable1]], donnees[[variable2]],
    method = methode, use = "complete.obs"
  )

  kpi_r <- creer_kpi(
    nom = paste0("Correlation ", methode, " (", variable1, " ~ ", variable2, ")"),
    valeur = as.numeric(res_cor$estimate), type = "correlation",
    format = "decimal_2", source = paste0("cor.", methode),
    regles_couleur = regles_couleur
  )
  kpi_p <- creer_kpi(
    nom = paste0("p-value correlation (", variable1, " ~ ", variable2, ")"),
    valeur = res_cor$p.value, type = "p_value", format = "decimal_3",
    source = paste0("cor.", methode)
  )

  res <- list()
  res[[kpi_r$id]] <- kpi_r
  res[[kpi_p$id]] <- kpi_p
  res
}

# --- Tableaux ---------------------------------------------------------------

#' Tableau : statistique par groupe
#'
#' @param donnees Data.frame.
#' @param variable Caractere. Variable numerique.
#' @param groupe Caractere. Variable de regroupement.
#' @param stat Caractere. "moyenne", "mediane", "ecart_type", "somme",
#'   "effectif", "minimum", "maximum".
#' @param totaux Logique. Ajouter une ligne Total.
#' @param regles Liste. Regles de couleur.
#' @param nom_custom Caractere ou NULL.
#' @param commentaire Caractere ou NULL.
#' @return Un KPI de type tableau.
#' @export
kpi_tableau_par_groupe <- function(donnees, variable, groupe, stat = "moyenne",
                                    totaux = TRUE, regles = list(),
                                    nom_custom = NULL, commentaire = NULL) {
  valider_colonne(donnees, variable, "le tableau par groupe")
  valider_colonne(donnees, groupe, "le tableau par groupe")
  valider_numerique(donnees[[variable]], variable)
  valider_groupe(donnees[[groupe]], groupe, 2)

  calc <- function(vec) {
    vec <- vec[!is.na(vec)]
    if (length(vec) == 0) return(NA_real_)
    switch(stat,
      "moyenne"    = mean(vec),
      "mediane"    = stats::median(vec),
      "ecart_type" = stats::sd(vec),
      "somme"      = sum(vec),
      "effectif"   = length(vec),
      "minimum"    = min(vec),
      "maximum"    = max(vec),
      NA_real_
    )
  }

  libelle_stat <- switch(stat,
    "moyenne"    = "Moyenne",
    "mediane"    = "Mediane",
    "ecart_type" = "Ecart-type",
    "somme"      = "Somme",
    "effectif"   = "Effectif",
    "minimum"    = "Minimum",
    "maximum"    = "Maximum",
    stat
  )

  modalites <- unique(donnees[[groupe]][!is.na(donnees[[groupe]])])
  valeurs <- sapply(modalites, function(m) {
    calc(donnees[[variable]][donnees[[groupe]] == m])
  })

  fmt <- if (stat == "effectif") "entier" else "decimal_2"
  valeurs_fmt <- sapply(valeurs, function(v) formater_valeur(v, fmt))

  nom_col_valeur <- paste0(libelle_stat, " (", variable, ")")

  tab <- data.frame(
    Modalite = as.character(modalites),
    Valeur = valeurs_fmt,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(tab) <- c(groupe, nom_col_valeur)

  if (totaux) {
    val_total <- calc(donnees[[variable]])
    ligne_total <- data.frame(
      Modalite = "Total",
      Valeur = formater_valeur(val_total, fmt),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(ligne_total) <- c(groupe, nom_col_valeur)
    tab <- rbind(tab, ligne_total)
  }

  nom_kpi <- if (!is.null(nom_custom) && nzchar(nom_custom)) {
    nom_custom
  } else {
    paste0(libelle_stat, " ", variable, " par ", groupe)
  }

  creer_kpi_tableau(
    nom = nom_kpi,
    tableau = tab,
    sous_type = "par_groupe",
    mode_affichage = "effectifs",
    stats_annexes = list(),
    commentaire = commentaire,
    regles_couleur = regles,
    colonne_valeur_index = 2
  )
}

#' Tableau croise entre 2 variables qualitatives
#'
#' @param donnees Data.frame.
#' @param var_ligne Caractere. Variable en lignes.
#' @param var_colonne Caractere. Variable en colonnes.
#' @param affichage Caractere. "effectifs", "pourcent_total",
#'   "pourcent_ligne", "pourcent_colonne".
#' @param totaux Logique.
#' @param stats_annexes Logique. Ajouter p-value et V de Cramer.
#' @param regles Liste.
#' @param nom_custom Caractere ou NULL.
#' @param commentaire Caractere ou NULL.
#' @return Un KPI de type tableau.
#' @export
kpi_tableau_croise <- function(donnees, var_ligne, var_colonne,
                                affichage = "effectifs", totaux = TRUE,
                                stats_annexes = TRUE, regles = list(),
                                nom_custom = NULL, commentaire = NULL) {
  valider_colonne(donnees, var_ligne, "le tableau croise")
  valider_colonne(donnees, var_colonne, "le tableau croise")
  if (identical(var_ligne, var_colonne)) {
    stop("Les deux variables doivent etre differentes.", call. = FALSE)
  }

  tab_brut <- table(donnees[[var_ligne]], donnees[[var_colonne]])

  # Ordre d'apparition
  modes_ligne <- unique(as.character(donnees[[var_ligne]][!is.na(donnees[[var_ligne]])]))
  modes_col   <- unique(as.character(donnees[[var_colonne]][!is.na(donnees[[var_colonne]])]))
  modes_ligne <- modes_ligne[modes_ligne %in% rownames(tab_brut)]
  modes_col   <- modes_col[modes_col %in% colnames(tab_brut)]

  mat <- as.matrix(tab_brut[modes_ligne, modes_col, drop = FALSE])
  noms_col <- colnames(mat)

  fmt_val <- function(x) {
    if (affichage == "effectifs") {
      as.character(as.integer(x))
    } else {
      paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), " %")
    }
  }

  # Calcul de la matrice affichee
  if (affichage == "effectifs") {
    mat_aff <- mat
    mat_total_ligne <- rowSums(mat)
    mat_total_col   <- colSums(mat)
    total_general   <- sum(mat)
  } else if (affichage == "pourcent_total") {
    total_general <- sum(mat)
    mat_aff <- mat / total_general * 100
    mat_total_ligne <- rowSums(mat) / total_general * 100
    mat_total_col   <- colSums(mat) / total_general * 100
  } else if (affichage == "pourcent_ligne") {
    row_tot <- rowSums(mat)
    mat_aff <- mat / row_tot * 100
    mat_total_ligne <- rep(100, nrow(mat))
    mat_total_col <- colSums(mat) / sum(mat) * 100
    total_general <- 100
  } else if (affichage == "pourcent_colonne") {
    col_tot <- colSums(mat)
    mat_aff <- sweep(mat, 2, col_tot, "/") * 100
    mat_total_ligne <- rowSums(mat) / sum(mat) * 100
    mat_total_col <- rep(100, ncol(mat))
    total_general <- 100
  }

  df <- data.frame(
    Modalite = rownames(mat_aff),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(noms_col)) {
    df[[noms_col[i]]] <- sapply(mat_aff[, i], fmt_val)
  }

  if (totaux) {
    df[["Total"]] <- sapply(mat_total_ligne, fmt_val)

    ligne_total <- data.frame(
      Modalite = "Total",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    for (i in seq_along(noms_col)) {
      ligne_total[[noms_col[i]]] <- fmt_val(mat_total_col[i])
    }
    ligne_total[["Total"]] <- fmt_val(total_general)
    df <- rbind(df, ligne_total)
  }

  names(df)[1] <- var_ligne

  stats_res <- list()
  if (stats_annexes) {
    tryCatch({
      chi <- suppressWarnings(stats::chisq.test(tab_brut))
      n <- sum(tab_brut)
      k <- nrow(tab_brut); r <- ncol(tab_brut)
      min_dim <- min(k, r)
      cramer <- if (min_dim > 1 && n > 0) {
        sqrt(as.numeric(chi$statistic) / (n * (min_dim - 1)))
      } else 0
      stats_res$p_value <- chi$p.value
      stats_res$v_cramer <- cramer
    }, error = function(e) NULL)
  }

  nom_kpi <- if (!is.null(nom_custom) && nzchar(nom_custom)) {
    nom_custom
  } else {
    paste0(var_ligne, " x ", var_colonne)
  }

  creer_kpi_tableau(
    nom = nom_kpi,
    tableau = df,
    sous_type = "croise",
    mode_affichage = affichage,
    stats_annexes = stats_res,
    commentaire = commentaire,
    regles_couleur = regles,
    colonne_valeur_index = NULL
  )
}