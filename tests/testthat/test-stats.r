# ==============================================================================
# Tests : R/stats.R
# ==============================================================================

df_stats <- data.frame(
  age = c(20, 25, 30, 35, 40, 45, 50, 55),
  sexe = factor(c("H", "F", "H", "F", "H", "F", "H", "F")),
  groupe = factor(c("A", "A", "A", "A", "B", "B", "B", "B")),
  score = c(10, 12, 14, 16, 18, 20, 22, 24),
  reponse = factor(c("Oui", "Non", "Oui", "Non", "Oui", "Non", "Oui", "Non")),
  stringsAsFactors = FALSE
)

test_that("kpi_moyenne calcule la moyenne globale", {
  k <- kpi_moyenne(df_stats, "age")
  expect_type(k, "list")
  expect_equal(k$valeur, 37.5)
  expect_equal(k$type, "moyenne")
})

test_that("kpi_moyenne par groupe produit plusieurs KPI", {
  kpis <- kpi_moyenne(df_stats, "age", "sexe")
  expect_type(kpis, "list")
  expect_equal(length(kpis), 2)
})

test_that("kpi_mediane calcule la mediane", {
  k <- kpi_mediane(df_stats, "age")
  expect_equal(k$valeur, 37.5)
})

test_that("kpi_ecart_type calcule l'ecart-type", {
  k <- kpi_ecart_type(df_stats, "age")
  expect_gt(k$valeur, 10)
  expect_lt(k$valeur, 15)
})

test_that("kpi_effectif compte les observations", {
  k <- kpi_effectif(df_stats, "age")
  expect_equal(k$valeur, 8)
})

test_that("kpi_frequence calcule un pourcentage", {
  k <- kpi_frequence(df_stats, "reponse", "Oui")
  expect_equal(k$valeur, 0.5)
  expect_match(k$label, "50,0 %")
})

test_that("kpi_test_t compare deux groupes", {
  kpis <- kpi_test_t(df_stats, "age", "sexe")
  expect_length(kpis, 4)
  types <- sapply(kpis, function(k) k$type)
  expect_true("p_value" %in% types)
  expect_true("d_cohen" %in% types)
})

test_that("kpi_test_t refuse un groupe a 3 modalites", {
  df3 <- data.frame(
    x = 1:6,
    g = factor(c("A", "A", "B", "B", "C", "C"))
  )
  expect_error(kpi_test_t(df3, "x", "g"), "maximum 2")
})

test_that("kpi_test_chisq calcule p et V de Cramer", {
  kpis <- kpi_test_chisq(df_stats, "sexe", "reponse")
  expect_length(kpis, 3)
  types <- sapply(kpis, function(k) k$type)
  expect_true("p_value" %in% types)
  expect_true("effectif" %in% types)
})

test_that("kpi_test_chisq refuse variables identiques", {
  expect_error(kpi_test_chisq(df_stats, "sexe", "sexe"), "differentes")
})

test_that("kpi_correlation calcule Pearson", {
  kpis <- kpi_correlation(df_stats, "age", "score", "pearson")
  expect_length(kpis, 2)
  r_kpi <- kpis[[1]]
  expect_equal(r_kpi$valeur, 1, tolerance = 0.001)
})

test_that("kpi_correlation refuse variables identiques", {
  expect_error(kpi_correlation(df_stats, "age", "age"), "differentes")
})

test_that("kpi_moyenne echoue si colonne inexistante", {
  expect_error(kpi_moyenne(df_stats, "inexistant"), "n'existe pas")
})

test_that("kpi_tableau_par_groupe construit un tableau", {
  k <- kpi_tableau_par_groupe(df_stats, "age", "sexe", stat = "moyenne")
  expect_equal(k$type, "tableau")
  expect_s3_class(k$tableau, "data.frame")
  expect_equal(ncol(k$tableau), 2)
  expect_equal(nrow(k$tableau), 3)
})

test_that("kpi_tableau_croise construit un tableau avec totaux", {
  k <- kpi_tableau_croise(df_stats, "sexe", "reponse",
                           affichage = "effectifs", totaux = TRUE)
  expect_equal(k$type, "tableau")
  expect_s3_class(k$tableau, "data.frame")
  # 2 lignes + Total = 3 lignes ; 2 col + Total = 3 colonnes
  # MAIS : la 1ere colonne est le libelle de ligne, donc :
  # - soit 4 colonnes (1 libelle + 2 modalites + Total)
  # On attend donc 4 colonnes et 3 lignes
  expect_equal(nrow(k$tableau), 3)
  expect_equal(ncol(k$tableau), 4)
})

test_that("kpi_tableau_croise en % ligne", {
  k <- kpi_tableau_croise(df_stats, "sexe", "reponse",
                           affichage = "pourcent_ligne", totaux = FALSE)
  valeurs <- unlist(k$tableau[, -1])
  expect_true(all(grepl("%", valeurs)))
})