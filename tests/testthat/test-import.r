# ==============================================================================
# Tests : R/import.R
# ==============================================================================

# --- Creation d'un CSV de test ---
creer_csv_test <- function() {
  chemin <- tempfile(fileext = ".csv")
  df <- data.frame(
    age = c(25, 30, 35, 40, 45),
    sexe = c("H", "F", "H", "F", "H"),
    score = c(12.5, 15.2, 18.3, 14.1, 20.0),
    stringsAsFactors = FALSE
  )
  write.csv(df, chemin, row.names = FALSE)
  chemin
}

test_that("importer_fichier lit un CSV", {
  chemin <- creer_csv_test()
  on.exit(unlink(chemin), add = TRUE)

  d <- importer_fichier(chemin)
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 5)
  expect_equal(ncol(d), 3)
  expect_true("age" %in% names(d))
})

test_that("importer_fichier echoue si fichier inexistant", {
  expect_error(
    importer_fichier("fichier_inexistant_xyz.csv"),
    "n'existe pas"
  )
})

test_that("importer_fichier echoue sur extension inconnue", {
  chemin <- tempfile(fileext = ".xyz")
  writeLines("test", chemin)
  on.exit(unlink(chemin), add = TRUE)
  expect_error(importer_fichier(chemin), "non supporte")
})

test_that("detecter_type_variable identifie les types", {
  expect_equal(detecter_type_variable(1:5), "numerique")
  expect_equal(detecter_type_variable(c(1.5, 2.5)), "numerique")
  expect_equal(detecter_type_variable(factor(c("a", "b"))), "facteur")
  expect_equal(detecter_type_variable(c("a", "b")), "facteur")
  expect_equal(detecter_type_variable(c(TRUE, FALSE)), "logique")
  expect_equal(detecter_type_variable(as.Date("2026-01-01")), "date")
})

test_that("resumer_variables retourne les bonnes colonnes", {
  df <- data.frame(
    age = c(25, 30, NA),
    sexe = factor(c("H", "F", "H")),
    stringsAsFactors = FALSE
  )
  res <- resumer_variables(df)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("variable", "type", "nb_na", "nb_uniques", "exemple"))
  expect_equal(nrow(res), 2)
})

test_that("resumer_variables calcule correctement le nombre de NA", {
  df <- data.frame(
    x = c(1, 2, NA, 4, NA),
    y = c("a", "b", "c", "d", "e"),
    stringsAsFactors = FALSE
  )
  res <- resumer_variables(df)
  expect_equal(res$nb_na[res$variable == "x"], 2)
  expect_equal(res$nb_na[res$variable == "y"], 0)
})

test_that("valider_donnees accepte un data.frame valide", {
  df <- data.frame(x = 1:3)
  expect_true(valider_donnees(df))
})

test_that("valider_donnees refuse un objet non data.frame", {
  expect_error(valider_donnees("texte"), "data.frame attendu")
})

test_that("valider_donnees refuse un data.frame vide en colonnes", {
  df <- data.frame()
  expect_error(valider_donnees(df), "aucune colonne")
})

test_that("valider_donnees refuse un data.frame vide en lignes", {
  df <- data.frame(x = numeric(0))
  expect_error(valider_donnees(df), "aucune ligne")
})