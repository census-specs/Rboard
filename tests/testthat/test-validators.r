# ==============================================================================
# Tests : R/validators.R
# ==============================================================================

df_test <- data.frame(
  age = c(25, 30, 35, 40),
  sexe = factor(c("H", "F", "H", "F")),
  groupe = factor(c("A", "B", "C", "A")),
  score = c(12, 15, 18, 20),
  stringsAsFactors = FALSE
)

test_that("valider_colonne accepte une colonne existante", {
  expect_true(valider_colonne(df_test, "age"))
  expect_true(valider_colonne(df_test, "sexe"))
})

test_that("valider_colonne echoue si colonne inexistante", {
  expect_error(valider_colonne(df_test, "inexistant"), "n'existe pas")
})

test_that("valider_colonne echoue si donnees NULL", {
  expect_error(valider_colonne(NULL, "age"), "Aucun jeu de donnees")
})

test_that("valider_colonne echoue si donnees pas data.frame", {
  expect_error(valider_colonne("texte", "age"), "data.frame")
})

test_that("valider_colonne echoue si nom vide", {
  expect_error(valider_colonne(df_test, ""), "Aucune variable")
})

test_that("valider_numerique accepte un vecteur numerique", {
  expect_true(valider_numerique(c(1, 2, 3), "x"))
})

test_that("valider_numerique echoue sur un vecteur non numerique", {
  expect_error(valider_numerique(c("a", "b"), "x"), "pas numerique")
})

test_that("valider_numerique echoue si tout est NA", {
  expect_error(valider_numerique(c(NA_real_, NA_real_), "x"),
               "aucune valeur exploitable")
})

test_that("valider_groupe accepte 2 modalites", {
  expect_true(valider_groupe(df_test$sexe, "sexe", 2, 2))
})

test_that("valider_groupe echoue si trop peu de modalites", {
  g <- factor(rep("A", 4))
  expect_error(valider_groupe(g, "g", 2), "au moins 2")
})

test_that("valider_groupe echoue si trop de modalites", {
  expect_error(valider_groupe(df_test$groupe, "g", 2, 2), "maximum 2")
})

test_that("valider_entier accepte un entier dans les bornes", {
  expect_true(valider_entier(5, "n", 1, 10))
  expect_true(valider_entier(5L, "n"))
})

test_that("valider_entier echoue si non entier", {
  expect_error(valider_entier(5.5, "n"), "entier")
})

test_that("valider_entier echoue si hors bornes", {
  expect_error(valider_entier(15, "n", 1, 10), "inferieur ou egal")
  expect_error(valider_entier(0, "n", 1, 10), "superieur ou egal")
})

test_that("valider_entier echoue si non numerique", {
  expect_error(valider_entier("abc", "n"), "nombre valide")
})

test_that("valider_texte accepte un texte non vide", {
  expect_true(valider_texte("bonjour", "nom"))
})

test_that("valider_texte echoue sur texte vide ou espaces", {
  expect_error(valider_texte("", "nom"), "obligatoire")
  expect_error(valider_texte("   ", "nom"), "obligatoire")
  expect_error(valider_texte(NULL, "nom"), "obligatoire")
})