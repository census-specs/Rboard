# ==============================================================================
# Tests : R/utils.R
# ==============================================================================

test_that("generer_id produit 4 caracteres avec bonne lettre prefixe", {
  expect_match(generer_id("kpi"), "^K[A-Z2-9]{3}$")
  expect_match(generer_id("graphique"), "^G[A-Z2-9]{3}$")
  expect_match(generer_id("page"), "^P[A-Z2-9]{3}$")
  expect_match(generer_id("ligne"), "^L[A-Z2-9]{3}$")
  expect_match(generer_id("element"), "^E[A-Z2-9]{3}$")
  expect_match(generer_id("tableau"), "^T[A-Z2-9]{3}$")
})

test_that("generer_id n'utilise pas les caracteres ambigus", {
  ids <- replicate(100, generer_id("kpi"))
  expect_false(any(grepl("[0O1IL]", ids)))
})

test_that("generer_id produit des identifiants differents", {
  ids <- replicate(50, generer_id("kpi"))
  expect_gt(length(unique(ids)), 40)
})

test_that("formater_valeur decimal_3 fonctionne", {
  expect_equal(formater_valeur(0.032, "decimal_3"), "0,032")
  expect_equal(formater_valeur(1.5, "decimal_3"), "1,500")
})

test_that("formater_valeur decimal_2 fonctionne", {
  expect_equal(formater_valeur(1.256, "decimal_2"), "1,26")
})

test_that("formater_valeur pourcent fonctionne", {
  expect_equal(formater_valeur(0.125, "pourcent"), "12,5 %")
  expect_equal(formater_valeur(0.78, "pourcent"), "78,0 %")
})

test_that("formater_valeur entier fonctionne", {
  expect_equal(formater_valeur(42.7, "entier"), "43")
  expect_equal(formater_valeur(42.2, "entier"), "42")
})

test_that("formater_valeur texte retourne as.character", {
  expect_equal(formater_valeur("abc", "texte"), "abc")
  expect_equal(formater_valeur(42, "texte"), "42")
})

test_that("formater_valeur format inconnu retourne la valeur", {
  expect_equal(formater_valeur(42, "inconnu"), "42")
})

test_that("formater_valeur gere NULL et NA", {
  expect_equal(formater_valeur(NULL, "decimal_2"), "")
  expect_true(is.na(formater_valeur(NA, "decimal_2")))
})

test_that("operateur %||% fonctionne", {
  expect_equal(NULL %||% "defaut", "defaut")
  expect_equal("valeur" %||% "defaut", "valeur")
  expect_equal(character(0) %||% "defaut", "defaut")
})

test_that("rboard_ok, rboard_warn, rboard_ko produisent les bons types", {
  expect_equal(rboard_ok("msg")$type, "success")
  expect_equal(rboard_warn("msg")$type, "warning")
  expect_equal(rboard_ko("msg")$type, "danger")
  expect_equal(rboard_info("msg")$type, "info")
})

test_that("rboard_try capture les erreurs", {
  res <- rboard_try(stop("boom"), "test")
  expect_false(res$ok)
  expect_null(res$valeur)
  expect_equal(res$message$type, "danger")
  expect_match(res$message$texte, "boom")
})

test_that("rboard_try retourne la valeur si succes", {
  res <- rboard_try(42, "test")
  expect_true(res$ok)
  expect_equal(res$valeur, 42)
})

# --- Helpers de securite ---

test_that("rboard_largeur_element retourne 6 par defaut", {
  expect_equal(rboard_largeur_element(NULL), 6L)
  expect_equal(rboard_largeur_element(list()), 6L)
  expect_equal(rboard_largeur_element(list(taille = NULL)), 6L)
  expect_equal(rboard_largeur_element(list(taille = NA)), 6L)
  expect_equal(rboard_largeur_element(list(taille = 4)), 6L)
})

test_that("rboard_largeur_element accepte 3, 6, 12", {
  expect_equal(rboard_largeur_element(list(taille = 3)), 3L)
  expect_equal(rboard_largeur_element(list(taille = 6)), 6L)
  expect_equal(rboard_largeur_element(list(taille = 12)), 12L)
})

test_that("rboard_hauteur_el retourne 1 par defaut", {
  expect_equal(rboard_hauteur_el(NULL), 1L)
  expect_equal(rboard_hauteur_el(list()), 1L)
  expect_equal(rboard_hauteur_el(list(hauteur_unites = NULL)), 1L)
  expect_equal(rboard_hauteur_el(list(hauteur_unites = NA)), 1L)
  expect_equal(rboard_hauteur_el(list(hauteur_unites = 5)), 1L)
})

test_that("rboard_hauteur_el accepte 1, 2, 3, 4", {
  expect_equal(rboard_hauteur_el(list(hauteur_unites = 1)), 1L)
  expect_equal(rboard_hauteur_el(list(hauteur_unites = 2)), 2L)
  expect_equal(rboard_hauteur_el(list(hauteur_unites = 3)), 3L)
  expect_equal(rboard_hauteur_el(list(hauteur_unites = 4)), 4L)
})

test_that("rboard_somme_largeurs gere le vide", {
  expect_equal(rboard_somme_largeurs(NULL), 0L)
  expect_equal(rboard_somme_largeurs(list()), 0L)
})

test_that("rboard_somme_largeurs calcule correctement", {
  els <- list(
    list(taille = 6),
    list(taille = 3),
    list(taille = 3)
  )
  expect_equal(rboard_somme_largeurs(els), 12L)
})

test_that("rboard_max_hauteurs gere le vide", {
  expect_equal(rboard_max_hauteurs(NULL), 0L)
  expect_equal(rboard_max_hauteurs(list()), 0L)
})

test_that("rboard_max_hauteurs calcule correctement", {
  els <- list(
    list(hauteur_unites = 1),
    list(hauteur_unites = 3),
    list(hauteur_unites = 2)
  )
  expect_equal(rboard_max_hauteurs(els), 3L)
})