# ==============================================================================
# Tests : R/kpi.R
# ==============================================================================

test_that("creer_kpi produit les 13 champs attendus", {
  k <- creer_kpi("Test", 0.5, "generique")
  expect_type(k, "list")
  expect_length(k, 13)
  expect_true(all(c("id", "nom", "valeur", "label", "type", "couleur",
                    "couleur_manuelle", "regles_couleur", "commentaire",
                    "source", "format", "groupe", "date_creation")
                  %in% names(k)))
})

test_that("creer_kpi genere un id court", {
  k <- creer_kpi("Test", 0.5)
  expect_match(k$id, "^K[A-Z2-9]{3}$")
})

test_that("creer_kpi construit un label automatique", {
  k <- creer_kpi("Moyenne age", 34.6, format = "decimal_2")
  expect_match(k$label, "Moyenne age = 34,60")
})

test_that("creer_kpi conserve le label fourni", {
  k <- creer_kpi("Test", 0.5, label = "Mon label personnalise")
  expect_equal(k$label, "Mon label personnalise")
})

test_that("decider_couleur pour p_value avec seuils par defaut", {
  k1 <- creer_kpi("p1", 0.03, "p_value")
  k2 <- creer_kpi("p2", 0.07, "p_value")
  k3 <- creer_kpi("p3", 0.50, "p_value")
  expect_equal(k1$couleur, "success")
  expect_equal(k2$couleur, "warning")
  expect_equal(k3$couleur, "danger")
})

test_that("decider_couleur pour d_cohen", {
  expect_equal(creer_kpi("d1", 0.95, "d_cohen")$couleur, "success")
  expect_equal(creer_kpi("d2", 0.60, "d_cohen")$couleur, "info")
  expect_equal(creer_kpi("d3", 0.30, "d_cohen")$couleur, "warning")
  expect_equal(creer_kpi("d4", 0.10, "d_cohen")$couleur, "secondary")
})

test_that("decider_couleur pour correlation", {
  expect_equal(creer_kpi("r1", 0.80, "correlation")$couleur, "success")
  expect_equal(creer_kpi("r2", 0.50, "correlation")$couleur, "info")
  expect_equal(creer_kpi("r3", 0.20, "correlation")$couleur, "secondary")
})

test_that("couleur_manuelle a priorite absolue", {
  k <- creer_kpi("Test", 0.03, "p_value", couleur_manuelle = "dark")
  expect_equal(k$couleur, "dark")
})

test_that("regles_couleur ont priorite sur logique automatique", {
  regles <- list(list(signe = "<", valeur = 0.10, couleur = "warning"))
  k <- creer_kpi("Test", 0.03, "p_value", regles_couleur = regles)
  expect_equal(k$couleur, "warning")
})

test_that("appliquer_regle fonctionne avec tous les signes", {
  expect_true(appliquer_regle(0.03, list(signe = "<", valeur = 0.05)))
  expect_true(appliquer_regle(0.05, list(signe = "<=", valeur = 0.05)))
  expect_true(appliquer_regle(0.10, list(signe = ">", valeur = 0.05)))
  expect_true(appliquer_regle(0.05, list(signe = ">=", valeur = 0.05)))
  expect_true(appliquer_regle(0.05, list(signe = "=", valeur = 0.05)))
  expect_false(appliquer_regle(0.10, list(signe = "<", valeur = 0.05)))
})

test_that("ajouter_kpi et supprimer_kpi fonctionnent", {
  p <- nouveau_projet("Test")
  k <- creer_kpi("Test", 0.5)
  p <- ajouter_kpi(p, k)
  expect_equal(length(p$kpi), 1)
  expect_true(k$id %in% names(p$kpi))

  p <- supprimer_kpi(p, k$id)
  expect_equal(length(p$kpi), 0)
})

test_that("supprimer_kpi ignore les ids inexistants", {
  p <- nouveau_projet("Test")
  expect_silent(p <- supprimer_kpi(p, "K999"))
  expect_equal(length(p$kpi), 0)
})