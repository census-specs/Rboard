# ==============================================================================
# Tests : R/graphiques.R
# ==============================================================================

df_graph <- data.frame(
  age = c(20, 25, 30, 35, 40, 45),
  sexe = factor(c("H", "F", "H", "F", "H", "F")),
  score = c(10, 12, 14, 16, 18, 20),
  stringsAsFactors = FALSE
)

test_that("suggerer_graphiques pour 2 numeriques", {
  sugg <- suggerer_graphiques("numerique", "numerique")
  expect_true("Nuage de points" %in% sugg)
  expect_true("Ligne" %in% sugg)
})

test_that("suggerer_graphiques pour facteur x numerique", {
  sugg <- suggerer_graphiques("facteur", "numerique")
  expect_true("Boxplot" %in% sugg)
  expect_true("Violon" %in% sugg)
  expect_true("Barplot" %in% sugg)
})

test_that("suggerer_graphiques pour 1 variable numerique", {
  sugg <- suggerer_graphiques("numerique")
  expect_true("Boxplot" %in% sugg)
  expect_true("Histogramme" %in% sugg)
})

test_that("suggerer_graphiques pour 1 variable facteur", {
  sugg <- suggerer_graphiques("facteur")
  expect_true("Barplot (effectifs)" %in% sugg)
  expect_true("Camembert" %in% sugg)
})

test_that("suggerer_graphiques traite logique comme facteur", {
  sugg1 <- suggerer_graphiques("logique", "numerique")
  sugg2 <- suggerer_graphiques("facteur", "numerique")
  expect_equal(sugg1, sugg2)
})

test_that("construire_graphique Nuage de points", {
  g <- construire_graphique(df_graph, x = "age", y = "score", type = "Nuage de points")
  expect_s3_class(g, "ggplot")
})

test_that("construire_graphique Boxplot", {
  g <- construire_graphique(df_graph, x = "sexe", y = "age", type = "Boxplot")
  expect_s3_class(g, "ggplot")
})

test_that("construire_graphique Barplot", {
  g <- construire_graphique(df_graph, x = "sexe", y = "age", type = "Barplot")
  expect_s3_class(g, "ggplot")
})

test_that("construire_graphique Camembert avec X seul", {
  g <- construire_graphique(df_graph, x = "sexe", type = "Camembert")
  expect_s3_class(g, "ggplot")
})

test_that("construire_graphique Histogramme avec X seul", {
  g <- construire_graphique(df_graph, x = "age", type = "Histogramme")
  expect_s3_class(g, "ggplot")
})

test_that("construire_graphique attache un code R", {
  g <- construire_graphique(df_graph, x = "age", y = "score", type = "Nuage de points")
  code <- attr(g, "code")
  expect_type(code, "character")
  expect_match(code, "ggplot")
  expect_match(code, "geom_point")
})

test_that("construire_graphique echoue si X = Y", {
  expect_error(
    construire_graphique(df_graph, x = "age", y = "age", type = "Nuage de points"),
    "differentes"
  )
})

test_that("construire_graphique echoue si colonne inexistante", {
  expect_error(
    construire_graphique(df_graph, x = "inexistant", y = "score", type = "Nuage de points"),
    "n'existe pas"
  )
})

test_that("construire_graphique echoue si type vide", {
  expect_error(
    construire_graphique(df_graph, x = "age", y = "score", type = ""),
    "type de graphique"
  )
})

test_that("creer_kpi_graphique cree la structure attendue", {
  g <- construire_graphique(df_graph, x = "age", y = "score", type = "Nuage de points")
  k <- creer_kpi_graphique("Mon nuage", g)
  expect_equal(k$type, "graphique")
  expect_s3_class(k$objet, "ggplot")
  expect_match(k$id, "^G")
})

test_that("ajouter_graphique et supprimer_graphique", {
  p <- nouveau_projet("Test")
  g <- construire_graphique(df_graph, x = "age", y = "score", type = "Nuage de points")
  k <- creer_kpi_graphique("Mon nuage", g)

  p <- ajouter_graphique(p, k)
  expect_equal(length(p$graphiques), 1)

  p <- supprimer_graphique(p, k$id)
  expect_equal(length(p$graphiques), 0)
})