# ==============================================================================
# Tests : R/projet.R
# ==============================================================================

# ==============================================================================
# 1. Creation
# ==============================================================================

test_that("nouveau_projet cree les 9 champs attendus", {
  p <- nouveau_projet("Test")
  expect_type(p, "list")
  expect_true(all(c("nom", "date_creation", "date_modification",
                    "donnees", "kpi", "graphiques", "textes",
                    "dashboard", "commentaires") %in% names(p)))
})

test_that("nouveau_projet initialise correctement", {
  p <- nouveau_projet("Mon analyse")
  expect_equal(p$nom, "Mon analyse")
  expect_s3_class(p$date_creation, "Date")
  expect_null(p$donnees)
  expect_equal(length(p$kpi), 0)
  expect_equal(length(p$graphiques), 0)
  expect_equal(length(p$textes), 0)
})

# ==============================================================================
# 2. Sauvegarde / chargement
# ==============================================================================

test_that("sauver_projet et charger_projet font un aller-retour", {
  p <- nouveau_projet("Test aller-retour")
  p$kpi <- list(kpi_test = list(id = "K123", nom = "Test", type = "moyenne"))

  chemin <- tempfile(fileext = ".rds")
  on.exit(unlink(chemin), add = TRUE)

  sauver_projet(p, chemin)
  expect_true(file.exists(chemin))

  p2 <- charger_projet(chemin)
  expect_equal(p2$nom, "Test aller-retour")
  expect_equal(length(p2$kpi), 1)
})

test_that("sauver_projet refuse un objet non-liste", {
  chemin <- tempfile(fileext = ".rds")
  expect_error(sauver_projet("pas une liste", chemin), "pas un projet")
})

test_that("charger_projet echoue si fichier inexistant", {
  expect_error(
    charger_projet("fichier_qui_nexiste_pas_xyz.rds"),
    "n'existe pas"
  )
})

test_that("charger_projet detecte les champs manquants", {
  faux <- list(nom = "Incomplet", date_creation = Sys.Date())
  chemin <- tempfile(fileext = ".rds")
  on.exit(unlink(chemin), add = TRUE)
  saveRDS(faux, chemin)

  expect_error(charger_projet(chemin), "Champs manquants")
})

test_that("charger_projet repare les dates invalides", {
  p <- nouveau_projet("Test")
  p$date_creation <- "2026-01-01"
  chemin <- tempfile(fileext = ".rds")
  on.exit(unlink(chemin), add = TRUE)
  saveRDS(p, chemin)

  p2 <- charger_projet(chemin)
  expect_s3_class(p2$date_creation, "Date")
})

# ==============================================================================
# 3. reparer_element
# ==============================================================================

test_that("reparer_element ajoute un id si absent", {
  el <- list(type = "texte", contenu = "x", taille = 6)
  el2 <- reparer_element(el, NULL)
  expect_true(!is.null(el2$id))
  expect_match(el2$id, "^E")
})

test_that("reparer_element pose type = texte si absent", {
  el <- list(id = "E001", contenu = "x", taille = 6)
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$type, "texte")
})

test_that("reparer_element migre taille 4 -> 6", {
  el <- list(id = "E001", type = "texte", taille = 4)
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$taille, 6L)
})

test_that("reparer_element pose tableau_etendu = FALSE", {
  el <- list(id = "E001", type = "kpi", id_kpi = "T1", taille = 6)
  el2 <- reparer_element(el, NULL)
  expect_false(isTRUE(el2$tableau_etendu))
})

test_that("reparer_element pose id_texte = NULL", {
  el <- list(id = "E001", type = "texte", contenu = "x", taille = 6)
  el2 <- reparer_element(el, NULL)
  expect_null(el2$id_texte)
})

test_that("reparer_element laisse tableau_long avec taille 6", {
  el <- list(id = "E001", type = "kpi", id_kpi = "T1", taille = 6,
             tableau_long = TRUE)
  el2 <- reparer_element(el, NULL)
  expect_true(el2$tableau_long)
  expect_equal(el2$taille, 6L)  # 6 est maintenant valide
})

test_that("reparer_element force taille 12 si tableau_long et taille invalide", {
  el <- list(id = "E001", type = "kpi", id_kpi = "T1", taille = 3,
             tableau_long = TRUE)
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$taille, 12L)
})

test_that("reparer_element force tableau_etendu -> taille 6", {
  el <- list(id = "E001", type = "kpi", id_kpi = "T1", taille = 12,
             tableau_etendu = TRUE)
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$taille, 6L)
})

test_that("reparer_element desactive tableau_etendu si long actif", {
  el <- list(id = "E001", type = "kpi", id_kpi = "T1", taille = 6,
             tableau_long = TRUE, tableau_etendu = TRUE)
  el2 <- reparer_element(el, NULL)
  expect_true(el2$tableau_long)
  expect_false(isTRUE(el2$tableau_etendu))
})

test_that("reparer_element nettoie un commentaire auto", {
  el <- list(id = "E001", type = "kpi", id_kpi = "K1", taille = 6,
             commentaire = "Extrait depuis un script (classe numeric).")
  el2 <- reparer_element(el, NULL)
  expect_null(el2$commentaire)
})

test_that("reparer_element preserve un commentaire manuel", {
  el <- list(id = "E001", type = "kpi", id_kpi = "K1", taille = 6,
             commentaire = "Mon commentaire")
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$commentaire, "Mon commentaire")
})

# ==============================================================================
# 4. reparer_projet
# ==============================================================================

test_that("reparer_projet ajoute les champs manquants", {
  p <- list(nom = "Casse")
  p2 <- reparer_projet(p)
  expect_true("donnees" %in% names(p2))
  expect_true("textes" %in% names(p2))
  expect_true("dashboard" %in% names(p2))
})

test_that("reparer_projet nettoie les commentaires auto des KPI", {
  p <- nouveau_projet("Test")
  p$kpi$K1 <- list(
    id = "K1", nom = "Moyenne", valeur = 42,
    type = "moyenne", format = "decimal_2",
    commentaire = "Extrait depuis un script (classe numeric).",
    regles_couleur = list()
  )

  p2 <- reparer_projet(p)
  expect_null(p2$kpi$K1$commentaire)
})

test_that("reparer_projet nettoie les commentaires auto des graphiques", {
  p <- nouveau_projet("Test")
  p$graphiques$G1 <- list(
    id = "G1", nom = "Boxplot",
    objet = ggplot2::ggplot(),
    commentaire = "Extrait depuis un script (classe ggplot)."
  )

  p2 <- reparer_projet(p)
  expect_null(p2$graphiques$G1$commentaire)
})

test_that("reparer_projet nettoie les commentaires auto des textes", {
  p <- nouveau_projet("Test")
  p$textes$TXT1 <- list(
    id = "TXT1", nom = "Intro", contenu = "Texte",
    style = "normal",
    commentaire = "Extrait depuis un script (classe character)."
  )

  p2 <- reparer_projet(p)
  expect_null(p2$textes$TXT1$commentaire)
})

test_that("reparer_projet reconstruit un graphique via son code", {
  p <- nouveau_projet("Test")
  code <- "library(ggplot2)\nggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()"
  p$graphiques <- list(
    G123 = list(id = "G123", nom = "Test", code = code, objet = NULL)
  )

  p2 <- reparer_projet(p)
  expect_true(inherits(p2$graphiques$G123$objet, "ggplot"))
})

# ==============================================================================
# 5. Migration ancien projet
# ==============================================================================

test_that("charger_projet migre un ancien projet grille 4", {
  ancien <- nouveau_projet("Ancien")
  ancien$dashboard <- list(pages = list(list(
    id = "P001", titre = "Page 1",
    lignes = list(list(
      id = "L001",
      elements = list(
        list(id = "E001", type = "texte", taille = 4,
             contenu = "ancien", style = "normal")
      )
    ))
  )))

  chemin <- tempfile(fileext = ".rds")
  on.exit(unlink(chemin), add = TRUE)
  saveRDS(ancien, chemin)

  p <- charger_projet(chemin)
  el <- p$dashboard$pages[[1]]$lignes[[1]]$elements[[1]]

  expect_equal(el$taille, 6L)
  expect_equal(el$hauteur_unites, 1L)
})

test_that("charger_projet migre un ancien tableau long sur largeur 6", {
  ancien <- nouveau_projet("Ancien tableau")
  ancien$kpi$T1 <- list(
    id = "T1", nom = "Grand tableau", type = "tableau",
    tableau = data.frame(a = 1:15, b = 16:30),
    format = "texte", source = "tableau"
  )
  ancien$dashboard <- list(pages = list(list(
    id = "P001", titre = "Page 1",
    lignes = list(list(
      id = "L001",
      elements = list(
        list(id = "E001", type = "kpi", id_kpi = "T1", taille = 6,
             style = "normal")
      )
    ))
  )))

  chemin <- tempfile(fileext = ".rds")
  on.exit(unlink(chemin), add = TRUE)
  saveRDS(ancien, chemin)

  p <- charger_projet(chemin)
  el <- p$dashboard$pages[[1]]$lignes[[1]]$elements[[1]]

  expect_true(isTRUE(el$tableau_long))
  # 6 est maintenant valide pour un tableau long
  expect_true(el$taille %in% c(6L, 12L))
  expect_equal(el$hauteur_unites, 3L)
})

test_that("la migration est idempotente", {
  ancien <- nouveau_projet("Idempotent")
  ancien$dashboard <- list(pages = list(list(
    id = "P001", titre = "Page 1",
    lignes = list(list(
      id = "L001",
      elements = list(
        list(id = "E001", type = "texte", taille = 4, contenu = "x", style = "normal")
      )
    ))
  )))
  chemin <- tempfile(fileext = ".rds")
  on.exit(unlink(chemin), add = TRUE)
  saveRDS(ancien, chemin)

  p1 <- charger_projet(chemin)
  p2 <- reparer_projet(p1)

  expect_identical(p1$dashboard, p2$dashboard)
})

# ==============================================================================
# 6. verifier_projet
# ==============================================================================

test_that("verifier_projet retourne TRUE sur projet valide", {
  p <- nouveau_projet("Test")
  expect_true(verifier_projet(p))
})

test_that("verifier_projet detecte un graphique casse", {
  p <- nouveau_projet("Test")
  p$graphiques <- list(
    G123 = list(id = "G123", nom = "Test", objet = NULL)
  )
  res <- verifier_projet(p)
  expect_type(res, "character")
  expect_true(any(grepl("ggplot manquant", res)))
})

# ==============================================================================
# 7. resumer_projet
# ==============================================================================

test_that("resumer_projet retourne les bons compteurs", {
  p <- nouveau_projet("Test")
  p$kpi$K1 <- list(id = "K1", nom = "K1", type = "moyenne")
  p$textes$TXT1 <- list(id = "TXT1", nom = "T", contenu = "x", style = "normal")
  p$graphiques$G1 <- list(id = "G1", nom = "G1")

  res <- resumer_projet(p)
  expect_equal(res$nb_kpi, 1L)
  expect_equal(res$nb_textes, 1L)
  expect_equal(res$nb_graphiques, 1L)
})

test_that("resumer_projet gere un projet NULL", {
  res <- resumer_projet(NULL)
  expect_equal(res$nb_kpi, 0L)
  expect_equal(res$nb_pages, 0L)
})