# ==============================================================================
# Tests : R/projet.R
# ==============================================================================

# ==============================================================================
# 1. Creation de projet
# ==============================================================================

test_that("nouveau_projet cree les 8 champs attendus", {
  p <- nouveau_projet("Test")
  expect_type(p, "list")
  expect_length(p, 8)
  expect_named(p, c("nom", "date_creation", "date_modification",
                    "donnees", "kpi", "graphiques", "dashboard", "commentaires"))
})

test_that("nouveau_projet initialise correctement", {
  p <- nouveau_projet("Mon analyse")
  expect_equal(p$nom, "Mon analyse")
  expect_s3_class(p$date_creation, "Date")
  expect_null(p$donnees)
  expect_equal(length(p$kpi), 0)
  expect_equal(length(p$graphiques), 0)
})

test_that("nouveau_projet accepte un nom par defaut", {
  p <- nouveau_projet()
  expect_equal(p$nom, "Nouveau projet")
})

# ==============================================================================
# 2. Sauvegarde et chargement
# ==============================================================================

test_that("sauver_projet et charger_projet font un aller-retour", {
  p <- nouveau_projet("Test aller-retour")
  p$kpi <- list(kpi_test = list(id = "K123", nom = "Test"))

  chemin <- tempfile(fileext = ".rds")
  on.exit(unlink(chemin), add = TRUE)

  sauver_projet(p, chemin)
  expect_true(file.exists(chemin))

  p2 <- charger_projet(chemin)
  expect_equal(p2$nom, "Test aller-retour")
  expect_equal(length(p2$kpi), 1)
  expect_equal(p2$kpi$kpi_test$id, "K123")
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
  p$date_creation <- "2026-01-01"  # pas un Date
  chemin <- tempfile(fileext = ".rds")
  on.exit(unlink(chemin), add = TRUE)
  saveRDS(p, chemin)

  p2 <- charger_projet(chemin)
  expect_s3_class(p2$date_creation, "Date")
})

# ==============================================================================
# 3. reparer_element : type et identifiants
# ==============================================================================

test_that("reparer_element ajoute un id si absent", {
  el <- list(type = "texte", contenu = "x", taille = 6)
  el2 <- reparer_element(el, NULL)
  expect_true(!is.null(el2$id))
  expect_match(el2$id, "^E")
})

test_that("reparer_element ajoute un type valide si absent", {
  el <- list(id = "E001", contenu = "x", taille = 6)
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$type, "texte")
})

test_that("reparer_element normalise un type invalide", {
  el <- list(id = "E001", type = "inconnu", taille = 6)
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$type, "texte")
})

test_that("reparer_element pose style = normal par defaut", {
  el <- list(id = "E001", type = "texte", taille = 6)
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$style, "normal")
})

# ==============================================================================
# 4. reparer_element : largeur
# ==============================================================================

test_that("reparer_element migre taille 4 -> 6", {
  el <- list(id = "E001", type = "texte", taille = 4)
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$taille, 6L)
})

test_that("reparer_element rejette les tailles invalides -> 6", {
  for (t in c(0, 1, 2, 5, 7, 8, 11, NA, NULL)) {
    el <- list(id = "E001", type = "texte", taille = t)
    el2 <- reparer_element(el, NULL)
    expect_equal(el2$taille, 6L,
                 info = sprintf("taille %s devrait devenir 6", as.character(t)))
  }
})

test_that("reparer_element conserve les tailles 3, 6, 12", {
  for (t in c(3L, 6L, 12L)) {
    el <- list(id = "E001", type = "texte", taille = t)
    el2 <- reparer_element(el, NULL)
    expect_equal(el2$taille, t)
  }
})

# ==============================================================================
# 5. reparer_element : tableau_long
# ==============================================================================

test_that("reparer_element laisse tableau_long = FALSE si non-tableau", {
  el <- list(id = "E001", type = "graphique", taille = 6)
  el2 <- reparer_element(el, NULL)
  expect_false(isTRUE(el2$tableau_long))
})

test_that("reparer_element detecte un tableau long via suggestion", {
  projet <- nouveau_projet("Test")
  projet$kpi$T1 <- list(
    id = "T1", nom = "Grand tableau", type = "tableau",
    tableau = data.frame(a = 1:10, b = 11:20)
  )
  el <- list(id = "E001", type = "kpi", id_kpi = "T1", taille = 6)

  el2 <- reparer_element(el, projet)
  expect_true(isTRUE(el2$tableau_long))
  expect_equal(el2$taille, 12L)  # forcé par tableau_long
})

test_that("reparer_element ne force pas un petit tableau en long", {
  projet <- nouveau_projet("Test")
  projet$kpi$T1 <- list(
    id = "T1", nom = "Petit tableau", type = "tableau",
    tableau = data.frame(a = 1:3, b = 4:6)
  )
  el <- list(id = "E001", type = "kpi", id_kpi = "T1", taille = 6)

  el2 <- reparer_element(el, projet)
  expect_false(isTRUE(el2$tableau_long))
  expect_equal(el2$taille, 6L)
})

test_that("reparer_element force taille = 12 si tableau_long = TRUE", {
  el <- list(id = "E001", type = "kpi", id_kpi = "T1", taille = 6,
             tableau_long = TRUE)
  el2 <- reparer_element(el, NULL)
  expect_true(el2$tableau_long)
  expect_equal(el2$taille, 12L)
})

test_that("reparer_element respecte tableau_long = FALSE explicite", {
  el <- list(id = "E001", type = "kpi", id_kpi = "T1", taille = 6,
             tableau_long = FALSE)
  el2 <- reparer_element(el, NULL)
  expect_false(isTRUE(el2$tableau_long))
})

# ==============================================================================
# 6. reparer_element : hauteur_unites
# ==============================================================================

test_that("reparer_element pose hauteur_unites pour un KPI simple", {
  el <- list(id = "E001", type = "kpi", id_kpi = "K1", taille = 6)
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$hauteur_unites, 1L)
})

test_that("reparer_element pose hauteur_unites = 3 pour un graphique", {
  el <- list(id = "E001", type = "graphique", taille = 6)
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$hauteur_unites, 3L)
})

test_that("reparer_element pose hauteur_unites = 1 pour un texte", {
  el <- list(id = "E001", type = "texte", taille = 6, contenu = "x")
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$hauteur_unites, 1L)
})

test_that("reparer_element pose hauteur_unites = 2 pour un tableau compact", {
  projet <- nouveau_projet("Test")
  projet$kpi$T1 <- list(id = "T1", type = "tableau",
                        tableau = data.frame(a = 1:3))
  el <- list(id = "E001", type = "kpi", id_kpi = "T1", taille = 6)

  el2 <- reparer_element(el, projet)
  expect_equal(el2$hauteur_unites, 2L)
})

test_that("reparer_element pose hauteur_unites = 3 pour un tableau long", {
  projet <- nouveau_projet("Test")
  projet$kpi$T1 <- list(id = "T1", type = "tableau",
                        tableau = data.frame(a = 1:10))
  el <- list(id = "E001", type = "kpi", id_kpi = "T1", taille = 6)

  el2 <- reparer_element(el, projet)
  expect_equal(el2$hauteur_unites, 3L)
})

test_that("reparer_element respecte un hauteur_unites valide existant", {
  el <- list(id = "E001", type = "kpi", id_kpi = "K1", taille = 6,
             hauteur_unites = 2L)
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$hauteur_unites, 2L)
})

test_that("reparer_element corrige un hauteur_unites invalide", {
  el <- list(id = "E001", type = "kpi", id_kpi = "K1", taille = 6,
             hauteur_unites = 5L)
  el2 <- reparer_element(el, NULL)
  expect_equal(el2$hauteur_unites, 1L)

  el3 <- list(id = "E001", type = "graphique", taille = 6,
              hauteur_unites = 0L)
  el3_r <- reparer_element(el3, NULL)
  expect_equal(el3_r$hauteur_unites, 3L)
})

# ==============================================================================
# 7. reparer_projet : structure
# ==============================================================================

test_that("reparer_projet ajoute les champs manquants", {
  p <- list(nom = "Casse")
  p2 <- reparer_projet(p)
  expect_true("donnees" %in% names(p2))
  expect_true("dashboard" %in% names(p2))
  expect_true(is.list(p2$kpi))
})

test_that("reparer_projet retourne un nouveau projet si NULL", {
  p <- reparer_projet(NULL)
  expect_true("nom" %in% names(p))
  expect_equal(length(p$kpi), 0)
})

test_that("reparer_projet gere un projet sain sans modification", {
  p <- nouveau_projet("Sain")
  p2 <- reparer_projet(p)
  expect_equal(p$nom, p2$nom)
  expect_equal(length(p2$kpi), 0)
  expect_equal(length(p2$graphiques), 0)
})

# ==============================================================================
# 8. reparer_projet : KPI
# ==============================================================================

test_that("reparer_projet complete les champs obligatoires d'un KPI", {
  p <- nouveau_projet("Test")
  p$kpi$K1 <- list(valeur = 0.5)  # ni id, ni nom, ni type

  p2 <- reparer_projet(p)
  k <- p2$kpi$K1
  expect_true(!is.null(k$id))
  expect_true(!is.null(k$nom))
  expect_true(!is.null(k$type))
  expect_true(!is.null(k$format))
  expect_true(is.list(k$regles_couleur))
})

test_that("reparer_projet recalcule la couleur d'un KPI si absente", {
  p <- nouveau_projet("Test")
  p$kpi$K1 <- list(id = "K1", nom = "p-val", valeur = 0.03,
                   type = "p_value", format = "decimal_3")

  p2 <- reparer_projet(p)
  expect_equal(p2$kpi$K1$couleur, "success")
})

test_that("reparer_projet ne touche pas un KPI complet", {
  p <- nouveau_projet("Test")
  p$kpi$K1 <- list(id = "K1", nom = "Moyenne", valeur = 40,
                   type = "moyenne", format = "decimal_2",
                   couleur = "primary",
                   regles_couleur = list())

  p2 <- reparer_projet(p)
  expect_equal(p2$kpi$K1$couleur, "primary")
  expect_equal(p2$kpi$K1$nom, "Moyenne")
})

# ==============================================================================
# 9. reparer_projet : graphiques
# ==============================================================================

test_that("reparer_projet reconstruit un graphique via son code", {
  p <- nouveau_projet("Test")
  code <- "library(ggplot2)\nggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()"
  p$graphiques <- list(
    G123 = list(id = "G123", nom = "Test", code = code, objet = NULL)
  )

  p2 <- reparer_projet(p)
  expect_true(inherits(p2$graphiques$G123$objet, "ggplot"))
})

test_that("reparer_projet laisse un graphique OK inchange", {
  p <- nouveau_projet("Test")
  g <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point()
  p$graphiques <- list(
    G123 = list(id = "G123", nom = "Test", code = NULL, objet = g)
  )

  p2 <- reparer_projet(p)
  expect_true(inherits(p2$graphiques$G123$objet, "ggplot"))
})

# ==============================================================================
# 10. reparer_projet : pages, lignes, elements
# ==============================================================================

test_that("reparer_projet repare les pages sans id", {
  p <- nouveau_projet("Test")
  p$dashboard <- list(pages = list(list(titre = "Page sans id", lignes = list())))

  p2 <- reparer_projet(p)
  expect_true(!is.null(p2$dashboard$pages[[1]]$id))
  expect_match(p2$dashboard$pages[[1]]$id, "^P")
})

test_that("reparer_projet repare les pages sans titre", {
  p <- nouveau_projet("Test")
  p$dashboard <- list(pages = list(list(id = "P001", lignes = list())))

  p2 <- reparer_projet(p)
  expect_equal(p2$dashboard$pages[[1]]$titre, "Page 1")
})

test_that("reparer_projet repare les lignes sans id", {
  p <- nouveau_projet("Test")
  p$dashboard <- list(pages = list(list(
    id = "P001", titre = "Page 1",
    lignes = list(list(elements = list()))
  )))

  p2 <- reparer_projet(p)
  expect_true(!is.null(p2$dashboard$pages[[1]]$lignes[[1]]$id))
  expect_match(p2$dashboard$pages[[1]]$lignes[[1]]$id, "^L")
})

test_that("reparer_projet repare les elements via reparer_element", {
  p <- nouveau_projet("Test")
  p$dashboard <- list(pages = list(list(
    id = "P001", titre = "Page 1",
    lignes = list(list(
      id = "L001",
      elements = list(
        list(type = "texte", contenu = "x", taille = 4),  # taille 4 -> 6
        list(type = "texte", contenu = "y", taille = 6)   # deja bon
      )
    ))
  )))

  p2 <- reparer_projet(p)
  els <- p2$dashboard$pages[[1]]$lignes[[1]]$elements
  expect_equal(els[[1]]$taille, 6L)
  expect_equal(els[[2]]$taille, 6L)
  expect_true(!is.null(els[[1]]$id))
  expect_true(!is.null(els[[1]]$hauteur_unites))
})

# ==============================================================================
# 11. verifier_projet
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

test_that("verifier_projet detecte les champs KPI manquants", {
  p <- nouveau_projet("Test")
  p$kpi$K1 <- list(valeur = 0.5)  # pas d'id, pas de nom

  res <- verifier_projet(p)
  expect_type(res, "character")
  expect_true(any(grepl("KPI.*pas d'id", res)))
  expect_true(any(grepl("KPI.*pas de nom", res)))
})

test_that("verifier_projet detecte un depassement de largeur", {
  p <- nouveau_projet("Test")
  p$dashboard <- creer_dashboard()
  p$dashboard <- ajouter_page(p$dashboard, creer_page("Page 1"))
  id_p <- p$dashboard$pages[[1]]$id
  p$dashboard <- ajouter_ligne(p$dashboard, id_p)
  id_l <- p$dashboard$pages[[1]]$lignes[[1]]$id

  # On force des elements qui depassent 12 col
  p$dashboard$pages[[1]]$lignes[[1]]$elements <- list(
    list(id = "E1", type = "texte", taille = 12, hauteur_unites = 1, style = "normal"),
    list(id = "E2", type = "texte", taille = 6,  hauteur_unites = 1, style = "normal")
  )

  res <- verifier_projet(p)
  expect_type(res, "character")
  expect_true(any(grepl("largeur.*18.*12", res)))
})

test_that("verifier_projet detecte une hauteur hors bornes", {
  p <- nouveau_projet("Test")
  p$dashboard <- creer_dashboard()
  p$dashboard <- ajouter_page(p$dashboard, creer_page("Page 1"))
  id_p <- p$dashboard$pages[[1]]$id
  p$dashboard <- ajouter_ligne(p$dashboard, id_p)
  id_l <- p$dashboard$pages[[1]]$lignes[[1]]$id

  p$dashboard$pages[[1]]$lignes[[1]]$elements <- list(
    list(id = "E1", type = "texte", taille = 6, hauteur_unites = 5, style = "normal")
  )

  res <- verifier_projet(p)
  expect_type(res, "character")
  expect_true(any(grepl("hauteur.*hors bornes", res)))
})

# ==============================================================================
# 12. resumer_projet
# ==============================================================================

test_that("resumer_projet retourne les bons compteurs", {
  p <- nouveau_projet("Test")
  p$kpi$K1 <- list(id = "K1", nom = "K1", type = "moyenne")
  p$kpi$K2 <- list(id = "K2", nom = "K2", type = "moyenne")
  p$graphiques$G1 <- list(id = "G1", nom = "G1")

  p$dashboard <- creer_dashboard()
  p$dashboard <- ajouter_page(p$dashboard, creer_page("Page 1"))
  p$dashboard <- ajouter_ligne(p$dashboard, p$dashboard$pages[[1]]$id)
  p$dashboard <- ajouter_ligne(p$dashboard, p$dashboard$pages[[1]]$id)

  id_p <- p$dashboard$pages[[1]]$id
  id_l1 <- p$dashboard$pages[[1]]$lignes[[1]]$id
  id_l2 <- p$dashboard$pages[[1]]$lignes[[2]]$id

  el1 <- creer_element("texte", contenu = "a", taille_largeur = 6,
                        hauteur_unites = 1L)
  el2 <- creer_element("texte", contenu = "b", taille_largeur = 6,
                        hauteur_unites = 2L)
  p$dashboard <- ajouter_element(p$dashboard, id_p, id_l1, el1)
  p$dashboard <- ajouter_element(p$dashboard, id_p, id_l2, el2)

  res <- resumer_projet(p)
  expect_equal(res$nb_kpi, 2L)
  expect_equal(res$nb_graphiques, 1L)
  expect_equal(res$nb_pages, 1L)
  expect_equal(res$nb_lignes, 2L)
  expect_equal(res$nb_elements, 2L)
  expect_equal(res$hauteur_totale_unites, 3L)  # 1 + 2
})

test_that("resumer_projet gere un projet NULL", {
  res <- resumer_projet(NULL)
  expect_equal(res$nb_kpi, 0L)
  expect_equal(res$nb_pages, 0L)
})

# ==============================================================================
# 13. Migration complete (integration)
# ==============================================================================

test_that("charger_projet migre un ancien projet grille 4", {
  # Ancien projet avec taille 4, pas de hauteur_unites, pas de tableau_long
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
  expect_false(isTRUE(el$tableau_long))
})

test_that("charger_projet migre un ancien tableau long", {
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
  expect_equal(el$taille, 12L)
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