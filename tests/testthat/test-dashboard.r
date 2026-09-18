# ==============================================================================
# Tests : R/dashboard.R
# ==============================================================================

# ==============================================================================
# 1. Constantes et helpers
# ==============================================================================

test_that("les constantes de grille sont coherentes", {
  expect_equal(RBOARD_GRILLE_LARGEUR, 12L)
  expect_equal(RBOARD_GRILLE_HAUTEUR, 6L)
  expect_equal(HAUTEUR_KPI, 1L)
  expect_equal(HAUTEUR_TEXTE, 1L)
  expect_equal(HAUTEUR_TABLEAU, 2L)
  expect_equal(HAUTEUR_TABLEAU_LONG, 3L)
  expect_equal(HAUTEUR_TABLEAU_ETENDU, 4L)
  expect_equal(HAUTEUR_GRAPHIQUE, 3L)
  expect_equal(HAUTEUR_GRAPHIQUE_LARGE, 4L)
})

test_that("rboard_hauteur_element retourne la bonne hauteur", {
  expect_equal(rboard_hauteur_element("kpi"), 1L)
  expect_equal(rboard_hauteur_element("texte"), 1L)
  expect_equal(rboard_hauteur_element("graphique"), 3L)
  expect_equal(rboard_hauteur_element("graphique", graphique_long = TRUE), 4L)

  kpi_tab <- list(type = "tableau")
  expect_equal(rboard_hauteur_element("kpi", kpi_tab), HAUTEUR_TABLEAU)
  expect_equal(rboard_hauteur_element("kpi", kpi_tab, tableau_long = TRUE),
               HAUTEUR_TABLEAU_LONG)
  expect_equal(rboard_hauteur_element("kpi", kpi_tab, tableau_etendu = TRUE),
               HAUTEUR_TABLEAU_ETENDU)

  kpi_simple <- list(type = "moyenne")
  expect_equal(rboard_hauteur_element("kpi", kpi_simple), 1L)
})

test_that("rboard_largeurs_autorisees tableau long accepte 6 ou 12", {
  kpi_tab <- list(type = "tableau")
  expect_equal(rboard_largeurs_autorisees("kpi", kpi_tab, tableau_long = TRUE),
               c(6L, 12L))
})

test_that("rboard_largeurs_autorisees tableau etendu impose 6", {
  kpi_tab <- list(type = "tableau")
  expect_equal(rboard_largeurs_autorisees("kpi", kpi_tab, tableau_etendu = TRUE),
               6L)
})

test_that("rboard_largeurs_autorisees standard", {
  expect_equal(rboard_largeurs_autorisees("kpi"), c(3L, 6L, 12L))
  expect_equal(rboard_largeurs_autorisees("texte"), c(3L, 6L, 12L))
  expect_equal(rboard_largeurs_autorisees("graphique"), c(6L, 12L))
})

test_that("rboard_suggere_tableau_long detecte les tableaux longs", {
  tab_court <- list(type = "tableau", tableau = data.frame(a = 1:3))
  tab_long <- list(type = "tableau", tableau = data.frame(a = 1:10))

  expect_false(rboard_suggere_tableau_long(tab_court))
  expect_true(rboard_suggere_tableau_long(tab_long))
  expect_false(rboard_suggere_tableau_long(list(type = "moyenne")))
  expect_false(rboard_suggere_tableau_long(NULL))
})

# ==============================================================================
# 2. Structure de base
# ==============================================================================

test_that("creer_dashboard retourne un dashboard vide", {
  d <- creer_dashboard()
  expect_type(d, "list")
  expect_true("pages" %in% names(d))
  expect_equal(length(d$pages), 0)
})

test_that("creer_page cree une page avec id et titre", {
  p <- creer_page("Ma page")
  expect_equal(p$titre, "Ma page")
  expect_match(p$id, "^P")
  expect_equal(length(p$lignes), 0)
})

test_that("ajouter_page et obtenir_page fonctionnent", {
  d <- creer_dashboard()
  p1 <- creer_page("Page 1")
  d <- ajouter_page(d, p1)
  expect_equal(length(d$pages), 1)
  expect_equal(obtenir_page(d, p1$id)$titre, "Page 1")
})

# ==============================================================================
# 3. creer_element
# ==============================================================================

test_that("creer_element pose hauteur_unites par defaut", {
  el <- creer_element("texte", contenu = "test", taille_largeur = 6)
  expect_equal(el$hauteur_unites, 1L)
  expect_equal(el$taille, 6L)
  expect_false(isTRUE(el$tableau_long))
  expect_false(isTRUE(el$tableau_etendu))
})

test_that("creer_element deduit la hauteur du type", {
  el_graph <- creer_element("graphique", id_kpi = "G1", taille_largeur = 6)
  expect_equal(el_graph$hauteur_unites, HAUTEUR_GRAPHIQUE)

  el_txt <- creer_element("texte", contenu = "x", taille_largeur = 6)
  expect_equal(el_txt$hauteur_unites, HAUTEUR_TEXTE)

  el_kpi <- creer_element("kpi", id_kpi = "K1", taille_largeur = 6)
  expect_equal(el_kpi$hauteur_unites, HAUTEUR_KPI)
})

test_that("creer_element accepte les largeurs 3, 6, 12", {
  expect_equal(creer_element("texte", contenu = "x", taille_largeur = 3)$taille, 3L)
  expect_equal(creer_element("texte", contenu = "x", taille_largeur = 6)$taille, 6L)
  expect_equal(creer_element("texte", contenu = "x", taille_largeur = 12)$taille, 12L)
})

test_that("creer_element refuse la largeur 4", {
  expect_error(creer_element("texte", contenu = "x", taille_largeur = 4),
               "invalide")
})

test_that("creer_element refuse un type invalide", {
  expect_error(creer_element("inconnu", contenu = "test"), "invalide")
})

test_that("creer_element avec tableau_long accepte largeur 6 ou 12", {
  el6 <- creer_element("kpi", id_kpi = "T1", taille_largeur = 6,
                        tableau_long = TRUE, hauteur_unites = 3L)
  expect_equal(el6$taille, 6L)
  expect_true(el6$tableau_long)

  el12 <- creer_element("kpi", id_kpi = "T1", taille_largeur = 12,
                         tableau_long = TRUE, hauteur_unites = 3L)
  expect_equal(el12$taille, 12L)
})

test_that("creer_element avec tableau_long et largeur 3 force a 12", {
  el <- creer_element("kpi", id_kpi = "T1", taille_largeur = 3,
                       tableau_long = TRUE, hauteur_unites = 3L)
  expect_equal(el$taille, 12L)
})

test_that("creer_element avec tableau_etendu force largeur 6 et h 4", {
  el <- creer_element("kpi", id_kpi = "T1", taille_largeur = 12,
                       tableau_etendu = TRUE)
  expect_equal(el$taille, 6L)
  expect_true(el$tableau_etendu)
  expect_equal(el$hauteur_unites, HAUTEUR_TABLEAU_ETENDU)
})

test_that("creer_element refuse tableau_long ET tableau_etendu", {
  expect_error(
    creer_element("kpi", id_kpi = "T1", tableau_long = TRUE, tableau_etendu = TRUE),
    "long.*etendu|etendu.*long"
  )
})

test_that("creer_element pose niveau_titre pour style=titre", {
  el <- creer_element("texte", contenu = "# Titre", style = "titre",
                       taille_largeur = 6, niveau_titre = 1L)
  expect_equal(el$niveau_titre, 1L)

  el2 <- creer_element("texte", contenu = "Titre", style = "titre",
                        taille_largeur = 6, niveau_titre = 99L)
  expect_equal(el2$niveau_titre, 2L)
})

test_that("creer_element accepte un id_texte", {
  el <- creer_element("texte", contenu = "x", taille_largeur = 6,
                       id_texte = "TXT001")
  expect_equal(el$id_texte, "TXT001")
})

# ==============================================================================
# 4. Ajout d'elements
# ==============================================================================

test_that("ajouter_element fonctionne sur ligne vide", {
  d <- creer_dashboard()
  p <- creer_page("P1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("texte", contenu = "x", taille_largeur = 6)
  d <- ajouter_element(d, p$id, id_l, el)
  expect_equal(length(d$pages[[1]]$lignes[[1]]$elements), 1)
})

test_that("ajouter_element refuse largeur > 12", {
  d <- creer_dashboard()
  p <- creer_page("P1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el1 <- creer_element("texte", contenu = "x", taille_largeur = 6, hauteur_unites = 1L)
  el2 <- creer_element("texte", contenu = "y", taille_largeur = 6, hauteur_unites = 1L)
  el3 <- creer_element("texte", contenu = "z", taille_largeur = 6, hauteur_unites = 1L)

  d <- ajouter_element(d, p$id, id_l, el1)
  d <- ajouter_element(d, p$id, id_l, el2)

  expect_error(ajouter_element(d, p$id, id_l, el3), "pleine")
})

test_that("ajouter_element refuse hauteur > 6", {
  d <- creer_dashboard()
  p <- creer_page("P1")
  d <- ajouter_page(d, p)

  for (i in 1:6) {
    d <- ajouter_ligne(d, p$id)
  }
  for (i in 1:6) {
    id_l <- d$pages[[1]]$lignes[[i]]$id
    el <- creer_element("texte", contenu = paste0("t", i),
                         taille_largeur = 12, hauteur_unites = 1L)
    d <- ajouter_element(d, p$id, id_l, el)
  }

  d <- ajouter_ligne(d, p$id)
  id_l7 <- d$pages[[1]]$lignes[[7]]$id
  el7 <- creer_element("texte", contenu = "x", taille_largeur = 12, hauteur_unites = 1L)
  expect_error(ajouter_element(d, p$id, id_l7, el7), "pleine")
})

# ==============================================================================
# 5. Modification
# ==============================================================================

test_that("modifier_element bascule tableau_long sur largeur 6", {
  d <- creer_dashboard()
  p <- creer_page("P1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("kpi", id_kpi = "T1", taille_largeur = 6,
                       hauteur_unites = 2L)
  d <- ajouter_element(d, p$id, id_l, el)

  d <- modifier_element(d, p$id, id_l, el$id,
                         list(tableau_long = TRUE, taille = 6L))
  el_mod <- d$pages[[1]]$lignes[[1]]$elements[[1]]
  expect_true(el_mod$tableau_long)
  expect_equal(el_mod$taille, 6L)
  expect_equal(el_mod$hauteur_unites, HAUTEUR_TABLEAU_LONG)
})

test_that("modifier_element bascule tableau_long sur largeur 12", {
  d <- creer_dashboard()
  p <- creer_page("P1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("kpi", id_kpi = "T1", taille_largeur = 6,
                       hauteur_unites = 2L)
  d <- ajouter_element(d, p$id, id_l, el)

  d <- modifier_element(d, p$id, id_l, el$id,
                         list(tableau_long = TRUE, taille = 12L))
  el_mod <- d$pages[[1]]$lignes[[1]]$elements[[1]]
  expect_equal(el_mod$taille, 12L)
})

test_that("modifier_element refuse taille 4", {
  d <- creer_dashboard()
  p <- creer_page("P1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("texte", contenu = "x", taille_largeur = 6)
  d <- ajouter_element(d, p$id, id_l, el)

  expect_error(
    modifier_element(d, p$id, id_l, el$id, list(taille = 4)),
    "invalide")
})

test_that("modifier_element bascule tableau_etendu", {
  d <- creer_dashboard()
  p <- creer_page("P1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("kpi", id_kpi = "T1", taille_largeur = 6,
                       hauteur_unites = 2L)
  d <- ajouter_element(d, p$id, id_l, el)

  d <- modifier_element(d, p$id, id_l, el$id, list(tableau_etendu = TRUE))
  el_mod <- d$pages[[1]]$lignes[[1]]$elements[[1]]
  expect_true(el_mod$tableau_etendu)
  expect_equal(el_mod$taille, 6L)
  expect_equal(el_mod$hauteur_unites, HAUTEUR_TABLEAU_ETENDU)
})

# ==============================================================================
# 6. Suppression / deplacement
# ==============================================================================

test_that("supprimer_element retire le bon element", {
  d <- creer_dashboard()
  p <- creer_page("P1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("texte", contenu = "test", taille_largeur = 6)
  d <- ajouter_element(d, p$id, id_l, el)
  expect_equal(length(d$pages[[1]]$lignes[[1]]$elements), 1)

  d <- supprimer_element(d, p$id, id_l, el$id)
  expect_equal(length(d$pages[[1]]$lignes[[1]]$elements), 0)
})

test_that("deplacer_element echange deux elements", {
  d <- creer_dashboard()
  p <- creer_page("P1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el1 <- creer_element("texte", contenu = "A", taille_largeur = 3)
  el2 <- creer_element("texte", contenu = "B", taille_largeur = 3)
  d <- ajouter_element(d, p$id, id_l, el1)
  d <- ajouter_element(d, p$id, id_l, el2)

  d <- deplacer_element(d, p$id, id_l, el1$id, "droite")
  expect_equal(d$pages[[1]]$lignes[[1]]$elements[[1]]$contenu, "B")
  expect_equal(d$pages[[1]]$lignes[[1]]$elements[[2]]$contenu, "A")
})

test_that("deplacer_ligne change l'ordre", {
  d <- creer_dashboard()
  p <- creer_page("P1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  d <- ajouter_ligne(d, p$id)

  id_l1 <- d$pages[[1]]$lignes[[1]]$id
  id_l2 <- d$pages[[1]]$lignes[[2]]$id

  d <- deplacer_ligne(d, p$id, id_l1, "bas")
  expect_equal(d$pages[[1]]$lignes[[1]]$id, id_l2)
  expect_equal(d$pages[[1]]$lignes[[2]]$id, id_l1)
})

# ==============================================================================
# 7. resumer_dashboard
# ==============================================================================

test_that("resumer_dashboard retourne un data.frame", {
  d <- creer_dashboard()
  p1 <- creer_page("Page 1")
  d <- ajouter_page(d, p1)
  d <- ajouter_ligne(d, p1$id)

  res <- resumer_dashboard(d)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
})