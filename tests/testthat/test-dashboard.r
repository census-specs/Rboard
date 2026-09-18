# ==============================================================================
# Tests : R/dashboard.R
# ==============================================================================

# ==============================================================================
# 1. Constantes et helpers de dimensions
# ==============================================================================

test_that("les constantes de grille sont coherentes", {
  expect_equal(RBOARD_GRILLE_LARGEUR, 12L)
  expect_equal(RBOARD_GRILLE_HAUTEUR, 6L)
  expect_equal(HAUTEUR_KPI, 1L)
  expect_equal(HAUTEUR_TEXTE, 1L)
  expect_equal(HAUTEUR_TABLEAU, 2L)
  expect_equal(HAUTEUR_TABLEAU_LONG, 3L)
  expect_equal(HAUTEUR_GRAPHIQUE, 3L)
})

test_that("les largeurs autorisees ne contiennent plus 4", {
  expect_equal(LARGEURS_STANDARD, c(3L, 6L, 12L))
  expect_equal(LARGEURS_LARGES, c(6L, 12L))
  expect_equal(LARGEURS_FORCEES_12, 12L)
  expect_false(4L %in% LARGEURS_STANDARD)
  expect_false(4L %in% LARGEURS_LARGES)
})

test_that("rboard_hauteur_element retourne la bonne hauteur", {
  expect_equal(rboard_hauteur_element("kpi"), 1L)
  expect_equal(rboard_hauteur_element("texte"), 1L)
  expect_equal(rboard_hauteur_element("graphique"), 3L)

  kpi_tab <- list(type = "tableau")
  expect_equal(rboard_hauteur_element("kpi", kpi_tab), HAUTEUR_TABLEAU)
  expect_equal(rboard_hauteur_element("kpi", kpi_tab, tableau_long = TRUE),
               HAUTEUR_TABLEAU_LONG)

  kpi_simple <- list(type = "moyenne")
  expect_equal(rboard_hauteur_element("kpi", kpi_simple), 1L)
})

test_that("rboard_largeurs_autorisees retourne les bonnes largeurs", {
  expect_equal(rboard_largeurs_autorisees("kpi"), LARGEURS_STANDARD)
  expect_equal(rboard_largeurs_autorisees("texte"), LARGEURS_STANDARD)
  expect_equal(rboard_largeurs_autorisees("graphique"), LARGEURS_LARGES)

  kpi_tab <- list(type = "tableau")
  expect_equal(rboard_largeurs_autorisees("kpi", kpi_tab), LARGEURS_STANDARD)
  expect_equal(rboard_largeurs_autorisees("kpi", kpi_tab, tableau_long = TRUE),
               12L)
})

test_that("rboard_suggere_tableau_long detecte les tableaux longs", {
  tab_court <- list(type = "tableau",
                    tableau = data.frame(a = 1:3, b = 4:6))
  tab_long <- list(type = "tableau",
                   tableau = data.frame(a = 1:10, b = 11:20))

  expect_false(rboard_suggere_tableau_long(tab_court))
  expect_true(rboard_suggere_tableau_long(tab_long))

  # Non-tableau : toujours FALSE
  expect_false(rboard_suggere_tableau_long(list(type = "moyenne")))
  expect_false(rboard_suggere_tableau_long(NULL))

  # Tableau vide
  expect_false(rboard_suggere_tableau_long(list(type = "tableau",
                                                 tableau = data.frame())))
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

test_that("supprimer_page retire la bonne page", {
  d <- creer_dashboard()
  p1 <- creer_page("Page 1")
  p2 <- creer_page("Page 2")
  d <- ajouter_page(d, p1)
  d <- ajouter_page(d, p2)
  d <- supprimer_page(d, p1$id)
  expect_equal(length(d$pages), 1)
  expect_equal(d$pages[[1]]$titre, "Page 2")
})

test_that("renommer_page change le titre", {
  d <- creer_dashboard()
  p <- creer_page("Ancien")
  d <- ajouter_page(d, p)
  d <- renommer_page(d, p$id, "Nouveau")
  expect_equal(obtenir_page(d, p$id)$titre, "Nouveau")
})

# ==============================================================================
# 3. creer_element
# ==============================================================================

test_that("creer_element pose hauteur_unites et tableau_long par defaut", {
  el <- creer_element("texte", contenu = "test", taille_largeur = 6)
  expect_equal(el$hauteur_unites, 1L)
  expect_false(isTRUE(el$tableau_long))
  expect_equal(el$taille, 6L)
})

test_that("creer_element deduit la hauteur du type", {
  el_graph <- creer_element("graphique", id_kpi = "G1", taille_largeur = 6)
  expect_equal(el_graph$hauteur_unites, HAUTEUR_GRAPHIQUE)

  el_txt <- creer_element("texte", contenu = "x", taille_largeur = 6)
  expect_equal(el_txt$hauteur_unites, HAUTEUR_TEXTE)

  el_kpi <- creer_element("kpi", id_kpi = "K1", taille_largeur = 6)
  expect_equal(el_kpi$hauteur_unites, HAUTEUR_KPI)
})

test_that("creer_element respecte hauteur_unites fourni", {
  el <- creer_element("kpi", id_kpi = "K1", taille_largeur = 6,
                       hauteur_unites = 3L)
  expect_equal(el$hauteur_unites, 3L)
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

test_that("creer_element refuse les largeurs invalides", {
  expect_error(creer_element("texte", contenu = "x", taille_largeur = 5), "invalide")
  expect_error(creer_element("texte", contenu = "x", taille_largeur = 7), "invalide")
  expect_error(creer_element("texte", contenu = "x", taille_largeur = 0), "invalide")
})

test_that("creer_element refuse un type invalide", {
  expect_error(creer_element("inconnu", contenu = "test"), "invalide")
})

test_that("creer_element force la largeur 12 pour tableau_long", {
  el <- creer_element("kpi", id_kpi = "T1", taille_largeur = 6,
                       tableau_long = TRUE, hauteur_unites = 3L)
  expect_equal(el$taille, 12L)
  expect_true(el$tableau_long)
  expect_equal(el$hauteur_unites, 3L)
})

test_that("creer_element pose niveau_titre pour style=titre", {
  el <- creer_element("texte", contenu = "# Titre", style = "titre",
                       taille_largeur = 6, niveau_titre = 1L)
  expect_equal(el$niveau_titre, 1L)

  el2 <- creer_element("texte", contenu = "Titre", style = "titre",
                        taille_largeur = 6, niveau_titre = 99L)
  expect_equal(el2$niveau_titre, 2L)  # fallback

  el3 <- creer_element("texte", contenu = "Texte", style = "normal",
                        taille_largeur = 6)
  expect_null(el3$niveau_titre)
})

# ==============================================================================
# 4. Lignes
# ==============================================================================

test_that("ajouter_ligne ajoute une ligne a la page", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  expect_equal(length(obtenir_page(d, p$id)$lignes), 1)
})

test_that("deplacer_ligne change l'ordre", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
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
# 5. Largeur et hauteur utilisees
# ==============================================================================

test_that("largeur_utilisee_page calcule correctement", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  expect_equal(largeur_utilisee_page(d, p$id), 0L)

  el1 <- creer_element("texte", contenu = "x", taille_largeur = 6)
  d <- ajouter_element(d, p$id, id_l, el1)
  expect_equal(largeur_utilisee_page(d, p$id), 6L)

  el2 <- creer_element("texte", contenu = "y", taille_largeur = 3)
  d <- ajouter_element(d, p$id, id_l, el2)
  expect_equal(largeur_utilisee_page(d, p$id), 9L)
})

test_that("hauteur_utilisee_page calcule correctement", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  expect_equal(hauteur_utilisee_page(d, p$id), 0L)

  # KPI en h=1
  el1 <- creer_element("texte", contenu = "x", taille_largeur = 6,
                        hauteur_unites = 1L)
  d <- ajouter_element(d, p$id, id_l, el1)
  expect_equal(hauteur_utilisee_page(d, p$id), 1L)
})

test_that("hauteur_ligne retourne le max des hauteurs", {
  ligne <- creer_ligne()
  ligne$elements <- list(
    creer_element("texte", contenu = "a", taille_largeur = 3, hauteur_unites = 1L),
    creer_element("texte", contenu = "b", taille_largeur = 3, hauteur_unites = 2L),
    creer_element("texte", contenu = "c", taille_largeur = 3, hauteur_unites = 1L)
  )
  expect_equal(hauteur_ligne(ligne), 2L)
})

# ==============================================================================
# 6. Ajout d'elements et contraintes
# ==============================================================================

test_that("ajouter_element refuse de depasser 12 colonnes", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el1 <- creer_element("texte", contenu = "x", taille_largeur = 6, hauteur_unites = 1L)
  el2 <- creer_element("texte", contenu = "y", taille_largeur = 6, hauteur_unites = 1L)
  el3 <- creer_element("texte", contenu = "z", taille_largeur = 3, hauteur_unites = 1L)

  d <- ajouter_element(d, p$id, id_l, el1)
  d <- ajouter_element(d, p$id, id_l, el2)

  expect_error(ajouter_element(d, p$id, id_l, el3), "pleine")
})

test_that("ajouter_element refuse de depasser la hauteur totale", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)

  # 6 lignes de h=1 -> page pleine (6 unites)
  for (i in 1:6) {
    d <- ajouter_ligne(d, p$id)
  }
  for (i in 1:6) {
    id_l <- d$pages[[1]]$lignes[[i]]$id
    el <- creer_element("texte", contenu = paste0("t", i),
                         taille_largeur = 12, hauteur_unites = 1L)
    d <- ajouter_element(d, p$id, id_l, el)
  }

  # Ajouter une 7eme ligne de h=1 doit echouer
  d <- ajouter_ligne(d, p$id)
  id_l7 <- d$pages[[1]]$lignes[[7]]$id
  el7 <- creer_element("texte", contenu = "debord", taille_largeur = 12,
                        hauteur_unites = 1L)
  expect_error(ajouter_element(d, p$id, id_l7, el7), "pleine")
})

test_that("ajouter_element accepte un graphique sur page vide", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("graphique", id_kpi = "G1", taille_largeur = 6,
                       hauteur_unites = 3L)
  d <- ajouter_element(d, p$id, id_l, el)
  expect_equal(hauteur_utilisee_page(d, p$id), 3L)
})

test_that("ajouter_element accepte graphique (h=3) + KPI (h=1) sur page vide", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el_g <- creer_element("graphique", id_kpi = "G1", taille_largeur = 6,
                         hauteur_unites = 3L)
  el_k <- creer_element("kpi", id_kpi = "K1", taille_largeur = 6,
                         hauteur_unites = 1L)

  d <- ajouter_element(d, p$id, id_l, el_g)
  # Apres graphique h=3 : il reste 3 unites
  expect_equal(hauteur_utilisee_page(d, p$id), 3L)
})

test_that("ajouter_element refuse graphique (h=3) + 2eme graphique (h=3)", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  d <- ajouter_ligne(d, p$id)

  # 1er graphique sur ligne 1 (12 col, h=3)
  id_l1 <- d$pages[[1]]$lignes[[1]]$id
  el_g1 <- creer_element("graphique", id_kpi = "G1", taille_largeur = 12,
                          hauteur_unites = 3L)
  d <- ajouter_element(d, p$id, id_l1, el_g1)

  # 2eme graphique sur ligne 2 (12 col, h=3) : total = 6 unites
  id_l2 <- d$pages[[1]]$lignes[[2]]$id
  el_g2 <- creer_element("graphique", id_kpi = "G2", taille_largeur = 12,
                          hauteur_unites = 3L)
  d <- ajouter_element(d, p$id, id_l2, el_g2)
  expect_equal(hauteur_utilisee_page(d, p$id), 6L)
})

test_that("ajouter_element accepte un tableau long en h=3 (12 col)", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("kpi", id_kpi = "T1", taille_largeur = 12,
                       tableau_long = TRUE, hauteur_unites = 3L)
  d <- ajouter_element(d, p$id, id_l, el)
  expect_equal(hauteur_utilisee_page(d, p$id), 3L)
})

# ==============================================================================
# 7. Suppression et modification d'elements
# ==============================================================================

test_that("supprimer_element retire le bon element", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("texte", contenu = "test", taille_largeur = 6)
  d <- ajouter_element(d, p$id, id_l, el)
  expect_equal(length(d$pages[[1]]$lignes[[1]]$elements), 1)

  d <- supprimer_element(d, p$id, id_l, el$id)
  expect_equal(length(d$pages[[1]]$lignes[[1]]$elements), 0)
})

test_that("modifier_element change les proprietes de base", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("texte", contenu = "ancien", taille_largeur = 6)
  d <- ajouter_element(d, p$id, id_l, el)

  d <- modifier_element(d, p$id, id_l, el$id,
                         list(contenu = "nouveau", taille = 12))
  el_modifie <- d$pages[[1]]$lignes[[1]]$elements[[1]]
  expect_equal(el_modifie$contenu, "nouveau")
  expect_equal(el_modifie$taille, 12)
})

test_that("modifier_element refuse taille=4", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("texte", contenu = "x", taille_largeur = 6)
  d <- ajouter_element(d, p$id, id_l, el)

  expect_error(
    modifier_element(d, p$id, id_l, el$id, list(taille = 4)),
    "invalide")
})

test_that("modifier_element bascule tableau_long -> force 12 et h=3", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("kpi", id_kpi = "T1", taille_largeur = 6,
                       hauteur_unites = 2L)
  d <- ajouter_element(d, p$id, id_l, el)

  d <- modifier_element(d, p$id, id_l, el$id,
                         list(tableau_long = TRUE))
  el_mod <- d$pages[[1]]$lignes[[1]]$elements[[1]]
  expect_true(el_mod$tableau_long)
  expect_equal(el_mod$taille, 12L)
  expect_equal(el_mod$hauteur_unites, HAUTEUR_TABLEAU_LONG)
})

test_that("modifier_element bascule tableau_long=FALSE -> retour h=2", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("kpi", id_kpi = "T1", taille_largeur = 12,
                       tableau_long = TRUE, hauteur_unites = 3L)
  d <- ajouter_element(d, p$id, id_l, el)

  d <- modifier_element(d, p$id, id_l, el$id,
                         list(tableau_long = FALSE))
  el_mod <- d$pages[[1]]$lignes[[1]]$elements[[1]]
  expect_false(el_mod$tableau_long)
  expect_equal(el_mod$hauteur_unites, HAUTEUR_TABLEAU)
})

test_that("modifier_element refuse tableau_long si largeur 12 occupee", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  # KPI simple en 6 col
  el1 <- creer_element("kpi", id_kpi = "K1", taille_largeur = 6,
                        hauteur_unites = 1L)
  d <- ajouter_element(d, p$id, id_l, el1)

  # Tableau compact en 6 col
  el2 <- creer_element("kpi", id_kpi = "T1", taille_largeur = 6,
                        hauteur_unites = 2L)
  d <- ajouter_element(d, p$id, id_l, el2)

  # Tenter de passer el2 en tableau long : impossible car el1 occupe 6 col
  expect_error(
    modifier_element(d, p$id, id_l, el2$id, list(tableau_long = TRUE)),
    "deja occupee")
})

test_that("deplacer_element echange deux elements", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
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

# ==============================================================================
# 8. verifier_placement
# ==============================================================================

test_that("verifier_placement accepte un element en place", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el <- creer_element("texte", contenu = "x", taille_largeur = 6,
                       hauteur_unites = 1L)
  expect_true(verifier_placement(d, p$id, id_l, el))
})

test_that("verifier_placement refuse si largeur saturée", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  d <- ajouter_ligne(d, p$id)
  id_l <- d$pages[[1]]$lignes[[1]]$id

  el1 <- creer_element("texte", contenu = "a", taille_largeur = 12,
                        hauteur_unites = 1L)
  d <- ajouter_element(d, p$id, id_l, el1)

  el2 <- creer_element("texte", contenu = "b", taille_largeur = 3,
                        hauteur_unites = 1L)
  expect_error(verifier_placement(d, p$id, id_l, el2), "pleine")
})

test_that("verifier_placement refuse si hauteur saturée", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  # 6 lignes de h=1
  for (i in 1:6) d <- ajouter_ligne(d, p$id)
  for (i in 1:6) {
    id_l <- d$pages[[1]]$lignes[[i]]$id
    el <- creer_element("texte", contenu = paste0("t", i),
                         taille_largeur = 12, hauteur_unites = 1L)
    d <- ajouter_element(d, p$id, id_l, el)
  }
  # Ajout d'une 7eme ligne
  d <- ajouter_ligne(d, p$id)
  id_l7 <- d$pages[[1]]$lignes[[7]]$id
  el7 <- creer_element("texte", contenu = "x", taille_largeur = 12,
                        hauteur_unites = 1L)
  expect_error(verifier_placement(d, p$id, id_l7, el7), "pleine")
})

test_that("verifier_placement refuse une ligne introuvable", {
  d <- creer_dashboard()
  p <- creer_page("Page 1")
  d <- ajouter_page(d, p)
  el <- creer_element("texte", contenu = "x", taille_largeur = 6)
  expect_error(verifier_placement(d, p$id, "L_inconnu", el), "introuvable")
})

# ==============================================================================
# 9. resumer_dashboard
# ==============================================================================

test_that("resumer_dashboard retourne un data.frame", {
  d <- creer_dashboard()
  p1 <- creer_page("Page 1")
  d <- ajouter_page(d, p1)
  d <- ajouter_ligne(d, p1$id)

  res <- resumer_dashboard(d)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_true("titre" %in% names(res))
})

test_that("resumer_dashboard gere un dashboard vide", {
  res <- resumer_dashboard(creer_dashboard())
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})