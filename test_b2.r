# ==============================================================================
# test_b2.R - Verification du chantier B2 (Fiabilite)
# ==============================================================================
# Usage : dans la console R, apres setwd(".../Rboard")
#   source("test_b2.R")
# ==============================================================================

cat("\n========================================\n")
cat("TEST B2 - FIABILITE\n")
cat("========================================\n\n")

# --- Charger les sources ---
suppressMessages({
  source("R/utils.R")
  source("R/projet.R")
  source("R/import.R")
  source("R/validators.R")
  source("R/kpi.R")
  source("R/stats.R")
  source("R/graphiques.R")
  source("R/dashboard.R")
})

# --- Jeu de donnees fictif ---
set.seed(42)
donnees_test <- data.frame(
  age = round(rnorm(60, 40, 10)),
  sexe = factor(rep(c("H", "F"), 30)),
  score = round(rnorm(60, 50, 15), 1),
  groupe = factor(rep(c("A", "B", "C"), 20))
)

# ==============================================================================
# Test 1 : Validations
# ==============================================================================
cat("--- Test 1 : Validations ---\n")

test_ok <- function(label, expr) {
  res <- tryCatch({ expr; "OK" }, error = function(e) paste("ECHEC:", conditionMessage(e)))
  cat(sprintf("  %-55s %s\n", label, res))
}

test_ok("valider_colonne valide",
        valider_colonne(donnees_test, "age"))
test_ok("valider_colonne invalide -> doit echouer",
        tryCatch({ valider_colonne(donnees_test, "inexistant"); stop("aurait du echouer") },
                 error = function(e) if (grepl("n'existe pas", conditionMessage(e))) "OK" else stop("mauvais message")))
test_ok("valider_numerique sur age",
        valider_numerique(donnees_test$age, "age"))
test_ok("valider_numerique sur sexe -> doit echouer",
        tryCatch({ valider_numerique(donnees_test$sexe, "sexe"); stop("aurait du echouer") },
                 error = function(e) if (grepl("pas numerique", conditionMessage(e))) "OK" else stop("mauvais message")))
test_ok("valider_groupe 2 modalites",
        valider_groupe(donnees_test$sexe, "sexe", 2, 2))
test_ok("valider_groupe 3 modalites -> doit echouer avec max 2",
        tryCatch({ valider_groupe(donnees_test$groupe, "groupe", 2, 2); stop("aurait du echouer") },
                 error = function(e) if (grepl("maximum 2", conditionMessage(e))) "OK" else stop("mauvais message")))
test_ok("valider_texte vide -> doit echouer",
        tryCatch({ valider_texte("", "nom"); stop("aurait du echouer") },
                 error = function(e) if (grepl("obligatoire", conditionMessage(e))) "OK" else stop("mauvais message")))

# ==============================================================================
# Test 2 : Statistiques
# ==============================================================================
cat("\n--- Test 2 : Statistiques ---\n")

test_ok("kpi_moyenne valide",
        { k <- kpi_moyenne(donnees_test, "age"); stopifnot(!is.null(k$id)) })
test_ok("kpi_moyenne sur variable inexistante -> doit echouer",
        tryCatch({ kpi_moyenne(donnees_test, "inexistant"); stop("aurait du echouer") },
                 error = function(e) if (grepl("n'existe pas", conditionMessage(e))) "OK" else stop("mauvais message")))
test_ok("kpi_test_t valide (2 groupes)",
        { k <- kpi_test_t(donnees_test, "age", "sexe"); stopifnot(length(k) == 4) })
test_ok("kpi_test_t avec 3 groupes -> doit echouer",
        tryCatch({ kpi_test_t(donnees_test, "age", "groupe"); stop("aurait du echouer") },
                 error = function(e) if (grepl("maximum 2", conditionMessage(e))) "OK" else stop("mauvais message")))

# ==============================================================================
# Test 3 : Graphiques
# ==============================================================================
cat("\n--- Test 3 : Graphiques ---\n")

test_ok("construire_graphique valide",
        { g <- construire_graphique(donnees_test, x = "sexe", y = "age", type = "Boxplot");
          stopifnot(inherits(g, "ggplot")) })
test_ok("construire_graphique X = Y -> doit echouer",
        tryCatch({ construire_graphique(donnees_test, x = "age", y = "age", type = "Nuage de points");
                   stop("aurait du echouer") },
                 error = function(e) if (grepl("differentes", conditionMessage(e))) "OK" else stop("mauvais message")))

# ==============================================================================
# Test 4 : Sauvegarde / chargement
# ==============================================================================
cat("\n--- Test 4 : Sauvegarde et chargement ---\n")

p <- nouveau_projet("Test B2")
p$donnees <- donnees_test
p <- ajouter_kpi(p, kpi_moyenne(donnees_test, "age"))
g <- construire_graphique(donnees_test, x = "sexe", y = "age", type = "Boxplot")
g_kpi <- creer_kpi_graphique(nom = "Boxplot age par sexe", objet_ggplot = g)
p <- ajouter_graphique(p, g_kpi)

chemin <- tempfile(fileext = ".rds")
sauver_projet(p, chemin)

test_ok("Sauvegarde reussie",
        { stopifnot(file.exists(chemin)) })

p2 <- charger_projet(chemin)

test_ok("Chargement : nom preserve",
        { stopifnot(p2$nom == "Test B2") })
test_ok("Chargement : 1 KPI present",
        { stopifnot(length(p2$kpi) == 1) })
test_ok("Chargement : 1 graphique present",
        { stopifnot(length(p2$graphiques) == 1) })
test_ok("Chargement : objet ggplot preserve",
        { stopifnot(inherits(p2$graphiques[[1]]$objet, "ggplot")) })

# ==============================================================================
# Test 5 : Verification de projet
# ==============================================================================
cat("\n--- Test 5 : Verification de projet ---\n")

test_ok("verifier_projet retourne TRUE",
        { res <- verifier_projet(p2); stopifnot(isTRUE(res)) })

p_casse <- p2
p_casse$graphiques[[1]]$objet <- NULL

test_ok("verifier_projet detecte graphique casse",
        { res <- verifier_projet(p_casse);
          stopifnot(is.character(res), length(res) > 0,
                    any(grepl("ggplot manquant", res))) })

# ==============================================================================
# Test 6 : Helpers de messages
# ==============================================================================
cat("\n--- Test 6 : Helpers de messages ---\n")

test_ok("rboard_ok",
        { m <- rboard_ok("Reussi"); stopifnot(m$type == "success") })
test_ok("rboard_ko",
        { m <- rboard_ko("Echec"); stopifnot(m$type == "danger") })
test_ok("rboard_try capture erreur",
        { res <- rboard_try(stop("test"), "l'operation");
          stopifnot(res$ok == FALSE, !is.null(res$message)) })
test_ok("rboard_try capture succes",
        { res <- rboard_try(42, "l'operation");
          stopifnot(res$ok == TRUE, res$valeur == 42) })

# ==============================================================================
# Test 7 : Grille 12 x 6 - Constantes et helpers
# ==============================================================================
cat("\n--- Test 7 : Grille 12 x 6 - Constantes et helpers ---\n")

test_ok("RBOARD_GRILLE_LARGEUR = 12",
        { stopifnot(RBOARD_GRILLE_LARGEUR == 12L) })
test_ok("RBOARD_GRILLE_HAUTEUR = 6",
        { stopifnot(RBOARD_GRILLE_HAUTEUR == 6L) })
test_ok("HAUTEUR_GRAPHIQUE = 3",
        { stopifnot(HAUTEUR_GRAPHIQUE == 3L) })
test_ok("HAUTEUR_TABLEAU_LONG = 3",
        { stopifnot(HAUTEUR_TABLEAU_LONG == 3L) })
test_ok("LARGEURS_STANDARD = c(3, 6, 12)",
        { stopifnot(identical(LARGEURS_STANDARD, c(3L, 6L, 12L))) })
test_ok("Largeur 4 absente partout",
        { stopifnot(!4L %in% LARGEURS_STANDARD,
                    !4L %in% LARGEURS_LARGES) })

test_ok("rboard_hauteur_element : graphique = 3",
        { stopifnot(rboard_hauteur_element("graphique") == 3L) })
test_ok("rboard_hauteur_element : tableau = 2",
        { stopifnot(rboard_hauteur_element("kpi", list(type = "tableau")) == 2L) })
test_ok("rboard_hauteur_element : tableau long = 3",
        { stopifnot(rboard_hauteur_element("kpi", list(type = "tableau"),
                                            tableau_long = TRUE) == 3L) })

# ==============================================================================
# Test 8 : Migration grille 4 -> 6
# ==============================================================================
cat("\n--- Test 8 : Migration grille 4 -> 6 ---\n")

# --- On fabrique un ancien projet (grille 4 x 12) avec des defauts volontaires ---
ancien_projet <- list(
  nom = "Ancien projet (grille 4)",
  date_creation = Sys.Date(),
  date_modification = Sys.Date(),
  donnees = donnees_test,
  kpi = list(
    K1 = list(
      id = "K1", nom = "Moyenne age", valeur = 40, type = "moyenne",
      label = "Moyenne age = 40,00", couleur = "primary",
      format = "decimal_2", source = "moyenne",
      regles_couleur = list(), date_creation = Sys.Date()
    ),
    T1 = list(
      id = "T1", nom = "Ventes par region",
      type = "tableau",
      tableau = data.frame(
        Region = c("Nord", "Sud", "Est", "Ouest", "Centre",
                   "Nord-Est", "Sud-Ouest", "Nord-Ouest", "Centre-Est", "Autre"),
        Ventes = c(100, 200, 150, 175, 125, 90, 80, 110, 95, 60),
        stringsAsFactors = FALSE),
      sous_type = "par_groupe", mode_affichage = "effectifs",
      colonne_valeur_index = 2, stats_annexes = list(),
      commentaire = NULL, couleur = "primary",
      regles_couleur = list(), source = "tableau", format = "texte",
      groupe = NULL, date_creation = Sys.Date()
    )
  ),
  graphiques = list(
    G1 = list(
      id = "G1", nom = "Boxplot age par sexe", type = "graphique",
      type_graphique = "Boxplot",
      objet = construire_graphique(donnees_test, x = "sexe", y = "age",
                                    type = "Boxplot"),
      code = NULL, commentaire = NULL, groupe = NULL,
      date_creation = Sys.Date()
    )
  ),
  dashboard = list(
    pages = list(
      list(
        id = "P001", titre = "Page 1",
        lignes = list(
          list(
            id = "L001",
            elements = list(
              # Element KPI avec ancienne taille 4 (n'existe plus)
              list(id = "E001", type = "kpi", id_kpi = "K1",
                   taille = 4, style = "normal",
                   nom_affiche = NULL, commentaire = NULL,
                   couleur_forcee = NULL, niveau_titre = NULL),
              # Element tableau : taille 6, PAS de hauteur_unites ni tableau_long
              list(id = "E002", type = "kpi", id_kpi = "T1",
                   taille = 6, style = "normal",
                   nom_affiche = NULL, commentaire = NULL,
                   couleur_forcee = NULL, niveau_titre = NULL)
            )
          ),
          list(
            id = "L002",
            elements = list(
              # Element graphique : taille 6, sans hauteur_unites
              list(id = "E003", type = "graphique", id_kpi = "G1",
                   taille = 6, style = "normal",
                   nom_affiche = NULL, commentaire = NULL,
                   couleur_forcee = NULL, niveau_titre = NULL)
            )
          )
        )
      )
    )
  ),
  commentaires = list()
)

chemin_ancien <- tempfile(fileext = ".rds")
saveRDS(ancien_projet, chemin_ancien)

p_migre <- charger_projet(chemin_ancien)

# --- Recuperation des elements apres migration ---
el_kpi    <- p_migre$dashboard$pages[[1]]$lignes[[1]]$elements[[1]]
el_tab    <- p_migre$dashboard$pages[[1]]$lignes[[1]]$elements[[2]]
el_graph  <- p_migre$dashboard$pages[[1]]$lignes[[2]]$elements[[1]]

test_ok("Migration : KPI avec taille 4 -> taille 6",
        { stopifnot(el_kpi$taille == 6L) })

test_ok("Migration : KPI avec hauteur_unites = 1 (deduit)",
        { stopifnot(el_kpi$hauteur_unites == 1L) })

test_ok("Migration : KPI avec tableau_long = FALSE",
        { stopifnot(isFALSE(el_kpi$tableau_long)) })

test_ok("Migration : Tableau 10 lignes -> tableau_long = TRUE",
        { stopifnot(isTRUE(el_tab$tableau_long)) })

test_ok("Migration : Tableau long -> taille forcee a 12",
        { stopifnot(el_tab$taille == 12L) })

test_ok("Migration : Tableau long -> hauteur_unites = 3",
        { stopifnot(el_tab$hauteur_unites == 3L) })

test_ok("Migration : Graphique -> hauteur_unites = 3",
        { stopifnot(el_graph$hauteur_unites == 3L) })

test_ok("Migration : Graphique -> tableau_long = FALSE",
        { stopifnot(isFALSE(el_graph$tableau_long)) })

test_ok("Migration : Graphique conserve sa taille 6",
        { stopifnot(el_graph$taille == 6L) })

test_ok("Migration : KPI de type tableau conserve son type",
        { stopifnot(identical(p_migre$kpi$T1$type, "tableau")) })

# --- Verification de la coherence apres migration ---
test_ok("Migration : projet coherent apres migration",
        { res <- verifier_projet(p_migre); stopifnot(isTRUE(res)) })

test_ok("Migration : largeur ligne 1 <= 12",
        { larg <- sum(sapply(p_migre$dashboard$pages[[1]]$lignes[[1]]$elements,
                              function(e) e$taille))
          stopifnot(larg <= 12L) })

test_ok("Migration : hauteur ligne 1 respecte le max des elements",
        { hs <- sapply(p_migre$dashboard$pages[[1]]$lignes[[1]]$elements,
                       function(e) e$hauteur_unites)
          stopifnot(max(hs) == 3L) })

# ==============================================================================
# Test 9 : Migration idempotente
# ==============================================================================
cat("\n--- Test 9 : Migration idempotente ---\n")

# Re-migration : ne doit rien changer
p_migre2 <- reparer_projet(p_migre)

test_ok("Re-migration : identique a la premiere",
        { stopifnot(identical(p_migre, p_migre2)) })

# ==============================================================================
# Test 10 : Migration d'un projet sain (ne casse rien)
# ==============================================================================
cat("\n--- Test 10 : Migration d'un projet sain ---\n")

# --- Projet neuf avec tous les champs deja corrects ---
p_sain <- nouveau_projet("Projet sain")
p_sain$dashboard <- creer_dashboard()
p_sain$dashboard <- ajouter_page(p_sain$dashboard, creer_page("Page 1"))
p_sain$dashboard <- ajouter_ligne(p_sain$dashboard,
                                   p_sain$dashboard$pages[[1]]$id)
id_l <- p_sain$dashboard$pages[[1]]$lignes[[1]]$id

el_sain <- creer_element("texte", contenu = "Deja bon",
                          taille_largeur = 6, hauteur_unites = 1L)
p_sain$dashboard <- ajouter_element(p_sain$dashboard,
                                     p_sain$dashboard$pages[[1]]$id,
                                     id_l, el_sain)

el_avant <- p_sain$dashboard$pages[[1]]$lignes[[1]]$elements[[1]]
p_sain_repare <- reparer_projet(p_sain)
el_apres <- p_sain_repare$dashboard$pages[[1]]$lignes[[1]]$elements[[1]]

test_ok("Projet sain : taille inchangee",
        { stopifnot(el_avant$taille == el_apres$taille) })

test_ok("Projet sain : hauteur_unites inchangee",
        { stopifnot(el_avant$hauteur_unites == el_apres$hauteur_unites) })

test_ok("Projet sain : tableau_long inchange",
        { stopifnot(el_avant$tableau_long == el_apres$tableau_long) })

test_ok("Projet sain : contenu preserve",
        { stopifnot(el_apres$contenu == "Deja bon") })

# ==============================================================================
# Test 11 : resumer_projet
# ==============================================================================
cat("\n--- Test 11 : resumer_projet ---\n")

res <- resumer_projet(p_migre)

test_ok("resumer_projet : nom preserve",
        { stopifnot(identical(res$nom, "Ancien projet (grille 4)")) })
test_ok("resumer_projet : compte 2 KPI",
        { stopifnot(res$nb_kpi == 2L) })
test_ok("resumer_projet : compte 1 graphique",
        { stopifnot(res$nb_graphiques == 1L) })
test_ok("resumer_projet : compte 1 page",
        { stopifnot(res$nb_pages == 1L) })
test_ok("resumer_projet : compte 2 lignes",
        { stopifnot(res$nb_lignes == 2L) })
test_ok("resumer_projet : compte 3 elements",
        { stopifnot(res$nb_elements == 3L) })

# ==============================================================================
# Nettoyage
# ==============================================================================
unlink(chemin)
unlink(chemin_ancien)

cat("\n========================================\n")
cat("FIN DES TESTS B2\n")
cat("========================================\n\n")