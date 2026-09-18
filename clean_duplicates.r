# clean_duplicates.R - Retire toutes les definitions locales de %||%
# ==============================================================================

cat("Recherche des definitions de %||% dans R/...\n\n")

fichiers <- list.files("R", pattern = "\\.R$", full.names = TRUE)
trouves <- character(0)
nettoyes <- 0

for (f in fichiers) {
  lignes <- readLines(f, warn = FALSE)

  # Chercher les lignes qui definissent %||%
  pattern <- "`%\\|\\|%`\\s*<-\\s*function"
  idx <- grep(pattern, lignes)

  if (length(idx) > 0) {
    trouves <- c(trouves, f)

    # Sauf utils.R qu'on garde
    if (basename(f) == "utils.R") {
      cat(sprintf("  [GARDE] %s : %d definition(s)\n", f, length(idx)))
      next
    }

    # Retirer les lignes
    lignes <- lignes[-idx]
    writeLines(lignes, f, useBytes = TRUE)
    nettoyes <- nettoyes + 1
    cat(sprintf("  [NETTOYE] %s : %d definition(s) retiree(s)\n", f, length(idx)))
  }
}

cat("\n============================================\n")
cat(sprintf("Fichiers analyses : %d\n", length(fichiers)))
cat(sprintf("Fichiers modifies : %d\n", nettoyes))
cat("============================================\n\n")

if (nettoyes > 0) {
  cat("Nettoyage termine. Relancez run.R et les tests.\n")
} else {
  cat("Aucun fichier a nettoyer.\n")
}

# Verification finale
cat("\nVerification : ou reste-t-il des definitions de %||% ?\n")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  lignes <- readLines(f, warn = FALSE)
  if (any(grepl("`%\\|\\|%`\\s*<-\\s*function", lignes))) {
    cat(sprintf("  - %s\n", f))
  }
}