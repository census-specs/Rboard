# Avant (ne lance rien avec source())
shinyApp(ui, server)

# Apres (lance l'app)
print(shinyApp(ui, server))