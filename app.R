# Carga librerías y recursos
source("global.R", local = TRUE)  # Opcional para código compartido

# Ejecuta la app
shinyApp(
  ui = source("ui.R", local = TRUE)$value,
  server = source("server.R", local = TRUE)$value
)