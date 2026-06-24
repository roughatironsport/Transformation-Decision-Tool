# =============================================================================
# app.R - Einstiegspunkt fuer das Deployment (z. B. shinyapps.io)
# =============================================================================
# Warum diese Datei?
#   shinyapps.io laeuft auf Linux mit case-sensitivem Dateisystem. Die in
#   diesem Projekt klein geschriebenen Dateien "ui.r"/"server.r" werden dort
#   NICHT automatisch als Shiny-App erkannt (Shiny sucht "app.R" bzw.
#   "ui.R"/"server.R"). Dieser Wrapper laedt beide Dateien explizit per
#   source() (exakter Dateiname, daher case-unkritisch) und baut die App
#   zusammen. Lokal unter Windows funktioniert er genauso.
#
# Hinweis zu Secrets:
#   Die Zugangsdaten (MONGODB_*, SDF_APP_*) werden weiterhin aus
#   Umgebungsvariablen gelesen. Beim Deployment werden sie ueber den
#   envVars-Mechanismus von rsconnect sicher uebertragen und stehen auf
#   shinyapps.io als Umgebungsvariablen zur Verfuegung.
# =============================================================================

source("ui.r")      # definiert das Objekt `ui` (secure_app-Wrapper, shinymanager)
source("server.r")  # definiert die Funktion `server`

shiny::shinyApp(ui = ui, server = server)
