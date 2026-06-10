# =============================================================================
# helpmongo.r - Administrations-/Hilfsskript fuer die MongoDB des SDF-Tools
# =============================================================================
# Zweck:
#   Dieses Skript wird NICHT von der Shiny-App verwendet. Es dient dem
#   einmaligen Befuellen (Seeding) bzw. dem manuellen Pruefen der MongoDB:
#     - Liest den initialen Skill-Pool aus Data/Skillpool.xlsx ein
#     - Erzeugt eine Beispiel-Statistik-Tabelle ueber alle Nutzer ("alluser")
#     - Stellt saveData()/loadData() zum Schreiben/Lesen der Collections bereit
#
# Verwendete Datenbanken/Collections:
#   - "mydata"   / "responses": reiner Skill-Pool (Bezeichnung + Beschreibung)
#   - "allusers" / "stats":     Skill-Pool inkl. aggregierter Nutzungsstatistiken
#
# Konfiguration:
#   Die Zugangsdaten werden aus Umgebungsvariablen gelesen (siehe
#   .Renviron.example im Projektverzeichnis). Niemals Klartext-Passwoerter
#   in dieses Skript eintragen oder nach Git einchecken!
# =============================================================================

library(openxlsx)   # Excel-Import des Skill-Pools
library(dbplyr)     # Datenbank-Backend fuer dplyr
library(mongolite)  # MongoDB-Anbindung

# -----------------------------------------------------------------------------
# 1) Initialen Skill-Pool aus Excel einlesen
# -----------------------------------------------------------------------------
responses_df = read.xlsx("./Data/Skillpool.xlsx")

# Optional: jedem Skill eine eindeutige ID (UUID) zuweisen
# datUUID = sapply(seq(dim(responses_df)[1]), UUIDgenerate)
# responses_df$row_id = datUUID

# -----------------------------------------------------------------------------
# 2) Beispiel-Statistikgeruest ueber alle Nutzer aufbauen
# -----------------------------------------------------------------------------
# Fuer jeden Skill werden pro Verteilungstyp folgende Kennwerte gefuehrt
# (jeweils Mittelwert und Standardabweichung, inkrementell fortgeschrieben):
#   - absolute_Häufigkeit: wie oft der Skill ausgewertet wurde
#   - P_Wert:              Erfolgswahrscheinlichkeit p
#   - Schwellenwert:       eingestellter Soll-/Mindestwert
#   - Unteres/Oberes_Quartil: Slider-Eingaben (mittlere 50 % der Mitarbeiter)
#
# Suffixe der Spaltenbloecke (= Verteilungstyp der Eingabe):
#   (ohne) = Gesamt | _nv = Normalverteilung | _snv = schiefe Normalverteilung
#   _u = stetige Gleichverteilung | _d = Dreieckverteilung
#   _p = Paretoverteilung | _ip = Paretoverteilung invers gespiegelt
#
# Die ersten 5 Zeilen enthalten Beispielwerte, der Rest wird mit 0 initialisiert.
alluser = data.frame(
  "absolute_Häufigkeit" = c(8,1,2,3,5,rep(0,length(responses_df[,2])-5)),
  "P_Wert_Mean" = c(0.3,0.4,0.5,0.1,0.11,rep(0,length(responses_df[,2])-5)),
  "P_Wert_SD" = c(0.3,0.2,0.2,0.1,0.2,rep(0,length(responses_df[,2])-5)),
  "Schwellenwert_Mean" = c(60,70,50,40,80,rep(0,length(responses_df[,2])-5)),
  "Schwellenwert_SD" = c(3,2,2,1,0,rep(0,length(responses_df[,2])-5)),
  "Unteres_Quartil_Mean" = c(30,50,30,20,40,rep(0,length(responses_df[,2])-5)),
  "Unteres_Quartil_SD" = c(3,2,2,10,20,rep(0,length(responses_df[,2])-5)),
  "Oberes_Quartil_Mean" = c(60,70,50,40,80,rep(0,length(responses_df[,2])-5)),
  "Oberes_Quartil_SD" = c(3,2,2,10,20,rep(0,length(responses_df[,2])-5)),

  "absolute_Häufigkeit_nv" = rep(0,length(responses_df[,2])),
  "P_Wert_Mean_nv" = rep(0,length(responses_df[,2])),
  "P_Wert_SD_nv" = rep(0,length(responses_df[,2])),
  "Schwellenwert_Mean_nv" = rep(0,length(responses_df[,2])),
  "Schwellenwert_SD_nv" = rep(0,length(responses_df[,2])),
  "Unteres_Quartil_Mean_nv" = rep(0,length(responses_df[,2])),
  "Unteres_Quartil_SD_nv" = rep(0,length(responses_df[,2])),
  "Oberes_Quartil_Mean_nv" = rep(0,length(responses_df[,2])),
  "Oberes_Quartil_SD_nv" = rep(0,length(responses_df[,2])),

  "absolute_Häufigkeit_snv" = rep(0,length(responses_df[,2])),
  "P_Wert_Mean_snv" = rep(0,length(responses_df[,2])),
  "P_Wert_SD_snv" = rep(0,length(responses_df[,2])),
  "Schwellenwert_Mean_snv" = rep(0,length(responses_df[,2])),
  "Schwellenwert_SD_snv" = rep(0,length(responses_df[,2])),
  "Unteres_Quartil_Mean_snv" = rep(0,length(responses_df[,2])),
  "Unteres_Quartil_SD_snv" = rep(0,length(responses_df[,2])),
  "Oberes_Quartil_Mean_snv" = rep(0,length(responses_df[,2])),
  "Oberes_Quartil_SD_snv" = rep(0,length(responses_df[,2])),

  "absolute_Häufigkeit_u" = rep(0,length(responses_df[,2])),
  "P_Wert_Mean_u" = rep(0,length(responses_df[,2])),
  "P_Wert_SD_u" = rep(0,length(responses_df[,2])),
  "Schwellenwert_Mean_u" = rep(0,length(responses_df[,2])),
  "Schwellenwert_SD_u" = rep(0,length(responses_df[,2])),
  "Unteres_Quartil_Mean_u" = rep(0,length(responses_df[,2])),
  "Unteres_Quartil_SD_u" = rep(0,length(responses_df[,2])),
  "Oberes_Quartil_Mean_u" = rep(0,length(responses_df[,2])),
  "Oberes_Quartil_SD_u" = rep(0,length(responses_df[,2])),

  "absolute_Häufigkeit_d" = rep(0,length(responses_df[,2])),
  "P_Wert_Mean_d" = rep(0,length(responses_df[,2])),
  "P_Wert_SD_d" = rep(0,length(responses_df[,2])),
  "Schwellenwert_Mean_d" = rep(0,length(responses_df[,2])),
  "Schwellenwert_SD_d" = rep(0,length(responses_df[,2])),
  "Unteres_Quartil_Mean_d" = rep(0,length(responses_df[,2])),
  "Unteres_Quartil_SD_d" = rep(0,length(responses_df[,2])),
  "Oberes_Quartil_Mean_d" = rep(0,length(responses_df[,2])),
  "Oberes_Quartil_SD_d" = rep(0,length(responses_df[,2])),

  "absolute_Häufigkeit_p" = rep(0,length(responses_df[,2])),
  "P_Wert_Mean_p" = rep(0,length(responses_df[,2])),
  "P_Wert_SD_p" = rep(0,length(responses_df[,2])),
  "Schwellenwert_Mean_p" = rep(0,length(responses_df[,2])),
  "Schwellenwert_SD_p" = rep(0,length(responses_df[,2])),
  "Unteres_Quartil_Mean_p" = rep(0,length(responses_df[,2])),
  "Unteres_Quartil_SD_p" = rep(0,length(responses_df[,2])),
  "Oberes_Quartil_Mean_p" = rep(0,length(responses_df[,2])),
  "Oberes_Quartil_SD_p" = rep(0,length(responses_df[,2])),

  "absolute_Häufigkeit_ip" = rep(0,length(responses_df[,2])),
  "P_Wert_Mean_ip" = rep(0,length(responses_df[,2])),
  "P_Wert_SD_ip" = rep(0,length(responses_df[,2])),
  "Schwellenwert_Mean_ip" = rep(0,length(responses_df[,2])),
  "Schwellenwert_SD_ip" = rep(0,length(responses_df[,2])),
  "Unteres_Quartil_Mean_ip" = rep(0,length(responses_df[,2])),
  "Unteres_Quartil_SD_ip" = rep(0,length(responses_df[,2])),
  "Oberes_Quartil_Mean_ip" = rep(0,length(responses_df[,2])),
  "Oberes_Quartil_SD_ip" = rep(0,length(responses_df[,2]))
)

# -----------------------------------------------------------------------------
# 3) MongoDB-Zugangsdaten aus Umgebungsvariablen laden (siehe .Renviron.example)
# -----------------------------------------------------------------------------
mongodb = list(
  "host"     = Sys.getenv("MONGODB_HOST"),
  "username" = Sys.getenv("MONGODB_USER"),
  "password" = Sys.getenv("MONGODB_PASSWORD")
)

if (mongodb$host == "" || mongodb$username == "" || mongodb$password == "") {
  stop(paste("MongoDB-Konfiguration fehlt: Bitte MONGODB_HOST, MONGODB_USER und",
             "MONGODB_PASSWORD in der Datei .Renviron setzen (Vorlage: .Renviron.example)."))
}

# -----------------------------------------------------------------------------
# 4) Schreib-/Lese-Funktionen fuer die MongoDB
# -----------------------------------------------------------------------------

# Schreibt einen data.frame als Collection in die MongoDB.
# ACHTUNG: Die bestehende Collection wird vorher vollstaendig geloescht (drop),
# damit der Datenbestand konsistent bleibt.
saveData <- function(data, databaseName, collectionName) {
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s",
                mongodb$username,
                mongodb$password,
                mongodb$host,
                databaseName
              ),
              options = ssl_options(weak_cert_validation = TRUE))
  db$drop()
  # transponieren, damit jede Spalte ein Dokument wird (Format der App)
  data <- as.data.frame(t(data))
  db$insert(data)
}

# Liest eine komplette Collection aus der MongoDB und liefert sie als
# (zurücktransponierten) data.frame.
loadData <- function(databaseName, collectionName) {
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s",
                mongodb$username,
                mongodb$password,
                mongodb$host,
                databaseName
              ),
              options = ssl_options(weak_cert_validation = TRUE))
  data <- db$find()
  data = as.data.frame(t(data))
}

# -----------------------------------------------------------------------------
# 5) Beispielaufrufe (zum manuellen Ausfuehren)
# -----------------------------------------------------------------------------

# Aktuellen Stand aus der Datenbank lesen:
df  = loadData("mydata", "responses")
df2 = loadData("allusers", "stats")

# Skill-Pool und Statistikgeruest kombinieren:
neudat = cbind(responses_df, alluser)

# Erstbefuellung der Datenbank (nur bei Bedarf einkommentieren!):
# saveData(responses_df, "mydata", "responses")
# saveData(neudat, "allusers", "stats")
