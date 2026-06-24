# CLAUDE.md

Leitfaden für Claude Code (und andere Agenten) zur Arbeit an diesem Repository.

## Projekt in einem Satz

**Skill-Demand-Forecasting (SDF)** – eine R-Shiny-Webanwendung als quantitative
Entscheidungshilfe für digitale Transformations-/Change-Projekte. Experten schätzen
per Schieberegler ein, wie gut vorhandene Mitarbeiter-Skills zu zukünftigen
Anforderungen passen; das Tool fittet Verteilungen und gibt eine Ampel-Auswertung aus.

> Status: **Showcase / Archiv** (Projekt von 2021). Die ursprüngliche MongoDB existiert
> nicht mehr. Für den Betrieb muss ein eigenes MongoDB-Atlas-Cluster angebunden werden
> (siehe „MongoDB" unten). Code/Architektur dienen als Referenz bzw. Arbeitsprobe.

## Tech-Stack

- **Sprache:** R (≥ 4.0; lokal installiert: R-4.5.2 unter `C:\Program Files\R\`)
- **Framework:** R Shiny (Single-Page-App)
- **Auth:** `shinymanager` (Login + Auto-Logout nach 5 min Inaktivität)
- **Persistenz:** MongoDB Atlas via `mongolite`
- **Lokaler Cache:** SQLite via `RSQLite` + `pool` (`db.sqlite`, wird zur Laufzeit neu aufgebaut)
- **Statistik:** `sn`, `triangle`, `EnvStats`, `stats` (Normal-, schiefe Normal-, Gleich-, Dreieck-, Paretoverteilung)
- **Viz/Tabellen:** `ggplot2`, `DT`; UI-Helfer: `shinyjs`, `shinyBS`, `shinyWidgets`, `shinyalert`, `shinythemes`

## Wichtige Dateien

| Pfad | Zweck |
|---|---|
| `app.R` | **Einstiegspunkt.** Lädt `ui.r`/`server.r` per `source()` und baut die App (`shinyApp`). Nötig fürs Deployment auf Linux (case-sensitiv) – siehe „Deployment". |
| `ui.r` | Benutzeroberfläche (eine Seite, 4 Bedienschritte). Enthält u. a. eine **vendored `setSliderColor`**-Funktion (Paketversion ist nur noch ein Stub) und einen **eigenen Logout-Button** (shinymanager-FAB via `fab_position = "none"` abgeschaltet). |
| `server.r` | Serverlogik, Statistik-Fits, DB-Anbindung (~2000 Zeilen) |
| `helpcode/helpmongo.r` | **Nicht Teil der App.** Admin-/Seeding-Skript für die MongoDB |
| `Data/Skillpool.xlsx` | Initialer Skill-Pool zur Erstbefüllung |
| `www/` | Statische HTML-Texte/Bilder (aus Word exportiert, Quellen in `word/`) |
| `.Renviron.example` | Vorlage für lokale Secrets (nach `.Renviron` kopieren) |
| `db.sqlite` | Lokaler Laufzeit-Cache (gitignored, wird neu erzeugt) |

## Ausführen

```r
# im Projektverzeichnis, R/RStudio
shiny::runApp()   # nutzt automatisch app.R
```

Per CLI (Beispiel): `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e "shiny::runApp(port=3838, launch.browser=FALSE)"`

Voraussetzung: gesetzte Umgebungsvariablen (siehe unten) und installierte Pakete
(Liste im README unter „Pakete installieren"). Ohne gültige `MONGODB_*`-Variablen
bricht die App beim Start bewusst mit einer klaren Fehlermeldung ab (`server.r`, ~Z. 609).

## Konfiguration / Secrets

Alle Zugangsdaten kommen aus Umgebungsvariablen — **keine Passwörter im Code**.
`.Renviron.example` nach `.Renviron` kopieren (gitignored) und ausfüllen:

| Variable | Bedeutung |
|---|---|
| `MONGODB_HOST` | Atlas-Cluster-Host, z. B. `cluster0.xxxxx.mongodb.net` (ohne `mongodb+srv://`) |
| `MONGODB_USER` | DB-Benutzer |
| `MONGODB_PASSWORD` | DB-Passwort |
| `SDF_APP_USER` | App-Login-Benutzer (shinymanager) |
| `SDF_APP_PASSWORD` | App-Login-Passwort |

Nach Änderungen R/RStudio neu starten, damit `.Renviron` neu geladen wird.

## MongoDB

Verbindung wird in `server.r` und `helpcode/helpmongo.r` identisch gebaut:

```
mongodb+srv://<USER>:<PASSWORD>@<HOST>/<DATABASE>
```
(mit `ssl_options(weak_cert_validation = TRUE)`).

**Genutzte Datenbanken / Collections:**

| Datenbank | Collection | Genutzt von | Zugriff |
|---|---|---|---|
| `allusers` | `stats` | **Laufende App** (`server.r`) + Seed-Skript | Lesen **und** Schreiben |
| `mydata` | `responses` | Nur Seed-Skript `helpmongo.r` | Lesen/Schreiben (optional) |

Wichtig: Schreiben erfolgt als **`drop()` + `insert()`** der ganzen Collection
(Konsistenz; siehe `saveData()`). Der DB-User braucht daher Lese-, Schreib- und
**Drop**-Rechte auf der/den Datenbank(en).

**Erstbefüllung:** In `helpcode/helpmongo.r` am Dateiende die `saveData(...)`-Aufrufe
einkommentieren und einmal ausführen (Arbeitsverzeichnis = Projektverzeichnis).

## Konventionen

- Kernlogik liegt in kleingeschriebenen `server.r`/`ui.r`; `app.R` ist nur ein dünner Wrapper, der beide lädt (Linux-Kompatibilität fürs Deployment).
- Spalten-/Variablennamen und Kommentare sind **deutsch**, teils mit Umlauten
  (z. B. `absolute_Häufigkeit`, `Schwellenwert_Mean`). Beim Editieren UTF-8 beibehalten.
- Statistik-Kennwerte werden **inkrementell** fortgeschrieben (laufender Mittelwert/SD),
  Suffixe markieren den Verteilungstyp: `_nv _snv _u _d _p _ip` (siehe `helpmongo.r`-Header).
- Es werden **nur anonyme, aggregierte** Kennwerte gespeichert, keine Einzeleingaben.
- Secrets niemals committen. `.Renviron`, `db.sqlite`, `rsconnect/` sind gitignored.

## Deployment (shinyapps.io)

Live: **https://projektplanung.shinyapps.io/projektplanung/** (Account `projektplanung`, App-id 2915742).

Deploy via `rsconnect::deployApp(...)`. Wichtige, **verifizierte** Besonderheiten:

- **Einstiegspunkt `app.R`** zwingend: shinyapps.io läuft auf Linux (case-sensitiv) und
  erkennt die lowercase `ui.r`/`server.r` nicht automatisch.
- **Secrets:** shinyapps.io unterstützt `deployApp(envVars=…)` **nicht** (nur Posit Connect).
  Stattdessen die `.Renviron` **ins Bundle** aufnehmen (`appFiles` inkl. `.Renviron`). Sie ist
  nicht über die URL erreichbar (nur `www/` wird als Asset ausgeliefert). Folge:
  Secret-Änderung erfordert ein Re-Deploy.
- **Schlankes Bundle:** nur `app.R`, `ui.r`, `server.r`, `.Renviron`, `www/`
  (kein `db.sqlite`, `Data/`, `helpcode/`, `word/`, `docs/`).
- **Atlas-IP-Allowlist** muss `0.0.0.0/0` enthalten — shinyapps.io hat dynamische Ausgangs-IPs.
- **Windows-Stolperstein:** R's libcurl scheitert hier am TLS-Handshake (SSL-Inspection). Vor
  `install.packages()` **und** vor `deployApp()` (renv-Abhängigkeitserfassung) jeweils
  `options(download.file.method='wininet', url.method='wininet')` setzen.
- shinyapps.io führt pro App nur **eine** Aufgabe gleichzeitig aus. Während ein Deploy läuft,
  schlägt ein paralleler Restart mit „Unable to dispatch task … 1 tasks already in progress"
  fehl (transient) — einfach abwarten und erneut versuchen.

Beispiel-Deploy:

```r
options(download.file.method = "wininet", url.method = "wininet")
library(rsconnect)
setAccountInfo(name = "projektplanung", token = "…", secret = "…")  # aus shinyapps.io → Tokens
files <- c("app.R", "ui.r", "server.r", ".Renviron",
           list.files("www", recursive = TRUE, full.names = TRUE))
deployApp(appName = "projektplanung", appPrimaryDoc = "app.R",
          appFiles = files, forceUpdate = TRUE)
```
