# Transformation-Decision-Tool

**Skill-Demand-Forecasting (SDF)** – eine R-Shiny-Webanwendung als quantitative
Entscheidungshilfe zur Unterstützung von digitalen Transformationsprozessen
und Change-Projekten.

> **Status: Showcase / Archiv.** Dieses Repository dokumentiert ein
> abgeschlossenes Projekt (2021). Die ursprünglich angebundene
> MongoDB-Instanz existiert nicht mehr – zum tatsächlichen Betrieb müsste
> ein eigenes MongoDB-Atlas-Cluster angelegt und wie unten beschrieben
> konfiguriert werden. Code, Architektur und Dokumentation dienen als
> Referenz bzw. Arbeitsprobe.

Das SDF hilft Projektverantwortlichen einzuschätzen, wie gut die Skills der
derzeitigen Mitarbeiter zu den Anforderungen zukünftiger Projekte passen. Dazu
werden Experteneinschätzungen über Schieberegler erfasst, statistisch modelliert
und als leicht verständliche **Ampel-Auswertung** ausgegeben.

> Entwickelt im Rahmen eines Kooperationsprojekts des Instituts für komplexe
> Systemforschung (IKS) der Hochschule Fresenius.

---

## Funktionsweise (4 Schritte)

1. **Skill-Auswahl** – Auswahl relevanter Skills aus einem zentral gepflegten
   Skill-Pool. Der Pool kann direkt in der App bearbeitet werden
   (Hinzufügen / Ändern / Löschen von Einträgen).
2. **Einschätzung** – Für jeden Skill wird per Slider eingeschätzt:
   - in welchem Bereich die *mittleren 50 %* der derzeitigen Mitarbeiter liegen,
   - welcher *Mindest-Sollwert* für das zukünftige Projekt nötig ist.

   Im optionalen **Advanced-Statistics-Modus** kann zusätzlich der
   Verteilungstyp gewählt werden (Normal-, schiefe Normal-, stetige Gleich-,
   Dreieck- oder Paretoverteilung inkl. Schiefeparameter).
3. **Auswertung je Skill** – Aus den Eingaben wird die gewählte Verteilung
   gefittet (u. a. via Simulated Annealing bzw. numerischer Optimierung) und
   die Wahrscheinlichkeit *p* berechnet, dass der Sollwert erreicht wird.
   Ergebnis: Dichteplot, Erklärtext und Ampel
   (*p* < 0,33 = rot, *p* < 0,66 = gelb, sonst grün).
4. **Gesamtauswertung** – Übersichtstabelle aller Skills mit geometrischem
   Mittelwert der Erfolgswahrscheinlichkeiten, CSV-Download sowie
   (anonymisierte) Nutzungsstatistik über alle Anwender.

Ein Flussdiagramm der gesamten Anwendung liegt unter
[docs/SDF-Flussdiagramm.pdf](docs/SDF-Flussdiagramm.pdf).

## Architektur

| Komponente | Technologie | Zweck |
|---|---|---|
| Frontend / Backend | R Shiny (`ui.r`, `server.r`) | Benutzeroberfläche und Auswertungslogik |
| Authentifizierung | `shinymanager` | Login-Maske, Inaktivitäts-Logout (5 min) |
| Persistente Daten | MongoDB Atlas (`mongolite`) | Skill-Pool + aggregierte Nutzungsstatistiken |
| Lokaler Cache | SQLite (`RSQLite`, `pool`) | schnelle Bearbeitung des Skill-Pools zur Laufzeit |
| Statistik | `sn`, `triangle`, `EnvStats`, `stats` | Verteilungsfits und Wahrscheinlichkeitsberechnung |
| Visualisierung | `ggplot2`, `DT` | Dichteplots, Diagramme, Tabellen |

**Datenfluss:** Beim App-Start wird der Skill-Pool aus der MongoDB geladen und
in eine lokale SQLite-Datenbank (`db.sqlite`, wird automatisch erzeugt)
gespiegelt. Änderungen am Skill-Pool sowie die aggregierten
Auswertungsergebnisse werden in die MongoDB zurückgeschrieben. Es werden dabei
nur **anonyme, aggregierte Kennwerte** gespeichert (laufende Mittelwerte /
Standardabweichungen), keine Einzeleingaben.

## Projektstruktur

```
├── ui.r                  # Benutzeroberfläche (eine Seite, 4 Schritte)
├── server.r              # Serverlogik, Statistik, Datenbankanbindung
├── Data/
│   └── Skillpool.xlsx    # Initialer Skill-Pool (für die Erstbefüllung)
├── helpcode/
│   └── helpmongo.r       # Admin-Skript: MongoDB befüllen/prüfen (Seeding)
├── www/                  # Statische Assets der App (HTML-Texte, Bilder)
├── word/                 # Word-Quelldateien der HTML-Texte in www/
├── docs/
│   └── SDF-Flussdiagramm.pdf  # Flussdiagramm der Anwendung
├── .Renviron.example     # Vorlage für die lokale Konfiguration (Secrets)
├── .gitignore
└── LICENSE               # MIT-Lizenz
```

Hinweis: Die HTML-Texte in `www/` wurden mit Microsoft Word erstellt; die
zugehörigen `.docx`-Quellen liegen in `word/`. Nach Textänderungen die Datei
erneut als „Webseite, gefiltert (*.htm)" nach `www/` exportieren.

## Installation

### Voraussetzungen

- [R](https://cran.r-project.org/) (≥ 4.0) und optional RStudio
- Zugriff auf ein [MongoDB-Atlas](https://www.mongodb.com/atlas)-Cluster

### Pakete installieren

```r
install.packages(c(
  "shiny", "DT", "RSQLite", "pool", "shinyjs", "uuid", "tidyverse",
  "shinyBS", "shinythemes", "shinyWidgets", "sn", "ggplot2", "mongolite",
  "shinymanager", "shinyalert", "triangle", "EnvStats",
  "openxlsx", "dbplyr"   # nur für helpcode/helpmongo.r nötig
))
```

### Konfiguration (Credentials)

Die App liest alle Zugangsdaten aus Umgebungsvariablen – **es stehen keine
Passwörter im Code**.

1. `.Renviron.example` nach `.Renviron` kopieren
2. Werte eintragen:

   | Variable | Bedeutung |
   |---|---|
   | `MONGODB_HOST` | Host des MongoDB-Atlas-Clusters |
   | `MONGODB_USER` | Datenbank-Benutzer |
   | `MONGODB_PASSWORD` | Datenbank-Passwort |
   | `SDF_APP_USER` | Benutzername für den App-Login |
   | `SDF_APP_PASSWORD` | Passwort für den App-Login |

3. R/RStudio neu starten

`.Renviron` ist in `.gitignore` eingetragen und wird nicht versioniert.

### Erstbefüllung der Datenbank

Beim allerersten Einsatz muss die MongoDB einmalig mit dem Skill-Pool aus
`Data/Skillpool.xlsx` befüllt werden. Dazu in `helpcode/helpmongo.r` die
`saveData(...)`-Aufrufe am Dateiende einkommentieren und das Skript einmal
ausführen (Arbeitsverzeichnis = Projektverzeichnis).

## Starten der App

```r
# im Projektverzeichnis
shiny::runApp()
```

Die App ist für Bildschirmauflösungen von 1366 × 768 bis 1920 × 1080 optimiert.

### Deployment (z. B. shinyapps.io)

```r
library(rsconnect)
rsconnect::deployApp("<Pfad-zum-Projektverzeichnis>")
```

Die Umgebungsvariablen müssen auf dem Zielsystem ebenfalls gesetzt werden
(bei shinyapps.io z. B. indem eine `.Renviron` mit ins Deployment-Bundle
gelegt wird – diese Datei dabei niemals zusätzlich in Git einchecken).

## Sicherheit

- Zugangsdaten ausschließlich über Umgebungsvariablen (`.Renviron`, gitignored)
- Login-Schutz der App über `shinymanager`
- Automatischer Logout nach 5 Minuten Inaktivität
- Es werden nur anonyme, aggregierte Statistiken gespeichert

## Wissenschaftlicher Hintergrund

Das Tool basiert u. a. auf dem Job-Demands-Resources-Modell
(Bakker & Demerouti, 2007) und dem Job-Demand-Control-Modell (Karasek).
Eine Literaturliste ist in der App unter „Weitere Informationen und Kontakt"
hinterlegt. Wissenschaftliche Rückfragen: IKS@hs-fresenius.de

## Lizenz

Dieses Projekt steht unter der [MIT-Lizenz](LICENSE).
© 2021–2026 Institut für komplexe Systemforschung (IKS), Hochschule Fresenius
