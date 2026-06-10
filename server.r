# =============================================================================
# server.r - Serverlogik des Skill-Demand-Forecasting-Tools (SDF)
# =============================================================================
# Inhalt dieser Datei:
#   1. Hilfsfunktionen fuer die Seitenleisten der Schritte 2-4
#   2. Hilfsfunktionen fuer Dropdown-Buttons (Verteilungswahl im Advanced-Modus)
#   3. Plot-/Tabellen-Generatoren fuer die Statistik ueber alle Nutzer
#   4. Funktionen zur inkrementellen Fortschreibung der Nutzungsstatistiken
#      (laufender Mittelwert und laufende Standardabweichung)
#   5. Die eigentliche server()-Funktion mit:
#        - Authentifizierung (shinymanager)
#        - Datenhaltung: MongoDB Atlas (persistent) + lokales SQLite (Cache)
#        - Observer fuer die 4 Bedienschritte der App
#
# Datenfluss (siehe auch docs/SDF-Flussdiagramm.pdf):
#   App-Start -> Skill-Pool aus MongoDB laden -> in lokale SQLite spiegeln
#   -> Nutzer waehlt Skills (Schritt 1) und schaetzt sie per Slider ein
#      (Schritt 2) -> je Skill wird eine Verteilung gefittet und die
#      Wahrscheinlichkeit p berechnet, dass der Soll-/Schwellenwert erreicht
#      wird (Schritt 3, Ampeldarstellung) -> Gesamtauswertung inkl.
#      CSV-Download (Schritt 4); die anonymen Eingaben werden aggregiert
#      in die MongoDB zurueckgeschrieben.
# =============================================================================

library(shiny)
library(DT) # nicer data tables
library(RSQLite) # SQLight
library(pool) # SQL
library(shinyjs) # Java script tools
library(uuid) # UUID generation
library(tidyverse) # data frame transformations
library(shinyBS) # bootstrap buttons an tooltips
library(shinythemes) # preloaded themes
library(shinyWidgets) # nicer buttons
library(sn) # skew normal distribution
library(ggplot2) # nicer plots
library(mongolite) # MONGO DB
library(shinymanager) # app password authentification tool
library(shinyalert) # nicer alert modals
library(triangle) #Dreieckverteilung
library(EnvStats) #Paretoverteilung

#Package update required
# update.packages(ask = FALSE, checkBuilt = TRUE)
#R update required
# install.packages("installr")
# library(installr)
# updateR()

# -----------------------------------------------------------------------------
# Seitenleiste fuer Schritt 4 (Gesamtuebersicht).
# Zeigt die Anzahl ausgewerteter Skills, den geometrischen Mittelwert der
# Erfolgswahrscheinlichkeiten sowie die Ampelbilanz (gruen/gelb/rot) und
# bietet den CSV-Download sowie die Statistik ueber alle Nutzer an.
#   geommean: geometrischer Mittelwert aller p-Werte
#   gr/ge/ro: Anzahl der Skills mit gruener/gelber/roter Ampel
# -----------------------------------------------------------------------------
text_step4 <- function(geommean,gr,ge,ro) {
  sidebarPanel(
    tags$span(style = "color:black; font-size: 16px; font-style: arial",tags$b("Gesamtübersicht")),
    br(),
    br(),
    tags$span(style = "color:black; font-size: 16px; font-style: arial",paste0("Es wurden ",gr+ge+ro," Skills ausgewertet mit einer mittleren ",
                    "Erfolgswahrscheinlichkeit (geometrischer Mittelwert) von p = ",round(geommean,2), ". Insgesamt ",
                    "sind ",gr," Skills grün, ",ge," Skills gelb und ",ro," Skills ",
                    "rot!")),
    br(),
    br(),
    downloadButton('download', 'Download Tabelle', class = "btn btn-danger", style="color: #fff"),
    bsTooltip("download","Sie können die Auswertung direkt als csv-file auf Ihrem Computer unter Downloads abspeichern."),
    br(),
    hr(),
    actionButton("ShowStatsOverAllUsers","Statistik",icon("plus"),class = "btn btn-danger"),
    bsTooltip("ShowStatsOverAllUsers","Dieser Button öffnet einen Modaldialog, in welchem Statistiken zur Anwendungshistorie gezeigt werden."),
    br(),
    width = 4
  )
}

# -----------------------------------------------------------------------------
# Seitenleiste fuer Schritt 3 (Auswertung je Skill): Beschreibung der Kurven,
# Erlaeuterung der Ampelfarben und Button zur Gesamtauswertung (Schritt 4).
# -----------------------------------------------------------------------------
text_step3 <- function() {
  sidebarPanel(
    
    includeHTML("www/SideBar3.htm"),
    
    br(),
    actionButton("BeschreibKurven", "Beschreibung", icon("plus"),class = "btn btn-danger"),
    bsTooltip("BeschreibKurven","Was bedeuten die Kurven?"),
    br(),
    br(),
    tags$span(style = "color:black; font-size: 16px; font-style: arial","Information zur Ampeldarstellung"),
    br(),
    dropdownButton(
      
      includeHTML("www/TextAmpelDescribe.htm"),
      
      circle = TRUE, 
      status = "danger", 
      icon = icon("info"), 
      width = "700px",
      tooltip = tooltipOptions(title = "Was bedeuten die Ampelfarben?")
    ),
    br(),
    hr(),
    actionButton("ConfirmStep3", "Gesamtauswertung", icon("calculator"),class = "btn btn-danger"),
    bsTooltip("ConfirmStep3","Im letzten Schritt wird eine Gesamtauswertung ausgegeben."),
    width = 4
  )
}

# -----------------------------------------------------------------------------
# Seitenleiste fuer Schritt 2 (Skill-Einschaetzung): Beispieleingabe anzeigen
# und Bestaetigung der Slider-Eingaben (startet die Auswertung).
# -----------------------------------------------------------------------------
text_step2 <- function() {
  sidebarPanel(
    
    includeHTML("www/SideBar2.htm"),
    
    br(),
    actionButton("ExampleStep2", "Beispieleingabe", icon("plus"),class = "btn btn-danger"),
    bsTooltip("ExampleStep2","Möchten Sie eine Beispielrechnung mit Output sehen?"),
    br(),
    hr(),
    actionButton("ConfirmStep2", "Auswerten", icon("calculator"), class = "btn btn-danger"),
    bsTooltip("ConfirmStep2","Mit Wahl dieses Buttons bestätigen Sie die Eingabe Ihrer Einschätzungen zu den Skills."),
    width = 4
  )
}

# -----------------------------------------------------------------------------
# Dropdown-Button (Advanced-Modus, rechts) fuer Skill i: Einstellung des
# Schiefeparameters einer schiefen Normalverteilung inkl. Erklaergrafik.
#   i:       laufender Index des ausgewaehlten Skills
#   rvs2:    Liste der Bestaetigen-Buttons (button2<i>)
#   inputID: ID des Dropdown-Buttons (DD2<i>)
#   slider2: Liste der Schiefe-Slider (obs<i>2)
# -----------------------------------------------------------------------------
DropDownConditions <- function(i,rvs2,inputID,slider2) {
  dropdownButton(
    column(12,
           tags$span(style = "color: black; font-size: 16px; font-style: arial","Einstellparameter"),
    ),
    br(),
    br(),
    column(12,
           tags$span(style = "color: black; font-size: 13px; font-style: arial","Schiefeparameter"),
    ),
    br(),
    br(),
    column(12,
           renderUI(tags$img(src = "skewness.png",height = 130, width = 370)),
           align = "center"
    ),
    br(),
    br(),
    slider2[[i]],
    bsTooltip(paste0("obs",paste0(i,2)), paste0("Die Schiefe zeigt die Asymmetrie der Verteilung an. Sehr links bedeutet, dass mehr Mitarbeiter ",
                                                "geringe Werte für das Skill aufweisen als hohe Werte - Sehr rechts bedeutet, dass mehr Mitarbeiter ",
                                                "hohe Werte für das Skill aufweisen als niedrige Werte. Siehe neben stehende Abbildung."),
              "bottom", options = list(container = "body")),
    rvs2[[i]],
    bsTooltip(paste0("button2",i),"Bestätigen Sie Ihre Eingabe.",placement = "top"),
    inputId = inputID,
    circle = TRUE, 
    status = "danger", 
    icon = icon("gear"), 
    width = 450,
    right = TRUE,
    tooltip = tooltipOptions(title = "Einstellparameter der gewählten speziellen Verteilung")
  )
}

# -----------------------------------------------------------------------------
# Dropdown-Button (Advanced-Modus, links) fuer Skill i: Auswahl des
# Verteilungstyps (Normal-, schiefe Normal-, Gleich-, Dreieck-, Paretoverteilung).
#   i:         laufender Index des ausgewaehlten Skills
#   rvs:       Liste der Bestaetigen-Buttons (button<i>)
#   prettyrad: Liste der Radiobuttons zur Verteilungswahl (prettyRB<i>)
#   inputID:   ID des Dropdown-Buttons (DD<i>)
# -----------------------------------------------------------------------------
DropDownBut <- function (i,rvs,prettyrad,inputID) {
  
  dropdownButton(
    column(12,
           tags$span(style = "color: black; font-size: 16px; font-style: arial","Verteilungen"),
    ),
    br(),
    br(),
    prettyrad[[i]],
    inputId = inputID,
    rvs[[i]],
    bsTooltip(paste0("button",i),"Hiermit bestätigen Sie die obige Eingabe und schalten den Dropdown-Button rechts frei.",placement = "top"),
    circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
    tooltip = tooltipOptions(title = "Hier wählen Sie weitere Verteilungsformen aus")
  )
}

# -----------------------------------------------------------------------------
# Baut das Eingabe-Panel fuer Skill i in Schritt 2 auf:
#   - Slider "mittlere 50% der derzeitigen Mitarbeiter" (Quartilsbereich)
#   - Slider "erwarteter Sollwert fuer zukuenftige Stellen"
#   - im Advanced-Modus (EasyMode = FALSE) zusaetzlich die beiden
#     Dropdown-Buttons zur Verteilungswahl und zum Schiefeparameter
#   i = 0 bedeutet: keine Skills ausgewaehlt -> es wird nichts gerendert.
# -----------------------------------------------------------------------------
show_multiple_sliders <- function(i,Text_i,EasyMode,slider1,slider3,rvs,DD,rvs2,DD2) {
  if (i == 0) {
    return()
  } else {
    wellPanel(
      tags$span(style = "color: black; font-size: 16px; font-style: arial",Text_i),
      br(),
      br(),
      chooseSliderSkin(skin = "Shiny", color = "#f2564b"),
      if (EasyMode == FALSE) {
        fluidRow(
          column(6,
                 slider1[[i]],
                 bsTooltip(paste0("obs",paste0(i,1)), paste0("Es sind die mittleren 50% der derzeitigen Mitarbeiter für das Skill ",Text_i, " einzuschätzen"),
                           "bottom", options = list(container = "body")),
          ),
          column(6,
                 slider3[[i]],
                 bsTooltip(paste0("obs",paste0(i,3)), paste0("Wie sollte Ihrer Einschätzung nach der ", tags$u("Mindestwert"), " der Mitarbeiter für das ",
                                                             "zukünftige Projekt für das Skill ",Text_i, " auf der Skala von 0 bis 100 beschaffen sein?"),
                           "bottom", options = list(container = "body")),
          ),
          column(12,
            tags$span(style = "color: black; font-size: 16px; font-style: arial","Einstellungen für weitere Verteilungsmodelle:"),
          ),
          column(6,
                 DD[[i]],
                 align = "center"
          ),
          column(6,
                 disabled(DD2[[i]]),
                 align = "center"
          )
        )
      } else {
        fluidRow(
          column(6,
                 slider1[[i]],
                 bsTooltip(paste0("obs",paste0(i,1)), paste0("Es sind die mittleren 50% der derzeitigen Mitarbeiter für das Skill ",Text_i, " einzuschätzen"),
                           "bottom", options = list(container = "body")),
          ),
          column(6,
                 slider3[[i]],
                 bsTooltip(paste0("obs",paste0(i,3)), paste0("Wie sollte Ihrer Einschätzung nach der ", tags$u("Mindestwert"), " der Mitarbeiter für das ",
                                                             "zukünftige Projekt für das Skill ",Text_i, " auf der Skala von 0 bis 100 beschaffen sein?"),
                           "bottom", options = list(container = "body")),
          ),
        )
      }
    )
  }
}

# -----------------------------------------------------------------------------
# Erzeugt fuer die Statistik ueber alle Nutzer (Modal in Schritt 4) die
# Render-Objekte eines Tabs: Datentabelle + 4 Balken-/Punktdiagramme
# (mittlere p-Werte, mittlere Schwellenwerte, Auswahl-Haeufigkeiten,
# mittlere Slider-Quartile).
#   Text:   Spalten-Suffix des Verteilungstyps ("", "_nv", "_snv", "_u",
#           "_d", "_p", "_ip") - siehe helpcode/helpmongo.r
#   alldat: aggregierte Statistik-Tabelle aller Nutzer (aus MongoDB)
# -----------------------------------------------------------------------------
GenerateOutputPlotsForTabs <- function(output,Text,alldat) {
  dat = alldat[,c("Skillbezeichnung",
                    paste0("absolute_Häufigkeit",Text),
                    paste0("P_Wert_Mean",Text),
                    paste0("P_Wert_SD",Text),
                    paste0("Schwellenwert_Mean",Text),
                    paste0("Schwellenwert_SD",Text),
                    paste0("Unteres_Quartil_Mean",Text),
                    paste0("Unteres_Quartil_SD",Text),
                    paste0("Oberes_Quartil_Mean",Text),
                    paste0("Oberes_Quartil_SD",Text))]
  
  i = 2:10
  dat[ , i] <- apply(dat[ , i], 2, function(x) as.numeric(as.character(x)))
  
  output[[paste0("TabTable",Text)]] = DT::renderDataTable(dat,options = list(scrollX = TRUE))
  
  output[[paste0("TabPValue",Text)]] = renderPlot(ggplot(dat, aes_string(x= paste0("reorder(Skillbezeichnung, -","P_Wert_Mean",Text,")"), y=paste0("P_Wert_Mean",Text)),size = 2) +
                                      geom_bar(stat="identity", fill="#db3522")+
                                      geom_errorbar(aes_string(ymin=paste0("P_Wert_Mean",Text," - ","P_Wert_SD",Text), 
                                                        ymax=paste0("P_Wert_Mean",Text, " + ","P_Wert_SD",Text)), width=.4, size = 2,
                                                    position=position_dodge(.9))+
                                      geom_text(aes_string(label=paste0("round(P_Wert_Mean",Text,",2)")), vjust=-0.3, size=5, color=c("#f24b2e"))+
                                      theme(text = element_text(size=16), axis.text.x = element_text(angle = 90)) + 
                                      labs(title = "Mittlere P-Werte", subtitle = "Mittlerer Anteil der roten Fläche  (sortiert nach auffälligsten P-Werten)", tag = "A") + 
                                      xlab("Skillbezeichnung") +
                                      ylab("P-Wert"),
                                      width = 3000, height = 500
  )
  
  output[[paste0("TabSchwellenwert",Text)]] = renderPlot(ggplot(dat, aes_string(x= paste0("reorder(Skillbezeichnung, -","Schwellenwert_Mean",Text,")"), y=paste0("Schwellenwert_Mean",Text), size = 2)) +
                                                    geom_bar(stat="identity", fill="#db3522")+
                                                    geom_errorbar(aes_string(ymin=paste0("Schwellenwert_Mean",Text," - ","Schwellenwert_SD",Text), 
                                                                             ymax=paste0("Schwellenwert_Mean",Text, " + ","Schwellenwert_SD",Text)), width=.4,
                                                                             position=position_dodge(.9), size = 2)+
                                                    geom_text(aes_string(label=paste0("round(Schwellenwert_Mean",Text,",2)")), vjust=-0.3, size=5, color=c("#f24b2e"))+
                                                    theme(text = element_text(size=16), axis.text.x = element_text(angle = 90)) + 
                                                    labs(title = "Mittlere Schwellenwerte", subtitle = "Mittelwert der eingestellten zukünftigen Sollwerte  (sortiert nach größtem Wert)", tag = "B") + 
                                                    xlab("Skillbezeichnung") +
                                                    ylab("P-Wert"),
                                                    width = 3000, height = 500
  )
  
  output[[paste0("TabFreq",Text)]] = renderPlot(ggplot(dat, aes_string(x= paste0("reorder(Skillbezeichnung, -","absolute_Häufigkeit",Text,")"), y=paste0("absolute_Häufigkeit",Text), size = 2)) +
                                    geom_bar(stat="identity", fill="#db3522")+
                                    geom_text(aes_string(label=paste0("round(absolute_Häufigkeit",Text,",2)")), vjust=-0.3, size=5, color=c("#f24b2e"))+
                                    theme(text = element_text(size=16), axis.text.x = element_text(angle = 90))+ 
                                    labs(title = "Häufigkeit der Auswahl der Skills", subtitle = "absolute Anzahlen (sortiert nach Häufigkeiten)", tag = "C") + 
                                    xlab("Skillbezeichnung") +
                                    ylab("absolute Häufigkeit"),
                                    width = 3000, height = 500
  )
  
  output[[paste0("TabSlider",Text)]] = renderPlot(ggplot(dat, aes(x=Skillbezeichnung)) +
                                      geom_point(aes_string(y=paste0("Unteres_Quartil_Mean",Text)),size = 2,color = "#d94c2b") +
                                      geom_errorbar(aes_string(ymin=paste0("Unteres_Quartil_Mean",Text," - ","Unteres_Quartil_SD",Text), 
                                                               ymax=paste0("Unteres_Quartil_Mean",Text," + ","Unteres_Quartil_SD",Text)), width=0.4,color = "#d94c2b",
                                                               position=position_dodge(.9),size = 2)+
                                      geom_point(aes_string(y=paste0("Oberes_Quartil_Mean",Text)),size = 2,color = "#8a2f1a") +
                                      geom_errorbar(aes_string(ymin=paste0("Oberes_Quartil_Mean",Text," - ","Oberes_Quartil_SD",Text), 
                                                               ymax=paste0("Oberes_Quartil_Mean",Text," + ","Oberes_Quartil_SD",Text)), width=0.4,color = "#8a2f1a",
                                                               position=position_dodge(.9), size = 2)+
                                      theme(text = element_text(size=16), axis.text.x = element_text(angle = 90))+ 
                                      labs(title = "Mittlere Sliderwerte für die Skills", subtitle = "Oberer und unterer Sliderwert über alle User", tag = "D") + 
                                      xlab("Skillbezeichnung") +
                                      ylab("Sliderwerte"),
                                    width = 3000, height = 500
  )
}

# -----------------------------------------------------------------------------
# Layout-Gegenstueck zu GenerateOutputPlotsForTabs(): ordnet Tabelle und Plots
# eines Verteilungs-Tabs untereinander an (mit horizontalem Scrollen).
# -----------------------------------------------------------------------------
OutputPlotsForTabs <- function(output,Text) {
  verticalLayout(
    dataTableOutput(paste0("TabTable",Text), width = "100%"),
    br(),
    br(),
    column(12,
           (div
            (style='overflow-x: scroll',
              plotOutput(paste0("TabPValue",Text))
            )
           )
    ),
    br(),
    br(),
    column(12,
           (div
            (style='overflow-x: scroll',
              plotOutput(paste0("TabSchwellenwert",Text))
            )
           )
    ),br(),
    br(),
    column(12,
           (div
            (style='overflow-x: scroll',
              plotOutput(paste0("TabFreq",Text))
            )
           )
    ),
    br(),
    br(),
    column(12,
           (div
            (style='overflow-x: scroll',
              plotOutput(paste0("TabSlider",Text))
            )
           )
    )
  )
}

# -----------------------------------------------------------------------------
# Schreibt die Auswertungsergebnisse der aktuellen Sitzung in die aggregierte
# Statistik-Tabelle aller Nutzer (eine Zeile je Skill).
# Die Zuordnung erfolgt ueber die Skillbezeichnung; schlaegt der direkte
# Vergleich fehl (z. B. wegen Sonderzeichen), wird ohne Satzzeichen verglichen.
#   dat:        Statistik-Tabelle aller Nutzer
#   Content:    Bezeichnungen der ausgewerteten Skills
#   Verteilung, PValue, Schwellenwert, MinSlider, MaxSlider:
#               Ergebnis-Vektoren der Sitzung (je ein Wert pro Skill)
# Rueckgabe: aktualisierte Tabelle.
# -----------------------------------------------------------------------------
InsertElementsIntoDatatable <- function(dat,Content,Verteilung,PValue,Schwellenwert,MinSlider,MaxSlider) {
  
  for (i in 1:length(Content)) {
    SkillContent = Content[i]
    what = grepl(SkillContent,dat$Skillbezeichnung)
    if (any(what)) {
      FalscheZeile = dat[what,]
      KorrekteZeile = InsertElementsIntoFalscheZeile(FalscheZeile,Verteilung[i],dat,PValue[i],Schwellenwert[i],MinSlider[i],MaxSlider[i])
      dat[grepl(SkillContent,dat$Skillbezeichnung),] <- KorrekteZeile
    } else { #then probably special characters may be inside the string
      SkillContent = gsub("[[:punct:]]",replacement = "", SkillContent)
      datalen = length(dat$Skillbezeichnung)
      for (z in 1:datalen) {
        newname = gsub("[[:punct:]]",replacement = "", dat$Skillbezeichnung[z])
        if (newname == SkillContent) {
          FalscheZeile = dat[z,]
          KorrekteZeile = InsertElementsIntoFalscheZeile(FalscheZeile,Verteilung[i],dat,PValue[i],Schwellenwert[i],MinSlider[i],MaxSlider[i])
          dat[z,] <- KorrekteZeile
        }
      }
    }
  }
  
  return(dat)
}

# -----------------------------------------------------------------------------
# Aktualisiert EINE Zeile der Statistik-Tabelle inkrementell um eine neue
# Nutzereingabe. Es werden immer zwei Spaltenbloecke fortgeschrieben:
# der Gesamtblock (Suffix "") und der Block des verwendeten Verteilungstyps
# (Suffix _nv/_snv/_u/_d/_p/_ip). Je Kennwert werden Haeufigkeit, laufender
# Mittelwert und laufende Standardabweichung angepasst (siehe ComputeNewMean/
# ComputeNewSD) - so muessen keine Einzeleingaben gespeichert werden.
# -----------------------------------------------------------------------------
InsertElementsIntoFalscheZeile <- function(FalscheZeile,Verteilung,dat,PValue,Schwellenwert,MinSlider,MaxSlider) {

  #Verteilung ist string mit folgenden Argumenten:
  #"symmetrisch"
  #c("sehr nach links","etwas nach links","sehr nach rechts","etwas nach rechts")
  #"stetige Gleichverteilung"
  #"Dreieckverteilung"
  #"Paretoverteilung"
  #"Paretoverteilung invers gespiegelt"
  
  if (Verteilung == "symmetrisch") {
    Adendum = "_nv"
    n = c(FalscheZeile$absolute_Häufigkeit,FalscheZeile$absolute_Häufigkeit_nv)
  } else if (any(Verteilung == c("sehr nach links","etwas nach links","sehr nach rechts","etwas nach rechts"))) {
    Adendum = "_snv"
    n = c(FalscheZeile$absolute_Häufigkeit,FalscheZeile$absolute_Häufigkeit_snv)
  } else if (Verteilung == "stetige Gleichverteilung") {
    Adendum = "_u"
    n = c(FalscheZeile$absolute_Häufigkeit,FalscheZeile$absolute_Häufigkeit_u)
  } else if (Verteilung == "Dreieckverteilung") {
    Adendum = "_d"
    n = c(FalscheZeile$absolute_Häufigkeit,FalscheZeile$absolute_Häufigkeit_d)
  } else if (Verteilung == "Paretoverteilung") {
    Adendum = "_p"
    n = c(FalscheZeile$absolute_Häufigkeit,FalscheZeile$absolute_Häufigkeit_p)
  } else if (Verteilung == "Paretoverteilung invers gespiegelt") {
    Adendum = "_ip"
    n = c(FalscheZeile$absolute_Häufigkeit,FalscheZeile$absolute_Häufigkeit_ip)
  } 
  
  Adendum = c("",Adendum)
  
  for (i in 1:2) {
    #absolute Häufigkeit increment
    FalscheZeile[,paste0("absolute_Häufigkeit",Adendum[i])] = FalscheZeile[,paste0("absolute_Häufigkeit",Adendum[i])] + 1
    
    #increment P_Wert_Mean
    MeanOld = FalscheZeile[,paste0("P_Wert_Mean",Adendum[i])]
    MeanNew = ComputeNewMean(MeanOld,PValue,n[i])
    FalscheZeile[,paste0("P_Wert_Mean",Adendum[i])] = MeanNew
    
    #increment P_Wert_SD
    FalscheZeile[,paste0("P_Wert_SD",Adendum[i])] = ComputeNewSD(FalscheZeile[,paste0("P_Wert_SD",Adendum[i])],MeanOld,MeanNew,PValue,n[i])
    
    #increment Schwellenwert_Mean and Schwellenwert_SD
    MeanOld = FalscheZeile[,paste0("Schwellenwert_Mean",Adendum[i])]
    MeanNew = ComputeNewMean(MeanOld,Schwellenwert,n[i])
    FalscheZeile[,paste0("Schwellenwert_Mean",Adendum[i])] = MeanNew
    
    FalscheZeile[,paste0("Schwellenwert_SD",Adendum[i])] = ComputeNewSD(FalscheZeile[,paste0("Schwellenwert_SD",Adendum[i])],MeanOld,MeanNew,Schwellenwert,n[i])
    
    #increment Unteres_Quartil_Mean and Unteres_Quartil_SD
    MeanOld = FalscheZeile[,paste0("Unteres_Quartil_Mean",Adendum[i])]
    MeanNew = ComputeNewMean(MeanOld,MinSlider,n[i])
    FalscheZeile[,paste0("Unteres_Quartil_Mean",Adendum[i])] = MeanNew
    
    FalscheZeile[,paste0("Unteres_Quartil_SD",Adendum[i])] = ComputeNewSD(FalscheZeile[,paste0("Unteres_Quartil_SD",Adendum[i])],MeanOld,MeanNew,MinSlider,n[i])
    
    #increment Oberes_Quartil_Mean and Oberes_Quartil_SD
    MeanOld = FalscheZeile[,paste0("Oberes_Quartil_Mean",Adendum[i])]
    MeanNew = ComputeNewMean(MeanOld,MaxSlider,n[i])
    FalscheZeile[,paste0("Oberes_Quartil_Mean",Adendum[i])] = MeanNew
    
    FalscheZeile[,paste0("Oberes_Quartil_SD",Adendum[i])] = ComputeNewSD(FalscheZeile[,paste0("Oberes_Quartil_SD",Adendum[i])],MeanOld,MeanNew,MaxSlider,n[i])
  }
    
  return(FalscheZeile)
}

# -----------------------------------------------------------------------------
# Laufender Mittelwert: aktualisiert den bisherigen Mittelwert (aus n Werten)
# um einen neuen Wert, ohne dass die Einzelwerte vorliegen muessen.
# -----------------------------------------------------------------------------
ComputeNewMean <- function(MeanOld,NewValue,n) {
  if (n == 0) {
    MeanNew = NewValue
  } else {
    MeanNew = (MeanOld * n + NewValue)/(n + 1) 
  }
  return(MeanNew)
}

# -----------------------------------------------------------------------------
# Laufende Standardabweichung: aktualisiert die bisherige Standardabweichung
# (aus n Werten) um einen neuen Wert - Umstellung der Verschiebungsformel der
# Varianz, sodass nur SD alt, Mittelwert alt/neu und n benoetigt werden.
# -----------------------------------------------------------------------------
ComputeNewSD <- function(SDOld,MeanOld,MeanNew,NewValue,n) {
  if (n == 0) {
    SDNew = 0
  } else {
    SDNew = sqrt((n-1)/n*SDOld^2+MeanOld^2+1/n*NewValue^2-((n+1)/n)*MeanNew^2)
  }
  return(SDNew)
}

# App-Authentifizierung (shinymanager)
# Nutzername und Passwort werden NICHT im Code hinterlegt, sondern aus
# Umgebungsvariablen gelesen (siehe .Renviron.example im Projektverzeichnis).
credentials <- data.frame(
  user = Sys.getenv("SDF_APP_USER"),
  password = Sys.getenv("SDF_APP_PASSWORD"),
  stringsAsFactors = FALSE
)

firstClickEdit = FALSE

server <- function(input, output, session) {
  
  # AUTHENTIFICATION ESSENTIALS
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  # MONGO DB SAVE FUNCTION
  saveData <- function(data,databaseName,collectionName) {
    # Connect to the database
    db <- mongo(collection = collectionName,
                url = sprintf(
                  "mongodb+srv://%s:%s@%s/%s",
                  mongodb$username,
                  mongodb$password,
                  mongodb$host,
                  databaseName
                ),
                options = ssl_options(weak_cert_validation = TRUE))
    #drop whole data from server
    #this is important because of consistency reasons
    db$drop()
    # Insert the data into the mongo collection as a data.frame
    data <- as.data.frame(t(data))
    db$insert(data)
  }
  
  # MONGO DB LOAD FUNCTION
  loadData <- function(databaseName,collectionName) {
    # Connect to the database
    db <- mongo(collection = collectionName,
                url = sprintf(
                  "mongodb+srv://%s:%s@%s/%s",
                  mongodb$username,
                  mongodb$password,
                  mongodb$host,
                  databaseName
                ),
                options = ssl_options(weak_cert_validation = TRUE))
    # Read all the entries
    data <- db$find()
    data = as.data.frame(t(data))
  }
  
  # ESTABLISH CONNECTIONS
  CreateConns <- function(pool) {
    # Mongo DB load data as data.frame
    responses_df <- loadData("allusers","stats") 
    responses_df <- responses_df %>% select(row_id,Skillbezeichnung,Beschreibung,Datum,everything())
    responses_df <- responses_df[order(responses_df$Skillbezeichnung),]
    # CREATE LOCAL SQLight DB
    dbWriteTable(pool, "responses_df", responses_df, overwrite = TRUE)
  }
  
  # pool object for local SQLight DB
  pool <- dbPool(RSQLite::SQLite(), dbname = "db.sqlite") #SQL-database
  
  # Zugangsdaten zum MongoDB-Atlas-Cluster.
  # Die Werte werden aus Umgebungsvariablen gelesen (siehe .Renviron.example) und
  # duerfen niemals im Klartext im Code stehen bzw. nach Git eingecheckt werden.
  # Verwaltung der Datenbank selbst erfolgt im MongoDB-Atlas-Portal.
  mongodb = list(
    "host"     = Sys.getenv("MONGODB_HOST"),
    "username" = Sys.getenv("MONGODB_USER"),
    "password" = Sys.getenv("MONGODB_PASSWORD")
  )

  # Abbruch mit klarer Fehlermeldung, falls die Konfiguration fehlt
  if (mongodb$host == "" || mongodb$username == "" || mongodb$password == "") {
    stop(paste("MongoDB-Konfiguration fehlt: Bitte MONGODB_HOST, MONGODB_USER und",
               "MONGODB_PASSWORD in der Datei .Renviron setzen (Vorlage: .Renviron.example)."))
  }

  # Verwendete Datenbank/Collection fuer den Skill-Pool: "allusers" / "stats"
  
  ##########################################################################################
  
  # Specifications of reactive values
  
  rvsSlider1 = reactiveValues(slider = list())
  rvsSlider2 = reactiveValues(slider = list())
  rvsSlider3 = reactiveValues(slider = list())
  rvs = reactiveValues(buttons = list(), observers = list())
  rvsDD = reactiveValues(buttons = list())
  rvs2 = reactiveValues(buttons = list(), observers = list())
  rvsDD2 = reactiveValues(buttons = list())
  rvsPretty = reactiveValues(radios = list())
  
  sliderlist2 = reactiveValues(val = list())
  
  # Skewnessparameter
  skewparam <- reactiveValues(value = 0)
  
  # Predetermined Datatable Outputs as reactive
  prob <- reactiveValues(value = data.frame(Kompetenzen = "", 
                                            Wahrscheinlichkeit = 0,
                                            Sollwert = 0,
                                            Einschätzung = "",
                                            Q25 = 0,
                                            Q75 = 0,
                                            Verteilung = "",
                                            stringsAsFactors=FALSE)) 
  
  # reactive value to determine the color of the traffic light
  Einsch <- reactiveValues(value = "")
  
  # important values for the reactivity of editing the group-check-box-entries 
  responses_df <- reactive({
    
    input$submit
    input$submit_edit
    input$delete_button
    firstClickEdit
    dbReadTable(pool, "responses_df")
    
  })
  
  allusers <- reactiveValues(dat = data.frame())
  
  ##########################################################################################
  
  # Tooltip for each checkable checkbox
  # important js: container: 'body' appends tooltip to element
  
  groupcheckboxTooltip <- function(id, choices, title, trigger = "hover", options = NULL){
    
    options = shinyBS:::buildTooltipOrPopoverOptionsList(title, "", trigger, options)
    options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
    bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choices, "') {
            opts = $.extend(", options, ", {html: true},{placement: 'top'},{container: 'body'});
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
    htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
  }
  
  # Only show group-checkbox entries from MongoDB with the first start of the app
  # as edit has been clicked: show local
  if (firstClickEdit == FALSE) {
    CreateConns(pool)
    queryall = dbReadTable(pool, "responses_df")
    choices = queryall[,2]
    bezeich = queryall[,3]
    lenchoi = length(choices)
    output$GroupCheckbox <- renderUI({
      tagList(
        checkboxGroupInput("GroupCheckbox",NULL, choices = choices, selected = NULL),
        lapply(seq(lenchoi), function(x) groupcheckboxTooltip(id = "GroupCheckbox", choice = choices[x], title = bezeich[x], trigger = "hover"))
      )
      })
  }
  
  # if first time app use: hints$yes = TRUE - then modals show up each time a confirm button is activated
  hints <- reactiveValues(yes = TRUE)
  
  # OBSERVERS
  #############################################################################################
  
  # close any open modal
  observeEvent(input$IDBack, {
    removeModal()
  })
  
  # if its the first time - then hints$yes will switch to TRUE and modals show up each time a confirm button is pressed
  observeEvent(input$ItsMyFirstTime,{
    if (input$ItsMyFirstTime == "Ja") {
      enable("Confirm")
      enable("EditDat")
      enable("AdvancedMode")
      hints$yes = TRUE
      delay(500,
            showModal(
                modalDialog(
                  title = renderUI(tags$img(src = "Welcome.png",height = 86, width = 204)),
                  div(tags$head(tags$style(".modal-dialog{ width:900px}")),
                  includeHTML("www/PopUpWelcome.htm"),
                  hr(),
                  includeHTML("www/PopUpWelcome2.htm"),
                  br(),
                  actionButton("NoInfo","Keine weiteren Informationen!",class = "btn btn-danger"),
                  bsTooltip("NoInfo","Wenn Sie diesen Button wählen, erhalten Sie keine weiteren Pop-Up-Messages."),
                  ),
                  footer =  actionButton("IDBack",label = "OK!",class = "btn btn-danger"),
                  easyClose = FALSE
                )
              )
            )
    } else if (input$ItsMyFirstTime == "Nichts ausgewählt") {
      disable("Confirm")
      disable("EditDat")
      disable("AdvancedMode")
      } else {
        enable("Confirm")
        enable("EditDat")
        enable("AdvancedMode")
        hints$yes = FALSE
        }
  })
  
  observeEvent(input$NoInfo,{
    updateRadioButtons(session, "ItsMyFirstTime", selected = "Nein")
    hints$yes = FALSE
    removeModal()
  })
  
  # EDITING OF DATA SURROUNDINGS
  #############################################################################################
  
  # Editing mode activation via modal
  entry_formbase <- function() {
    showModal(
      modalDialog(
        div(tags$head(tags$style(".modal-dialog{ width:900px}")), #Modify the width of the dialog
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
            fluidPage(
              fluidRow(
                headerPanel("Datenbankübersicht"),
                helpText("Hier können die Einträge der Datenbank abgeändert werden."),
                hr(),
                actionButton("add_button", "Hinzufügen", icon("plus"),class = "btn btn-danger"),
                actionButton("edit_button", "Ändern", icon("edit"),class = "btn btn-danger"),
                actionButton("delete_button", "Löschen", icon("trash-alt"),class = "btn btn-danger"),
                bsTooltip("add_button","Einen neuen Skilleintrag generieren."),
                bsTooltip("edit_button","Einen bestehenden Skilleintrag bearbeiten."),
                bsTooltip("delete_button","Einen oder mehrere bestehende Skilleinträge löschen."),
                bsTooltip("GoBack","Aus der Bearbeitung der Datenbank herausgehen.",placement = "top"),
                dataTableOutput("responses_table", width = "100%")
              )
            )
        ),
        footer = actionButton("GoBack",label = "Zurück",class = "btn btn-danger"),
        easyClose = TRUE
      )
    )
  }
  
  # GoBack = go out of the editing mode and save the changes to Mongo DB and to local SQLight DB
  observeEvent(input$GoBack,{
    selected = input$GroupCheckbox
    queryall = dbReadTable(pool, "responses_df")
    queryall <- queryall %>% select(row_id,Skillbezeichnung,Beschreibung,Datum,everything())
    queryall <- queryall[order(queryall$Skillbezeichnung),]
    
    saveData(queryall,"allusers","stats")
    
    removeModal()
    choices = queryall[,2]
    bezeich = queryall[,3]
    lenchoi = length(choices)
    selected = intersect(selected,choices) #maintain activated checkboxes before editing (as far as they where not deleted or renamed)
    output$GroupCheckbox <- renderUI({
      tagList(
        checkboxGroupInput("GroupCheckbox",NULL, choices = choices, selected = selected),
        lapply(seq(lenchoi), function(x) groupcheckboxTooltip(id = "GroupCheckbox", choice = choices[x], title = bezeich[x], trigger = "hover"))
      )
    })
  })
  
  # edit data
  observeEvent(input$EditDat, priority = 20,{
    firstClickEdit = TRUE
    entry_formbase()
  })
  
  # toggle state of save button (from disabled to enabled) only if all the mandatory fields have been filled
  fieldsMandatory <- c("Skillbezeichnung")
  
  labelMandatory <- function(label) {
    tagList(
      label,
      span("*", class = "mandatory_star")
    )
  }
  
  observe({
    
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    
  })
  
  # this is the entry form to create new entry
  entry_form <- function(button_id){
    
    showModal(
      modalDialog(
        div(id=("entry_form"),
            easyClose = FALSE,
            tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
            fluidPage(
              fluidRow(
                headerPanel("Eingabeformular"),
                helpText("Bitte spezifizieren Sie die Eingaben. Es muss ein Skill eingegeben oder geändert",
                         "werden (notwendige Eingabe). Darüber hinaus kann das Skill beschrieben werden. ",
                         "Die Beschreibung des Skills erscheint dann auch in der Auswahl (Allgemeiner ",
                         "Skill-Pool). Bestätigen Sie die Eingabe mit dem Speichern-Button."),
                textInput("Skillbezeichnung", labelMandatory("Skillbezeichnung"), placeholder = ""),
                textAreaInput("Beschreibung", "Beschreibung", placeholder = "", height = 100, width = "354px"),
                helpText(labelMandatory(""), paste("notwendige Eingabe")),
                actionButton(button_id, "Speichern",class = "btn btn-danger")
              )
            )
        ),
        footer = actionButton("restoreBase",label = "Zurück",class = "btn btn-danger")
      )
    )
  }
  
  # the base modal is shown
  observeEvent(input$restoreBase, {
    entry_formbase()
  })
  
  # specify data table as reactive and specify columnnames
  formData <- reactive({
    
    formData <- data.frame(row_id = UUIDgenerate(),
                           Skillbezeichnung = input$Skillbezeichnung,
                           Beschreibung = input$Beschreibung,
                           Datum = as.character(format(Sys.Date(), format="%d.%m.%Y")),
                           
                           absolute_Häufigkeit = "0",
                           P_Wert_Mean = "0",
                           P_Wert_SD = "0",
                           Schwellenwert_Mean = "0",
                           Schwellenwert_SD = "0",
                           Unteres_Quartil_Mean = "0",
                           Unteres_Quartil_SD = "0",
                           Oberes_Quartil_Mean = "0",
                           Oberes_Quartil_SD = "0",
                           
                           absolute_Häufigkeit_nv = "0",
                           P_Wert_Mean_nv = "0",
                           P_Wert_SD_nv = "0",
                           Schwellenwert_Mean_nv = "0",
                           Schwellenwert_SD_nv = "0",
                           Unteres_Quartil_Mean_nv = "0",
                           Unteres_Quartil_SD_nv = "0",
                           Oberes_Quartil_Mean_nv = "0",
                           Oberes_Quartil_SD_nv = "0",
                           
                           absolute_Häufigkeit_snv = "0",
                           P_Wert_Mean_snv = "0",
                           P_Wert_SD_snv = "0",
                           Schwellenwert_Mean_snv = "0",
                           Schwellenwert_SD_snv = "0",
                           Unteres_Quartil_Mean_snv = "0",
                           Unteres_Quartil_SD_snv = "0",
                           Oberes_Quartil_Mean_snv = "0",
                           Oberes_Quartil_SD_snv = "0",
                           
                           absolute_Häufigkeit_u = "0",
                           P_Wert_Mean_u = "0",
                           P_Wert_SD_u = "0",
                           Schwellenwert_Mean_u = "0",
                           Schwellenwert_SD_u = "0",
                           Unteres_Quartil_Mean_u = "0",
                           Unteres_Quartil_SD_u = "0",
                           Oberes_Quartil_Mean_u = "0",
                           Oberes_Quartil_SD_u = "0",
                           
                           absolute_Häufigkeit_d = "0",
                           P_Wert_Mean_d = "0",
                           P_Wert_SD_d = "0",
                           Schwellenwert_Mean_d = "0",
                           Schwellenwert_SD_d = "0",
                           Unteres_Quartil_Mean_d = "0",
                           Unteres_Quartil_SD_d = "0",
                           Oberes_Quartil_Mean_d = "0",
                           Oberes_Quartil_SD_d = "0",
                           
                           absolute_Häufigkeit_p = "0",
                           P_Wert_Mean_p = "0",
                           P_Wert_SD_p = "0",
                           Schwellenwert_Mean_p = "0",
                           Schwellenwert_SD_p = "0",
                           Unteres_Quartil_Mean_p = "0",
                           Unteres_Quartil_SD_p = "0",
                           Oberes_Quartil_Mean_p = "0",
                           Oberes_Quartil_SD_p = "0",
                           
                           absolute_Häufigkeit_ip = "0",
                           P_Wert_Mean_ip = "0",
                           P_Wert_SD_ip = "0",
                           Schwellenwert_Mean_ip = "0",
                           Schwellenwert_SD_ip = "0",
                           Unteres_Quartil_Mean_ip = "0",
                           Unteres_Quartil_SD_ip = "0",
                           Oberes_Quartil_Mean_ip = "0",
                           Oberes_Quartil_SD_ip = "0",
                           stringsAsFactors = FALSE)
    return(formData)
  })
  
  unique_id <- function(data){
    replicate(nrow(data), UUIDgenerate())
  }
  
  # submit
  observeEvent(input$submit, priority = 20,{
    
    appendData(formData())
    shinyjs::reset("entry_form")
    entry_formbase()
    
  })
  
  appendData <- function(data){
    
    quary <- sqlAppendTable(pool, "responses_df", data, row.names = FALSE)
    dbExecute(pool, quary)
    
  }
  
  # add
  observeEvent(input$add_button, priority = 20,{
    
    entry_form("submit")
    
  })
  
  # delete
  deleteData <- reactive({
    
    SQL_df <- dbReadTable(pool, "responses_df")
    row_selection <- SQL_df[row$selection_id, "row_id"]
    
    quary <- lapply(row_selection, function(nr) {
      dbExecute(pool, sprintf('DELETE FROM "responses_df" WHERE "row_id" == ("%s")', nr))
    })
  })
  
  del <- reactiveValues(response = FALSE)
  row <- reactiveValues(selection_id = "")
  
  observeEvent(input$delete_button, priority = 20,{
    
    if(length(input$responses_table_rows_selected)>=1 ) {
      
      row$selection_id <- input$responses_table_rows_selected
      
      queryall = dbReadTable(pool, "responses_df")
      choices = data.frame(Skillbezeichnung = queryall[row$selection_id,2])
      choices = aggregate(Skillbezeichnung~.,choices,toString)
      
      delay(200,
            shinyalert(title = "Erbitte erneute Bestätigung.",
                       text = paste0("Sollen die ausgewählten Skills ", paste0(choices) ," wirklich gelöscht werden?"),
                       type = "info",
                       showCancelButton = TRUE,
                       cancelButtonText = "Nein",
                       showConfirmButton = TRUE,
                       confirmButtonText = "Ja, löschen!",
                       callbackR = function(x) {
                         del$response <- x
                       },
                       confirmButtonCol = "#f31d1d",
                       inputId = "ConfirmDeleteData")
      )
    }
    if (length(input$responses_table_rows_selected)<1) {
      delay(500,
            shinyalert(
              title = "Achtung",
              text = "Zum Löschen eines Eintrages bitte mindestens einen Eintrag auswählen!",
              type = "error",
              showConfirmButton = TRUE,
              confirmButtonText = "OK",
              confirmButtonCol = "#f31d1d"
              )
            )
    }
  })
  
  observe({
    input$ConfirmDeleteData
    if (del$response == TRUE) {
      deleteData()
      entry_formbase()
      del$response = FALSE
    }
  })
  
  # edit
  observeEvent(input$edit_button, priority = 20,{
    if(length(input$responses_table_rows_selected) > 1 ){
      delay(500,
            shinyalert(
              title = "Achtung",
              text = "Zum Editieren bitte genau einen Eintrag auswählen!",
              type = "error",
              showConfirmButton = TRUE,
              confirmButtonText = "OK",
              confirmButtonCol = "#f31d1d"
            )
      )
    } else if(length(input$responses_table_rows_selected) < 1){
      delay(500,
            shinyalert(
              title = "Achtung",
              text = "Zum Editieren bitte einen Eintrag auswählen!",
              type = "error",
              showConfirmButton = TRUE,
              confirmButtonText = "OK",
              confirmButtonCol = "#f31d1d"
            )
      )
    }
    
    if(length(input$responses_table_rows_selected) == 1 ){
      SQL_df <- dbReadTable(pool, "responses_df")
      
      entry_form("submit_edit")
      
      updateTextInput(session, "Skillbezeichnung", value = SQL_df[input$responses_table_rows_selected, "Skillbezeichnung"])
      updateTextAreaInput(session, "Beschreibung", value = SQL_df[input$responses_table_rows_selected, "Beschreibung"])

    }
    
  })
  
  observeEvent(input$submit_edit, priority = 20, {
    
    SQL_df <- dbReadTable(pool, "responses_df")
    row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"] #
    dbExecute(pool, sprintf('UPDATE "responses_df" SET "Skillbezeichnung" = ?,
                            "Beschreibung" = ? WHERE "row_id" = ("%s")', row_selection), 
              param = list(input$Skillbezeichnung,
                           input$Beschreibung))
    entry_formbase()
    
  })
  
  # Data table Output in base edit
  output$responses_table <- DT::renderDataTable({
    
    table <- responses_df() %>% select(Skillbezeichnung,Beschreibung,Datum)
    names(table) <- c("Skillbezeichnung", "Beschreibung", "Datum")
    table <- datatable(table, 
                       rownames = FALSE,
                       options = list(searching = FALSE, lengthChange = FALSE)
    )
  })
  
  #           ADVANCE MODE
###########################################################################################  
  
  # check if advanced mode is activated
  alert <- reactiveValues(response = FALSE)
  
  observeEvent(input$AdvancedMode, {
    
    disable("ConfirmStep2")
    disable("ConfirmStep3")
    disable("download")
    disable("ShowStatsOverAllUsers")
    
    if (input$AdvancedMode == TRUE) {
      shinyalert(title = "Advanced-Statistics-Modus",
                 text = "Soll der Advanced-Statistics-Modus eingestellt werden?",
                 type = "success",
                 showCancelButton = TRUE,
                 cancelButtonText = "Nein",
                 showConfirmButton = TRUE,
                 confirmButtonText = "Bestätigen",
                 callbackR = function(x) {
                   alert$response <- x
                 },
                 confirmButtonCol = "#f31d1d",
                 inputId = "ConfirmAdvancedMode")
    }
  })
  
  observe({
    input$ConfirmAdvancedMode
    if (alert$response == FALSE) {
      updateCheckboxInput(session,"AdvancedMode",value = FALSE)
      alert$response = TRUE
    }
  })
  
  #          EXAMPLE BUTTON MODALS
########################################################################################  
  
  observeEvent(input$ExampleStep2, {
    showModal(
      modalDialog(div(tags$head(tags$style(".modal-dialog{ width:900px}")),
        fluidPage(
          fluidRow(
              includeHTML("www/PopUpBeispieleingabe.htm"),  
              renderUI(tags$img(src = "Example1.png", width = 840)),
          )
        )),
        footer = actionButton("IDBack",label = "OK!",class = "btn btn-danger"),
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$BeschreibKurven, {
    showModal(
      modalDialog(div(tags$head(tags$style(".modal-dialog{ width:900px}")),
        fluidPage(
          fluidRow(
            
            includeHTML("www/BeschreibungGraph1.htm"),
            
            wellPanel(
              renderUI(tags$img(src = "ExampleGraph.png", width = 840)),
            ),
            
            includeHTML("www/BeschreibungGraph2.htm"),
            
          )
        )),
        footer = actionButton("IDBack",label = "OK!",class = "btn btn-danger"),
        easyClose = TRUE
      )
    )
  })
  
  # Statistik ueber alle Nutzer (Button in Schritt 4): laedt die aggregierten
  # Daten aus der MongoDB und zeigt sie in einem Modal mit einem Tab je
  # Verteilungstyp (Tabelle + Diagramme).
  observeEvent(input$ShowStatsOverAllUsers,{

    #load Mongo data all users
    allusers$dat = loadData("allusers","stats")
    
    GenerateOutputPlotsForTabs(output,"",allusers$dat)
    GenerateOutputPlotsForTabs(output,"_nv",allusers$dat)
    GenerateOutputPlotsForTabs(output,"_snv",allusers$dat)
    GenerateOutputPlotsForTabs(output,"_u",allusers$dat)
    GenerateOutputPlotsForTabs(output,"_d",allusers$dat)
    GenerateOutputPlotsForTabs(output,"_p",allusers$dat)
    GenerateOutputPlotsForTabs(output,"_ip",allusers$dat)
    
    delay(500,
          showModal(
            modalDialog(
              title = renderUI(tags$img(src = "Info.png",height = 50, width = 50)),
              div(tags$head(tags$style(".modal-dialog{ width:1200px}")),
                  br(),
                  fluidPage(
                    sidebarLayout(
                      sidebarPanel(
                        fluidPage(
                          fluidRow(
                            includeHTML("www/BeschreibungGraph1.htm"),
                          )
                        ),
                        width = 4
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Gesamt",OutputPlotsForTabs(output,"")),
                                    tabPanel("Normalverteilung",OutputPlotsForTabs(output,"_nv")),
                                    tabPanel("schiefe Normalverteilung",OutputPlotsForTabs(output,"_snv")),
                                    tabPanel("stetige Gleichverteilung",OutputPlotsForTabs(output,"_u")),
                                    tabPanel("Dreiecksverteilung",OutputPlotsForTabs(output,"_d")),
                                    tabPanel("Paretoverteilung",OutputPlotsForTabs(output,"_p")),
                                    tabPanel("Paretoverteilung invers",OutputPlotsForTabs(output,"_ip"))
                        )
                      )
                    )
                  )
              ),
              footer =  actionButton("IDBack",label = "OK!",class = "btn btn-danger"),
              easyClose = FALSE
            )
          )
    )
  })
  
  
  #              CONFIRM ACTION BUTTONS / STEPS
  #############################################################################################
  
  # SCHRITT 1 -> 2: Skill-Auswahl bestaetigt.
  # Erzeugt dynamisch fuer jeden angehakten Skill die Slider von Schritt 2
  # (im Advanced-Modus zusaetzlich die Dropdowns zur Verteilungswahl) und
  # zeigt bei Erstnutzung ein interaktives Hilfe-Modal mit Beispielplot.
  observeEvent(input$Confirm, {

    enable("ConfirmStep2")
    disable("ConfirmStep3")
    disable("download")
    disable("ShowStatsOverAllUsers")
    
    contentClicked = input$GroupCheckbox
    numberClicked = length(contentClicked)
    
    if (numberClicked == 0) {
      output$show_slider_inputs <- renderUI(
        show_multiple_sliders(numberClicked,"",EasyMode = TRUE,rvsSlider1$sliders,rvsSlider3$sliders,rvs$buttons,rvsDD$buttons,rvs2$buttons,rvsDD2$buttons)
      )
      showModal(
        modalDialog(
          title = "Fehler!",
          paste("Es muss mindestens ein Skill aus dem Skill-Pool ausgewählt werden." ),
          footer = actionButton("IDBack",label = "OK!",class = "btn btn-danger"),
          easyClose = TRUE)
      )
    } else {
      
      sliderlist2$val = lapply(seq(numberClicked),function(x) "symmetrisch")
      
      for (i in 1:numberClicked) {
        rvsSlider1$sliders[[i]] = sliderInput(paste0("obs",paste0(i,1)), "Mittlere 50% der derzeitigen Mitarbeiter:",
                                              min = 0, max = 100, value = c(30,70))
        rvsSlider3$sliders[[i]] = sliderInput(paste0("obs",paste0(i,3)), "Erwarteter Wert für die zukünftigen Stellen:",
                                              min = 0, max = 100, value = c(50))
      }
      
      if (hints$yes == TRUE) {
        delay(500,
              showModal(
                modalDialog(
                  title = renderUI(tags$img(src = "Info.png",height = 50, width = 50)),
                  div(tags$head(tags$style(".modal-dialog{ width:800px}")),
                  includeHTML("www/PopUpHelp1.htm"),
                  
                  br(),
                  br(),
                  
                  fluidRow(
                    column(6,
                           sliderInput("SliderExampleMittlere50", "Skill-Ausprägung der mittleren 50% bei den derzeitigen Mitarbeitern",
                                       min = 0, max = 100, value = c(50,65)),
                           bsTooltip("SliderExampleMittlere50", "Es sind die mittleren 50% der derzeitigen Mitarbeiter für das Skill einzuschätzen",
                                     "bottom", options = list(container = "body")),
                    ),
                    column(6,
                           sliderInput("SliderExampleSoll", "Skill-Ausprägung (Mindest-Sollwert) der zukünftigen Arbeitsanforderung",
                                       min = 0, max = 100, value = c(70)),
                           bsTooltip("SliderExampleSoll", paste0("Wie sollte Ihrer Einschätzung nach der ", tags$u("Mindestwert"), " der Mitarbeiter für das ",
                                                                 "zukünftige Projekt für das Skill auf der Skala von 0 bis 100 beschaffen sein?"),
                                     "bottom", options = list(container = "body")),
                    ),
                  ),
                  plotOutput("distributionPlot"),
                  br(),
                  hr(),
                  tags$span(style = "color: black; font-size: 16px; font-style: arial",tags$b("Beschreibung Ihrer Eingabe")),
                  br(),
                  br(),
                  textOutput("distributionText"),
                  tags$head(tags$style("#distributionText{
                                 color: black;
                                 font-size: 16px;
                                 font-style: arial;
                                 }"
                  )
                  )),
                  footer =  actionButton("IDBack",label = "OK!",class = "btn btn-danger"),
                  easyClose = FALSE
                )
              )
        )
        output$distributionPlot <- renderPlot({
          y = input$SliderExampleMittlere50
          soll = input$SliderExampleSoll
          y1 = mean(y)
          y2 = (max(y)-y1)/qnorm(0.75)
          pr1 = 1-pnorm(soll,y1,y2)
          ggplot(data.frame(Skalenwert = c(-10, 110)), aes(Skalenwert)) + 
            ggtitle("Beispiel") +
            stat_function(fun=function(x) dnorm(x,y1,y2),size = 1, xlim = c(0,100)) +
            geom_vline(xintercept = soll, linetype = "dashed", color = "red", size = 1) +
            geom_vline(xintercept = y[1], color = "black", size = 0.5) +
            geom_vline(xintercept = y[2], color = "black", size = 0.5) +
            geom_text(x=soll, y=dnorm(50,y1,y2)/2, label=paste0("p = ", round(pr1,2))) +
            geom_text(x=y1, y=dnorm(60,y1,y2)/2, label="mittlere 50%")
        })
        output$distributionText <- renderText({
          y = input$SliderExampleMittlere50
          soll = input$SliderExampleSoll
          y1 = mean(y)
          y2 = (max(y)-y1)/qnorm(0.75)
          pr1 = 1-pnorm(soll,y1,y2)
          paste0("Für Ihren Beispiel-Skill wurde für die derzeitigen ", 
                 "Mitarbeiter eine Spannweite von ", min(y), " bis ", max(y), 
                 " eingegeben (vertikale durchgehende Linien). Bei einem Sollwert für zukünftige Mitarbeiter / zukünftige Arbeitsanforderungen von ", 
                 soll, " (rote gestrichelte Linie) ergibt dies einen Wahrscheinlichkeitswert von: p = ", round(pr1,2),
                 " Dies bedeutet, dass ca. ", round(pr1,2)*100, "% der derzeitigen Mitarbeiter den ",
                 "angegebenen Sollwert überschreiten (entspricht der Fläche unterhalb der Glockenkurve rechts der rot getrichelten Linie) und entsprechend ", 100-round(pr1,2)*100, "% der derzeitigen ",
                 "Mitarbeiter den Sollwert unterschreiten (entspricht der Fläche unterhalb der Glockenkurve links der rot getrichelten Linie).")
        })
      }
      
      if (input$AdvancedMode == TRUE) {
        
        for (i in 1:numberClicked) {
          rvsSlider2$sliders[[i]] = sliderTextInput(paste0("obs",paste0(i,2)), "Schiefe der derzeitigen Mitarbeiter:",
                                                    choices = c("sehr nach links","etwas nach links","symmetrisch","etwas nach rechts","sehr nach rechts"),selected = "symmetrisch")
          rvsPretty$radios[[i]] = prettyRadioButtons(
            label = "Wählen Sie aus:",
            choices = c("Normalverteilung",
                        "schiefe Normalverteilung",
                        "stetige Gleichverteilung",
                        "Dreieckverteilung",
                        "Paretoverteilung",
                        "Paretoverteilung invers gespiegelt"),
            icon = icon("check"),
            animation = "tada",
            status = "default",
            inputId = paste0("prettyRB",i)
          )
        }
        
        for(i in 1:numberClicked) {
          rvs$buttons[[i]] = actionButton(inputId = paste0("button",i), label = "Bestätigen",icon("check"),class = "btn btn-danger")
          rvsDD$buttons[[i]] = DropDownBut(i,rvs$buttons,rvsPretty$radios,paste0("DD",i))
          rvs2$buttons[[i]] = actionButton(inputId = paste0("button2",i), label = "Bestätigen",icon("check"),class = "btn btn-danger")
          rvsDD2$buttons[[i]] = DropDownConditions(i,rvs2$buttons,paste0("DD2",i),rvsSlider2$sliders)
        }
        
        output$show_text_step2 <- renderUI(
          text_step2()
        )
        output$show_slider_inputs <- renderUI(
          lapply(seq(numberClicked), function(x) show_multiple_sliders(x,contentClicked[x],EasyMode = FALSE,rvsSlider1$sliders,rvsSlider3$sliders,rvs$buttons,rvsDD$buttons,rvs2$buttons,rvsDD2$buttons))
        )
        
        rvs$observers = lapply(
          seq(numberClicked), 
          function(i) {
            observeEvent(input[[paste0("button",i)]], {
              if (input[[paste0("prettyRB",i)]] == "schiefe Normalverteilung") {
                enable(paste0("DD2",i))
                enable(paste0("button2",i))
                enable(paste0("obs",paste0(i,2)))
              } else if (input[[paste0("prettyRB",i)]] == "stetige Gleichverteilung") {
                sliderlist2$val[[i]] <- "stetige Gleichverteilung"
              } else if (input[[paste0("prettyRB",i)]] == "Dreieckverteilung") {
                sliderlist2$val[[i]] <- "Dreieckverteilung"
              } else if (input[[paste0("prettyRB",i)]] == "Paretoverteilung") {
                sliderlist2$val[[i]] <- "Paretoverteilung"
              } else if (input[[paste0("prettyRB",i)]] == "Paretoverteilung invers gespiegelt") {
                sliderlist2$val[[i]] <- "Paretoverteilung invers gespiegelt"
              } 
              session$sendCustomMessage("close_drop", "")
            }
            )
          }
        )
        
        rvs2$observers = lapply(
          seq(numberClicked), 
          function(i) {
            observeEvent(input[[paste0("button2",i)]], {
              sliderlist2$val[[i]] <- input[[paste0("obs",paste0(i,2))]]
              session$sendCustomMessage("close_drop", "")
              disable(paste0("DD2",i))
            }
            )
          }
        )
        
      } else {
        output$show_text_step2 <- renderUI(
          text_step2()
        )
        output$show_slider_inputs <- renderUI(
          lapply(seq(numberClicked), function(x) show_multiple_sliders(x,contentClicked[x],EasyMode = TRUE,rvsSlider1$sliders,rvsSlider3$sliders,rvs$buttons,rvsDD$buttons,rvs2$buttons,rvsDD2$buttons))
        )
      }
    }
  })

  # SCHRITT 2 -> 3: Slider-Eingaben bestaetigt - Kern der Auswertung.
  # Fuer jeden Skill wird aus dem Quartilsbereich (Slider 1) die gewaehlte
  # Verteilung gefittet und die Wahrscheinlichkeit p berechnet, dass der
  # Sollwert (Slider 3) erreicht wird:
  #   - symmetrisch:             Normalverteilung (geschlossene Loesung)
  #   - schiefe NV:              Fit via Simulated Annealing (sn::sn.mple)
  #   - Gleich-/Dreieck-/Pareto: Fit der Quartile via Optimierung (optim)
  # Ergebnis je Skill: Dichteplot mit eingefaerbten Flaechen, Erklaertext und
  # Ampel (p < 0.33 rot, < 0.66 gelb, sonst gruen). Abschliessend werden die
  # anonymisierten Ergebnisse in die MongoDB-Statistik zurueckgeschrieben.
  observeEvent(input$ConfirmStep2, {

    enable("ConfirmStep3")
    disable("download")
    disable("ShowStatsOverAllUsers")
    
    contentClicked = input$GroupCheckbox
    numberClicked = length(contentClicked)
    
    if (numberClicked == 0) {
      showModal(
        modalDialog(
          title = "Fehler!",
          paste("Es muss mindestens ein Skill aus dem Skill-Pool ausgewählt werden." ),
          footer = actionButton("IDBack",label = "OK!",class = "btn btn-danger"),
          easyClose = TRUE)
      )
    } else {
      
      #definiere inputliste aus den slidern
      sliderlist1 = lapply(seq(numberClicked),function(x) input[[paste0("obs",paste0(x,1))]])
      sliderlist3 = lapply(seq(numberClicked),function(x) input[[paste0("obs",paste0(x,3))]])
      
      prob$value <- data.frame(Kompetenzen = rep("a",numberClicked), 
                               Wahrscheinlichkeit = rep(0,numberClicked),
                               Sollwert = rep(0,numberClicked),
                               Einschätzung = rep("a",numberClicked),
                               Q25 = rep(0,numberClicked),
                               Q75 = rep(0,numberClicked),
                               Verteilung = rep("a",numberClicked),
                               stringsAsFactors=FALSE)
      
      output$show_text_step3 <- renderUI(
        text_step3()
      )
      
      output$showCalcStep3 <- renderUI ({
        plot_output_list <- lapply(seq(numberClicked), function(i) {
          plotname <- paste0(contentClicked[i])
          textname <- paste0("text",i)
          DecisionTextname <- paste0("DecisionText",i)
          Imagename <- paste0("Ampel",i)
          wellPanel(
            fluidRow(
              column(5,
                     plotOutput(plotname),
              ),
              column(5,
                     verticalLayout(
                       textOutput(textname),
                       wellPanel(
                         splitLayout(align="center",
                                     uiOutput(DecisionTextname),
                                     uiOutput(Imagename)
                         ),
                       )
                     )
              )
            )
          )
        })
        
        do.call(tagList, plot_output_list)
        
      })
      withProgress(message = 'Erstelle Auswertung', value = 0, {
        for (i in 1:numberClicked) {
          incProgress(1/numberClicked, detail = paste("Erstelle Plot ", i))
          # Need local so that each item gets its own number. Without it, the value
          # of i in the renderPlot() will be the same across all instances, because
          # of when the expression is evaluated.
          local({
            my_i <- i
            plotname <- paste0(contentClicked[my_i])
            textname <- paste0("text",i)
            DecisionTextname <- paste0("DecisionText",i)
            Imagename <- paste0("Ampel",i)
            
            sl1 = sliderlist1[[my_i]]
            sl2 = sliderlist2$val[[my_i]]
            sl3 = sliderlist3[[my_i]]
            
            if (sl2 == "symmetrisch") {
              y1 = mean(sl1)
              y2 = (max(sl1)-y1)/qnorm(0.75)
              pr1 = 1-pnorm(sl3,y1,y2)
              output[[plotname]] <- renderPlot({
                ggplot(data.frame(Skalenwert = c(-50, 150)), aes(Skalenwert)) + 
                  ggtitle(plotname) +
                  stat_function(fun=function(x) dnorm(x,y1,y2),size = 2, color = "#fc0303", xlim = c(0,100)) +
                  stat_function(fun=function(x) dnorm(x,y1,y2), geom = "area", fill = "#fc0303", alpha = 0.4, xlim = c(-50,sl3)) +
                  stat_function(fun=function(x) dnorm(x,y1,y2), geom = "area", fill = "#206800", alpha = 0.4, xlim = c(sl3,150)) +
                  geom_vline(xintercept = sl3, linetype = "dashed", color = "red", size = 1) +
                  geom_vline(xintercept = 0, color = "red", size = 0.5) +
                  geom_vline(xintercept = 100, color = "red", size = 0.5) +
                  stat_function(fun=function(x) dnorm(x,y1,y2), linetype = "dashed", size = 0.5) +
                  scale_x_continuous(waiver(),c(0,50,100)) +
                  geom_text(x=sl3, y=dnorm(50,y1,y2)/2, label=paste0("p = ", round(pr1,2))) +
                  theme(plot.title = element_text(size = 14, face = "bold",color="black")) +
                  theme(axis.line = element_line(colour = "#fc0303", size = 2, linetype = "solid")) +
                  theme(axis.text.x = element_text(face="bold", size=14)) +
                  theme(plot.title = element_text(hjust = 0.5),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        panel.background = element_rect(fill = "transparent",colour = NA),
                        plot.background = element_rect(fill = "transparent",colour = NA)) +
                  theme(axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.y=element_blank())
              },bg = "transparent")
              output[[textname]] <- renderText({
                paste0("Die Ausprägung des Skills \"", contentClicked[my_i], "\" wurde für die mittleren 50% der derzeitigen ", 
                       "Mitarbeiter in einem Bereich zwischen ", min(sl1), " und ", max(sl1), 
                       " eingestuft (somit erreichen 25% einen Wert von unter ", min(sl1) ," und weitere 25 % einen Wert von über ", max(sl1) ,"). Bei einem Sollwert für zukünftige Arbeitsanforderung / für die zukünftigen Mitarbeiter von ", 
                       sl3, " ergibt dies einen Wahrscheinlichkeitswert für die Erreichung des Schwellwertes von: p = ", round(pr1,2),
                       " Anders ausgedrückt bedeutet das, dass ca. ", 100-round(pr1,2)*100, "% der derzeitigen Mitarbeiter den ",
                       "angegebenen Sollwert (die Mindestanforderung) unterschreiten (also nicht erreichen).")
              })
            } else if (any(sl2 == c("sehr nach links","etwas nach links","sehr nach rechts","etwas nach rechts"))) {
              unt = min(sl1)
              obe = max(sl1)
              if (sl2 == "sehr nach links") {
                skewparam$value = 0.1
              } else if (sl2 == "etwas nach links") {
                skewparam$value = 0.3
              } else if (sl2 == "sehr nach rechts") {
                skewparam$value = 0.9
              } else if (sl2 == "etwas nach rechts") {
                skewparam$value = 0.7
              }
              med = unt + (obe-unt)*skewparam$value
              koeff = 0.7+(1-(obe-med)/(obe-unt))*0.1
              v = c(rep(unt-(med-unt)*koeff,10),rep(med,10),rep(obe+(obe-med)*koeff,10))
              cp.est <- sn.mple(y=v,opt.method = "SANN")$cp #simulated annealing
              dp.est <- cp2dp(cp.est,family="SN")
              pr1 = 1-psn(sl3,dp = dp.est)
              output[[plotname]] <- renderPlot({
                ggplot(data.frame(Skalenwert = c(-50, 150)), aes(Skalenwert)) + 
                  ggtitle(plotname) +
                  stat_function(fun=function(x) dsn(x,dp = dp.est),size = 2, color = "#fc0303", xlim = c(0,100)) +
                  stat_function(fun=function(x) dsn(x,dp = dp.est), geom = "area", fill = "#fc0303", alpha = 0.4, xlim = c(-50,sl3)) +
                  stat_function(fun=function(x) dsn(x,dp = dp.est), geom = "area", fill = "#206800", alpha = 0.4, xlim = c(sl3,150)) +
                  geom_vline(xintercept = sl3, linetype = "dashed", color = "red", size = 1) +
                  geom_vline(xintercept = 0, color = "red", size = 0.5) +
                  geom_vline(xintercept = 100, color = "red", size = 0.5) +
                  stat_function(fun=function(x) dsn(x,dp = dp.est), linetype = "dashed", size = 0.5) +
                  scale_x_continuous(waiver(),c(0,50,100)) +
                  geom_text(x=sl3, y=dsn(med,dp = dp.est)/2, label=paste0("p = ", round(pr1,2))) +
                  theme(plot.title = element_text(size = 14, face = "bold",color="black")) +
                  theme(axis.line = element_line(colour = "#fc0303", size = 2, linetype = "solid")) +
                  theme(axis.text.x = element_text(face="bold", size=14)) +
                  theme(plot.title = element_text(hjust = 0.5),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        panel.background = element_rect(fill = "transparent",colour = NA),
                        plot.background = element_rect(fill = "transparent",colour = NA)) +
                  theme(axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.y=element_blank())
              },bg = "transparent")
              output[[textname]] <- renderText({
                paste0("Die Ausprägung des Skills \"", contentClicked[my_i], "\" wurde für die mittleren 50% der derzeitigen ", 
                       "Mitarbeiter in einem Bereich zwischen ", min(sl1), " und ", max(sl1), 
                       " eingestuft (somit erreichen 25% einen Wert von unter ", min(sl1) ," und weitere 25 % einen Wert von über ", max(sl1) ,"). Bei einem Sollwert für zukünftige Arbeitsanforderung / für die zukünftigen Mitarbeiter von ", 
                       sl3, " ergibt dies einen Wahrscheinlichkeitswert für die Erreichung des Schwellwertes von: p = ", round(pr1,2),
                       " Anders ausgedrückt bedeutet das, dass ca. ", 100-round(pr1,2)*100, "% der derzeitigen Mitarbeiter den ",
                       "angegebenen Sollwert (die Mindestanforderung) unterschreiten (also nicht erreichen).")
              })
            } else if (sl2 == "stetige Gleichverteilung") {
              l = max(sl1) - min(sl1)
              y1 = min(sl1) - l/2
              y2 = max(sl1) + l/2
              pr1 = 1-punif(sl3,y1,y2)
              output[[plotname]] <- renderPlot({
                ggplot(data.frame(Skalenwert = c(-50, 150)), aes(Skalenwert)) + 
                  ggtitle(plotname) +
                  stat_function(fun=function(x) dunif(x,y1,y2),size = 2, color = "#fc0303", xlim = c(0,100)) +
                  stat_function(fun=function(x) dunif(x,y1,y2), geom = "area", fill = "#fc0303", alpha = 0.4, xlim = c(-50,sl3)) +
                  stat_function(fun=function(x) dunif(x,y1,y2), geom = "area", fill = "#206800", alpha = 0.4, xlim = c(sl3,150)) +
                  geom_vline(xintercept = sl3, linetype = "dashed", color = "red", size = 1) +
                  geom_vline(xintercept = 0, color = "red", size = 0.5) +
                  geom_vline(xintercept = 100, color = "red", size = 0.5) +
                  stat_function(fun=function(x) dunif(x,y1,y2), linetype = "dashed", size = 0.5) +
                  scale_x_continuous(waiver(),c(0,50,100)) +
                  geom_text(x=sl3, y=dunif(50,y1,y2)/2, label=paste0("p = ", round(pr1,2))) +
                  theme(plot.title = element_text(size = 14, face = "bold",color="black")) +
                  theme(axis.line = element_line(colour = "#fc0303", size = 2, linetype = "solid")) +
                  theme(axis.text.x = element_text(face="bold", size=14)) +
                  theme(plot.title = element_text(hjust = 0.5),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        panel.background = element_rect(fill = "transparent",colour = NA),
                        plot.background = element_rect(fill = "transparent",colour = NA)) +
                  theme(axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.y=element_blank())
              },bg = "transparent")
              output[[textname]] <- renderText({
                paste0("Die Ausprägung des Skills \"", contentClicked[my_i], "\" wurde für die mittleren 50% der derzeitigen ", 
                       "Mitarbeiter in einem Bereich zwischen ", min(sl1), " und ", max(sl1), 
                       " eingestuft (somit erreichen 25% einen Wert von unter ", min(sl1) ," und weitere 25 % einen Wert von über ", max(sl1) ,"). Bei einem Sollwert für zukünftige Arbeitsanforderung / für die zukünftigen Mitarbeiter von ", 
                       sl3, " ergibt dies einen Wahrscheinlichkeitswert für die Erreichung des Schwellwertes von: p = ", round(pr1,2),
                       " Anders ausgedrückt bedeutet das, dass ca. ", 100-round(pr1,2)*100, "% der derzeitigen Mitarbeiter den ",
                       "angegebenen Sollwert (die Mindestanforderung) unterschreiten (also nicht erreichen).")
              })
            } else if (sl2 == "Dreieckverteilung") {
              x1 = min(sl1)
              x2 = max(sl1)
              errorFn = function(params) (ptriangle(x1, params[1], params[2], params[3]) - 0.25)^2 + 
                (ptriangle(x2, params[1], params[2], params[3]) - 0.75)^2
              a = 0
              b = 100
              c = 50
              res = optim( c(a,b,c), errorFn, method = 'L-BFGS-B') #Broyden–Fletcher–Goldfarb–Shanno
              y1 = res$par[1]
              y2 = res$par[2]
              y3 = res$par[3]
              pr1 = 1-ptriangle(sl3,y1,y2,y3)
              output[[plotname]] <- renderPlot({
                ggplot(data.frame(Skalenwert = c(-50, 150)), aes(Skalenwert)) + 
                  ggtitle(plotname) +
                  stat_function(fun=function(x) dtriangle(x,y1,y2,y3),size = 2, color = "#fc0303", xlim = c(0,100)) +
                  stat_function(fun=function(x) dtriangle(x,y1,y2,y3), geom = "area", fill = "#fc0303", alpha = 0.4, xlim = c(-50,sl3)) +
                  stat_function(fun=function(x) dtriangle(x,y1,y2,y3), geom = "area", fill = "#206800", alpha = 0.4, xlim = c(sl3,150)) +
                  geom_vline(xintercept = sl3, linetype = "dashed", color = "red", size = 1) +
                  geom_vline(xintercept = 0, color = "red", size = 0.5) +
                  geom_vline(xintercept = 100, color = "red", size = 0.5) +
                  stat_function(fun=function(x) dtriangle(x,y1,y2,y3), linetype = "dashed", size = 0.5) +
                  scale_x_continuous(waiver(),c(0,50,100)) +
                  geom_text(x=sl3, y=dtriangle(50,y1,y2,y3)/2, label=paste0("p = ", round(pr1,2))) +
                  theme(plot.title = element_text(size = 14, face = "bold",color="black")) +
                  theme(axis.line = element_line(colour = "#fc0303", size = 2, linetype = "solid")) +
                  theme(axis.text.x = element_text(face="bold", size=14)) +
                  theme(plot.title = element_text(hjust = 0.5),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        panel.background = element_rect(fill = "transparent",colour = NA),
                        plot.background = element_rect(fill = "transparent",colour = NA)) +
                  theme(axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.y=element_blank())
              },bg = "transparent")
              output[[textname]] <- renderText({
                paste0("Die Ausprägung des Skills \"", contentClicked[my_i], "\" wurde für die mittleren 50% der derzeitigen ", 
                       "Mitarbeiter in einem Bereich zwischen ", min(sl1), " und ", max(sl1), 
                       " eingestuft (somit erreichen 25% einen Wert von unter ", min(sl1) ," und weitere 25 % einen Wert von über ", max(sl1) ,"). Bei einem Sollwert für zukünftige Arbeitsanforderung / für die zukünftigen Mitarbeiter von ", 
                       sl3, " ergibt dies einen Wahrscheinlichkeitswert für die Erreichung des Schwellwertes von: p = ", round(pr1,2),
                       " Anders ausgedrückt bedeutet das, dass ca. ", 100-round(pr1,2)*100, "% der derzeitigen Mitarbeiter den ",
                       "angegebenen Sollwert (die Mindestanforderung) unterschreiten (also nicht erreichen).")
              })
            } else if (sl2 == "Paretoverteilung") {
              x1 = min(sl1)
              x2 = max(sl1)
              errorFn = function(params) (ppareto(x1, params[1], params[2]) - 0.25)^2 + 
                (ppareto(x2, params[1], params[2]) - 0.75)^2
              loc = x1
              shap = 1
              res = optim(c(loc,shap), errorFn, method = 'CG') #conjugate gradients method
              y1 = res$par[1]
              y2 = res$par[2]
              pr1 = 1-ppareto(sl3,y1,y2)
              output[[plotname]] <- renderPlot({
                ggplot(data.frame(Skalenwert = c(-50, 150)), aes(Skalenwert)) + 
                  ggtitle(plotname) +
                  stat_function(fun=function(x) dpareto(x,y1,y2),size = 2, color = "#fc0303", xlim = c(0,100)) +
                  stat_function(fun=function(x) dpareto(x,y1,y2), geom = "area", fill = "#fc0303", alpha = 0.4, xlim = c(-50,sl3)) +
                  stat_function(fun=function(x) dpareto(x,y1,y2), geom = "area", fill = "#206800", alpha = 0.4, xlim = c(sl3,150)) +
                  geom_vline(xintercept = sl3, linetype = "dashed", color = "red", size = 1) +
                  geom_vline(xintercept = 0, color = "red", size = 0.5) +
                  geom_vline(xintercept = 100, color = "red", size = 0.5) +
                  stat_function(fun=function(x) dpareto(x,y1,y2), linetype = "dashed", size = 0.5) +
                  scale_x_continuous(waiver(),c(0,50,100)) +
                  geom_text(x=sl3, y=dpareto(50,y1,y2)/2, label=paste0("p = ", round(pr1,2))) +
                  theme(plot.title = element_text(size = 14, face = "bold",color="black")) +
                  theme(axis.line = element_line(colour = "#fc0303", size = 2, linetype = "solid")) +
                  theme(axis.text.x = element_text(face="bold", size=14)) +
                  theme(plot.title = element_text(hjust = 0.5),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        panel.background = element_rect(fill = "transparent",colour = NA),
                        plot.background = element_rect(fill = "transparent",colour = NA)) +
                  theme(axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.y=element_blank())
              },bg = "transparent")
              output[[textname]] <- renderText({
                paste0("Die Ausprägung des Skills \"", contentClicked[my_i], "\" wurde für die mittleren 50% der derzeitigen ", 
                       "Mitarbeiter in einem Bereich zwischen ", min(sl1), " und ", max(sl1), 
                       " eingestuft (somit erreichen 25% einen Wert von unter ", min(sl1) ," und weitere 25 % einen Wert von über ", max(sl1) ,"). Bei einem Sollwert für zukünftige Arbeitsanforderung / für die zukünftigen Mitarbeiter von ", 
                       sl3, " ergibt dies einen Wahrscheinlichkeitswert für die Erreichung des Schwellwertes von: p = ", round(pr1,2),
                       " Anders ausgedrückt bedeutet das, dass ca. ", 100-round(pr1,2)*100, "% der derzeitigen Mitarbeiter den ",
                       "angegebenen Sollwert (die Mindestanforderung) unterschreiten (also nicht erreichen).")
              })
            } else if (sl2 == "Paretoverteilung invers gespiegelt") {
              x1 = 100 - max(sl1)
              x2 = 100 - min(sl1)
              errorFn = function(params) (ppareto(x1, params[1], params[2]) - 0.25)^2 + 
                (ppareto(x2, params[1], params[2]) - 0.75)^2
              loc = x1
              shap = 1
              res = optim(c(loc,shap), errorFn, method = 'CG') #conjugate gradients method
              y1 = res$par[1]
              y2 = res$par[2]
              sl3 = 100 - sl3
              pr1 = ppareto(sl3,y1,y2)
              output[[plotname]] <- renderPlot({
                ggplot(data.frame(Skalenwert = c(-150, 50)), aes(Skalenwert)) + 
                  ggtitle(plotname) +
                  stat_function(fun=function(x) dpareto(-x,y1,y2),size = 2, color = "#fc0303", xlim = c(-100,0)) +
                  stat_function(fun=function(x) dpareto(-x,y1,y2), geom = "area", fill = "#fc0303", alpha = 0.4, xlim = c(-150,0-sl3)) +
                  stat_function(fun=function(x) dpareto(-x,y1,y2), geom = "area", fill = "#206800", alpha = 0.4, xlim = c(0-sl3,50)) +
                  geom_vline(xintercept = 0-sl3, linetype = "dashed", color = "red", size = 1) +
                  geom_vline(xintercept = -100, color = "red", size = 0.5) +
                  geom_vline(xintercept = 0, color = "red", size = 0.5) +
                  stat_function(fun=function(x) dpareto(-x,y1,y2), linetype = "dashed", size = 0.5) +
                  scale_x_continuous(waiver(),breaks = c(-100,-50,0), labels = c(0,50,100)) +
                  geom_text(x=0-sl3, y=dpareto(50,y1,y2)/2, label=paste0("p = ", round(pr1,2))) +
                  theme(plot.title = element_text(size = 14, face = "bold",color="black")) +
                  theme(axis.line = element_line(colour = "#fc0303", size = 2, linetype = "solid")) +
                  theme(axis.text.x = element_text(face="bold", size=14)) +
                  theme(plot.title = element_text(hjust = 0.5),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        panel.background = element_rect(fill = "transparent",colour = NA),
                        plot.background = element_rect(fill = "transparent",colour = NA)) +
                  theme(axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.y=element_blank())
              },bg = "transparent")
              output[[textname]] <- renderText({
                paste0("Die Ausprägung des Skills \"", contentClicked[my_i], "\" wurde für die mittleren 50% der derzeitigen ", 
                       "Mitarbeiter in einem Bereich zwischen ", min(sl1), " und ", max(sl1), 
                       " eingestuft (somit erreichen 25% einen Wert von unter ", min(sl1) ," und weitere 25 % einen Wert von über ", max(sl1) ,"). Bei einem Sollwert für zukünftige Arbeitsanforderung / für die zukünftigen Mitarbeiter von ", 
                       sl3, " ergibt dies einen Wahrscheinlichkeitswert für die Erreichung des Schwellwertes von: p = ", round(pr1,2),
                       " Anders ausgedrückt bedeutet das, dass ca. ", 100-round(pr1,2)*100, "% der derzeitigen Mitarbeiter den ",
                       "angegebenen Sollwert (die Mindestanforderung) unterschreiten (also nicht erreichen).")
              })
            }
            
            if (pr1 < 0.33) {
              Einsch$value = "rot"
              output[[DecisionTextname]] <- renderUI({
                paste0("Achtung!")
              })
              output[[Imagename]] <- renderUI(tags$img(src = "Ampelrot.png",height = 100, width = 60))
            } else if (pr1 < 0.66) {
              Einsch$value = "gelb"
              output[[DecisionTextname]] <- renderUI({
                paste0("Hinweis!")
              })
              output[[Imagename]] <- renderUI(tags$img(src = "Ampelgelb.png",height = 100, width = 60))
            } else {
              Einsch$value = "grün"
              output[[DecisionTextname]] <- renderUI({
                paste0("Normbereich")
              })
              output[[Imagename]] <- renderUI(tags$img(src = "Ampelgruen.png",height = 100, width = 60))
            }
            prob$value[my_i,1] <- contentClicked[my_i]
            prob$value[my_i,2] <- round(pr1,3)
            prob$value[my_i,3] <- sl3
            prob$value[my_i,4] <- Einsch$value
            prob$value[my_i,5] <- min(sl1)
            prob$value[my_i,6] <- max(sl1)
            prob$value[my_i,7] <- sl2
          })
        }
      })
    }
    if (hints$yes == TRUE) {
      delay(500,
            showModal(
              modalDialog(
                title = renderUI(tags$img(src = "Info.png",height = 50, width = 50)),
                
                div(tags$head(tags$style(".modal-dialog{ width:900px}")),
                includeHTML("www/PopUpAuswertung1.htm"),
                
                wellPanel(
                  renderUI(tags$img(src = "ExampleGraph.png", width = 840)),
                ),
                
                includeHTML("www/PopUpAuswertung2.htm")),
                
                footer =  actionButton("IDBack",label = "OK!",class = "btn btn-danger"),
                easyClose = FALSE
              )
            )
      )
    }
    
    #load Mongo data all users
    allusers$dat = loadData("allusers","stats")
    dat = allusers$dat
    varanz = dim(dat)[2]
    
    if (varanz > 0) {
      #create new data.frame all users
      Content = prob$value[,1]
      Verteilung = prob$value[,7] #
      PValue = prob$value[,2]
      Schwellenwert = prob$value[,3]
      MinSlider = prob$value[,5]
      MaxSlider = prob$value[,6]
      
      dat <- dat %>% select(row_id,Beschreibung,Datum,Skillbezeichnung,absolute_Häufigkeit, everything())
      i = 5:varanz
      dat[ ,i] <- apply(dat[ , i], 2, function(x) as.numeric(as.character(x)))
      
      allusers = InsertElementsIntoDatatable(dat,Content,Verteilung,PValue,Schwellenwert,MinSlider,MaxSlider)
      
      #save Mongo data all users
      saveData(allusers,"allusers","stats")
    } else {
      showModal(
        modalDialog(
          title = "Fehler!",
          paste("Die Datenbank ist leer. Kontaktieren Sie bitte den Administrator" ),
          footer = actionButton("IDBack",label = "OK!",class = "btn btn-danger"),
          easyClose = TRUE)
      )
    }
    
    })
  
  # SCHRITT 3 -> 4: Gesamtauswertung.
  # Berechnet den geometrischen Mittelwert aller p-Werte, zaehlt die
  # Ampelfarben aus und blendet die Ergebnis-Tabelle samt Download und
  # Einordnungstext ein.
  observeEvent(input$ConfirmStep3,{

    if (hints$yes == TRUE) {
      delay(500,
            showModal(
              modalDialog(
                title = renderUI(tags$img(src = "Info.png",height = 50, width = 50)),
                includeHTML("www/Gesamtauswertung.htm"),
                br(),
                footer =  actionButton("IDBack",label = "OK!",class = "btn btn-danger"),
                easyClose = FALSE
              )
            )
      )
    }
    
    enable("download")
    enable("ShowStatsOverAllUsers")
    
    x = prob$value[,2]
    geommean = exp(mean(log(x)))
    sub = prob$value[,4]
    gr = length(grep("grün", sub))
    ge = length(grep("gelb", sub))
    ro = length(grep("rot", sub))
    
    output$show_text_step4 <- renderUI(
      text_step4(geommean,gr,ge,ro)
    )
    shinyjs::showElement(id= "hiddenPanel")
    output$Gesamtauswertung = DT::renderDataTable(prob$value,options = list(scrollX = TRUE))
    getPage1<-function() {
      return(
        wellPanel(
          includeHTML("www/5_Schritt4Einordnung.htm")
        )
      )
    }
    output$DescribeStep4<-renderUI({getPage1()})
  })
  
  # CSV-Download der Gesamtauswertung (Schritt 4)
  output$download <- downloadHandler(
    filename = function() {
      paste("Gesamtauswertung.csv")
    },
    content = function(file) {
      write.csv2(prob$value, file)
    }
  )
  
  # infomaterial and "impressum"
  ############################################################################################
  
  getPage2<-function() {
    return(
      includeHTML("www/6_WeitereInfo.htm")
    )
  }
  
  output$DescribeInfo4<-renderUI({getPage2()})
}

# Deployment (z. B. auf shinyapps.io):
# library(rsconnect)
# rsconnect::deployApp("<Pfad-zum-Projektverzeichnis>")
# Hinweis: Vor dem Deployment die Umgebungsvariablen (siehe .Renviron.example)
# auf dem Zielsystem konfigurieren.