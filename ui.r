# =============================================================================
# ui.r - Benutzeroberflaeche des Skill-Demand-Forecasting-Tools (SDF)
# =============================================================================
# Das SDF ist eine quantitative Entscheidungshilfe zur Unterstuetzung von
# digitalen Transformationsprozessen und Change-Projekten.
#
# Aufbau der Oberflaeche (eine durchscrollbare Seite, 4 Schritte):
#   - Login-Maske (shinymanager) mit Inaktivitaets-Timer
#   - Kopfbereich: Titel, Logo, einleitende HTML-Texte (aus www/)
#   - Erstnutzungs-Abfrage (steuert, ob Hilfe-Pop-ups gezeigt werden)
#   - Schritt 1: Skill-Auswahl aus dem Skill-Pool (editierbar)
#   - Schritt 2: Einschaetzung je Skill ueber Slider
#                (optional "Advanced-Statistics-Modus" mit Verteilungswahl)
#   - Schritt 3: Auswertung je Skill (Dichteplot + Ampel)
#   - Schritt 4: Gesamtauswertung (Tabelle, Download, Statistik ueber alle User)
#   - Fussbereich: weitere Informationen / Impressum
#
# Die statischen Texte liegen als HTML-Dateien in www/ (Quelle: word/*.docx).
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

options(width = 1200)

# define CSS for mandatory star (editing modal)
appCSS <- ".mandatory_star { color: red; }"

# -----------------------------------------------------------------------------
# setSliderColor: lokale Kopie der Funktion aus shinyWidgets.
# Die Paket-Funktion ist seit shinyWidgets ueberholt und nur noch ein leerer
# Stub (gibt eine Deprecation-Warnung aus und faerbt nichts mehr). Diese
# vendored Version (Quelle: dreamRs/shinyWidgets, Commit 26838f9) stellt die
# Faerbung der Slider wieder her und vermeidet die Warnung. Da sie hier global
# definiert wird, hat sie Vorrang vor der gleichnamigen Paketfunktion.
# -----------------------------------------------------------------------------
setSliderColor <- function(color, sliderId) {
  stopifnot(!is.null(color))
  stopifnot(is.character(color))
  stopifnot(is.numeric(sliderId))
  stopifnot(!is.null(sliderId))

  # die CSS-Klasse fuer ionRangeSlider beginnt bei 0 -> Index um 1 reduzieren
  sliderId <- sliderId - 1

  sliderCol <- lapply(sliderId, FUN = function(i) {
    paste0(
      ".js-irs-", i, " .irs-single,",
      " .js-irs-", i, " .irs-from,",
      " .js-irs-", i, " .irs-to,",
      " .js-irs-", i, " .irs-bar-edge,",
      " .js-irs-", i,
      " .irs-bar{  border-color: transparent;background: ", color[i + 1],
      "; border-top: 1px solid ", color[i + 1],
      "; border-bottom: 1px solid ", color[i + 1],
      ";}"
    )
  })

  tags$head(tags$style(HTML(as.character(sliderCol))))
}

# Predefined IDLE TIMER
# Set Idle-Timer to 5Min. = 300000 or change
inactivity <- "function idleTimer() {
  var t = setTimeout(logout, 300000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  // catches keyboard actions

    function logout() {
      window.close();  //close the window
    }

    function resetTimer() {
      clearTimeout(t);
      t = setTimeout(logout, 300000);  // time is in milliseconds (1000 is 1 second)
    }
  }
  idleTimer();"

# authentification page - what to be shown
set_labels(language = "en",
           "Please authenticate" = "Bitte loggen Sie sich ein!",
           "Username:" = "Nutzername:",
           "Password:" = "Geben Sie das Passwort ein:")

# UI
#############################################################################################

ui <- secure_app(head_auth = tags$script(inactivity),
  # Den unklaren schwebenden "+"-Button (FAB) von shinymanager abschalten und
  # stattdessen unten einen klar beschrifteten Logout-Button einbauen (siehe unten).
  fab_position = "none",
  fluidPage(

    theme = shinytheme("spacelab"),

    # enable shinyjs
    useShinyjs(),
    shinyjs::inlineCSS(appCSS),#see above

    # enable shinyalert
    useShinyalert(),

    # Sichtbarer Logout-Button unten rechts. Loest denselben Logout aus wie der
    # frühere FAB: shinymanager beobachtet input$.shinymanager_logout.
    tags$div(
      style = "position:fixed; bottom:15px; right:15px; z-index:1050;",
      actionButton(
        inputId = ".shinymanager_logout",
        label   = "Logout",
        icon    = icon("right-from-bracket"),
        class   = "btn-danger"
      )
    ),

    # title
    div(column(12,
        includeHTML("www/0_Ueberschrift.htm"),
        br(),
      ),style = 'width:1200px;'
    ),
    
    # company logos
    fluidRow(align = "right",
             column(12,
                    img(src = "IKSF.png", height = 40, width = 150),
                    style = 'width:1200px;')
    ),
    
    ########### INCLUDE HTML PAGES ###########
    
    fluidRow(
    column(12,
           hr(),
           style = 'width:1200px;'
           )
    ),
    
    fluidPage(
      fluidRow(
        
        # include HTML pages in the beginning
        
        div(
          wellPanel(style = "background: white",
                    br(),
                    includeHTML("www/1_Anf.htm"),
                    hr(),
                    br(),
                    includeHTML("www/2_I_UeberdasSDF.htm"),
                    br(),
                    HTML('<center><img src="JobDC.png" width="400"></center>'), 
                    br(),
                    includeHTML("www/3_GrafikLegende.htm"),
                    br(),
                    hr(),
                    br(),
                    includeHTML("www/4_II_ZumVorgehen.htm"),
          ),style = 'width:1200px;'
        )
      )
    ),
    
    ########### FIRST TIME USAGE ###########
    
    fluidPage(
      fluidRow(
        
        # Ask for first time usage
        column(12,
               hr(),
               br(),
               style = 'width:1200px;'),
        
        
        column(12,
               includeHTML("www/5_QuantitativerTeil.htm"),
               style = 'width:1200px;'),
        
        column(12,
               prettyRadioButtons(
                 label = "Bitte wählen Sie aus:",
                 choices = c(
                   "Nichts ausgewählt",
                   "Ja",
                   "Nein"),
                 icon = icon("check"),
                 inline = TRUE,
                 animation = "tada",
                 status = "default",
                 inputId = "ItsMyFirstTime",
                 selected = "Nichts ausgewählt"
               ),
               bsTooltip("ItsMyFirstTime","Ohne Wahl einer Antwort kann nicht fortgefahren werden"),
               align = "center",
               style = "margin-bottom: 10px;",
               style = "margin-top: 10px;",
               style = 'width:1200px;')
      )
    ),
    tags$head(tags$style("#ItsMyFirstTime{
                                 font-size: 16px;
                                 font-style: arial;
                                 }"
                         )
              ),
    
    fluidRow(
      column(12,
             hr(),
             style = 'width:1200px;'
      )
    ),
    
    ############ STEP 1 ###########
    
    fluidRow(
      sidebarLayout(
        sidebarPanel(id = "sidebar",
                     
                     includeHTML("www/SideBar1.htm"),
                     
                     disabled(actionButton("EditDat","Editieren",icon("edit"),class = "btn btn-danger")),
                     bsTooltip("EditDat","Mit diesem Button kann der neben stehende Skill-Pool modifiziert werden."),
                     br(),
                     hr(),
                     disabled(actionButton("Confirm", "Bestätigen",icon("check"),class = "btn btn-danger")),
                     bsTooltip("Confirm","Mit diesem Button bestätigen Sie Ihre Auswahl."),
                     hr(),
                     
                     includeHTML("www/SideBar1ASM.htm"),
                     
                     disabled(prettyCheckbox("AdvancedMode", "Advanced-Statistics-Modus", 
                                             animation = "pulse",
                                             shape = "curve",
                                             icon = icon("check"))),
                     bsTooltip("AdvancedMode","Wenn Sie dieses Häkchen setzen und den Bestätigen-Button bedienen, haben Sie bei der Eingabe Ihrer Daten in den nächsten Schritten weitere Einstellmöglichkeiten."),
                     width = 4
        ),
        mainPanel(
          div(
            wellPanel(
              list(
                tags$head(
                  
                  # HTML DEFINE GROUP CHECKBOX IN 3 COLUMNS
                  
                  tags$style(HTML(
                    "
                       .multicol {
                       -webkit-column-count: 3; /* Chrome, Safari, Opera */
                       -moz-column-count: 3;    /* Firefox */
                       column-count: 3;
                       -moz-column-fill: auto;
                       -column-fill: auto;
                       }
                       "
                  ))
                )
              ),
              br(),
              
              list(tags$span(style = "color: black; font-size: 16px; font-style: arial","Allgemeiner Skill-Pool:"),
                   tags$br(),
                   tags$br(),
                   tags$div(align = 'left',
                            class = 'multicol',
                            uiOutput("GroupCheckbox"),
                   )
              ),
            )
            ,style = 'width:795px;')
        )
      ),
      
      ############ STEP 2 ###########
      
      #mimic click outside to close the DropdownButton
      tags$head(
        tags$script("Shiny.addCustomMessageHandler('close_drop', function(x){
                  $('html').click();
                });")
      ),
      
      #textOutput einfügen
      sidebarLayout(
        uiOutput("show_text_step2"),
        mainPanel(
          div(
            setSliderColor(rep("#ff0101",1000),seq(1000)),
            uiOutput("show_slider_inputs"),
            style = 'width:795px;'))
        ),
      
      ############ STEP 3 ###########
      
      sidebarLayout(
        uiOutput("show_text_step3"),
        mainPanel(div(
          uiOutput("showCalcStep3"),
          style = 'width:795px;'))
        ),
      tags$head(tags$style("#showCalcStep3{color: black;
                                 font-size: 16px;
                                 font-style: arial;
                                 }"
                           )
                ),
      
      ############ STEP 4 ###########
      
      sidebarLayout(
        uiOutput("show_text_step4"),
        mainPanel(div(
          shinyjs::hidden(wellPanel(id = "hiddenPanel", 
                                    DT::dataTableOutput('Gesamtauswertung')
          )),
          style = 'width:795px;')
          )
      ),
      
      fluidPage(
        fluidRow(
          mainPanel(
          # HTML output to further describe step 4 interpretation - show only with step 4 
          div(
            htmlOutput('DescribeStep4'),
            style = 'width:1200px;')
          ),
          ########### HTML PAGE OUTPUT - IMPRESSUM ###########
          
          column(12,
                 tags$span(style = "color: black; font-size: 16px; font-style: arial","III Weitere Informationen und Kontakt"),
                 dropdownButton(wellPanel(htmlOutput('DescribeInfo4')),
                                circle = TRUE, 
                                status = "danger", 
                                icon = icon("gear"), 
                                width = "1200px",
                                tooltip = tooltipOptions(title = "Kontakt und weitere Informationen")
                 ),
                 align="center")
        )
      )
    )
  )
)