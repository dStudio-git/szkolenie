library(plyr)
library(dplyr)
library(ggplot2)
library(qcc)
library(reshape2)
library(shinythemes)
library(shinydashboard)
library(FrF2)
library(ggrepel)
library(extrafont)
library(Rttf2pt1)
library(extrafontdb)
library(Cairo)
library(ggthemes)
library(DT)


dashboardPage(skin = "black",
  dashboardHeader(title = "MSE / COV / DoE"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("MSE", tabName = "MSE", icon = icon("line-chart"), badgeLabel = "", badgeColor = "green"),
      menuItem("COV", tabName = "COV", icon = icon("reorder"), badgeLabel = "", badgeColor = "yellow"),
      menuItem("DoE", tabName = "DoE", icon = icon("th"), badgeLabel = "", badgeColor = "yellow")
    )
  ),
dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "superhero.css")
  ),
  tabItems(
    tabItem(tabName = "MSE",
      fluidRow(
        tabBox(side="left",width = 12, title = NULL, selected = "Dane MSE", 
          tabPanel(title = "Dane MSE",
            br(), br(),
            fluidRow(
              column(width=5,
                box(title = "Wyniki MSE", status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, width = 12,                                    column(width=6,
                    numericInput("operatorzy", label = h4("Operatorzy"), value = 2),
                    numericInput("czesci", label = h4("Czesci:"), value = 5)
                  ),
                  column(width=6,
                    numericInput("pomiary", label = h4("Pomiary"), value = 3),
                    h4("Pobierz wzór"),
                    downloadButton('downloadData', 'Pobierz')
                  )
                ),
                
                box(title = "Struktura MSE", status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, width = 12,
                  fileInput('file1', label = '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                )
              ),
              column(width=7,
                box(title = "Tabela MSE", status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, width = 12,
                  DT::dataTableOutput('tabela')
                )
              )
            )

          ), # Koniec TabPanel MSE
          tabPanel(title = "Analiza Praktyczna", br(),
            fluidRow(
              column(width = 12,
                box(title = "Box Plot", status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, width = 12,
                  plotOutput('practical', width = "100%"), br()
                )
              )  
            ),
            fluidRow(
              column(width=3,
                box(title = "Wprowadz zakres", status = "primary", height=240, solidHeader = TRUE,
                    collapsible = FALSE, collapsed = FALSE, width = 12,
                  uiOutput('tolerancje.DGT'),
                  uiOutput('tolerancje.GGT')
                )
              ), 
              column(width=3,
                box(title = "Rysuj", status = "primary", height=240, solidHeader = TRUE,
                    collapsible = FALSE, collapsed = FALSE, width = 12,
                  radioButtons("rysuj.praktyczna", label = h5("Obrys:"), 
                      choices = list("tylko punkty"=0, "czesci" = 1, "+ operatorzy" = 2, "+ trendy" = 3),selected = NULL)
                )
              ),
              column(width=6,
                box(title = "Interpretacja analizy praktycznej", status = "primary", solidHeader = TRUE,
                    collapsible = FALSE, collapsed = FALSE, width = 12,
                  h6("✔ Określ granice specyfikacji badanych częsci "),
                  h6("✔ Odpowiedz na pytania: (uruchom kolejne opcje pola 'Rysuj'"),
                  h6("   • czy uchwycono realną zmienność?"),
                  h6("   • które źródło zmienności dominuje?"),
                  h6("   • czy obserujemy trendy / nielosowe przebiegi?"),
                  h6("   • czy obserujemy potencjalne przyczyny specjalne?")
                ) 
              )      
            )
        ), # koniec tabPanel Analiza Praktyczna
        tabPanel(title = "Analiza Graficzna", br(),
          fluidRow(
            column(width=12,
              box(title = "Wyniki MSE", status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, width = 12,
                plotOutput('graficznaX',height="200px"),
                plotOutput('graficznaR',height="200px")
              )  
            )
          ),
          br(),
          fluidRow(
            column(width=6, offset = 6,
              box(title = "Interpretacja analizy graficznej", status = "primary", solidHeader = TRUE,
                  collapsible = FALSE, collapsed = FALSE, width = 12,
                h6("✔ Okdpowiedz na pytania"),
                h6("   • (karta R) Czy zmnienność pomiar-do-pomiaru jest SPC?"),
                h6("   • (karta X) Czy istnieje dowód na zmienność część-do-części?")
              )
            )
          )
        ), # koniec tabPanel Analiza Graficzna
        tabPanel(title = "Gage R&R",
          fluidRow(
            HTML("<div class='col-sm-4' style='min-width: 355px !important;'>"),
            box(title = "Wariancja", width = NULL, status="primary", solidHeader = TRUE,
              tableOutput('GageRNR_wariancja')),
            HTML("</div>"),
            HTML("<div class='col-sm-4' style='min-width: 510px !important;'>"),
            box(title = "StudyVar", width = NULL, status="primary", solidHeader = TRUE,
              tableOutput('GageRNR_odchylenie')
            ),
            HTML("</div>")
          ), #koniec fluidRow
          fluidRow(
            column(width = 4,valueBoxOutput("distinct.categories", width=NULL)),
            column(width = 4,valueBoxOutput("process.tolerance", width=NULL)),
            column(width = 4, 
              numericInput("StudyVar", label = h4("Study Variation"), value = 5.15, step=0.05)
            )# koniec column
          ) #koniec fluidRow
        )#koniec Gage R&R
        ) #koniec tabBox
      )
    ), #koniec tab "MSE"
    tabItem(tabName = "COV"
    ),
    tabItem(tabName = "DoE",
      fluidRow(
        tabBox(side="left",width = 12, title = NULL, selected = "Dane DoE", 
          tabPanel(title = "Dane DoE",
            br(), br(),
            fluidRow(
              column(width=5,
                box(title = "Wyniki DoE", status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, width = 12,
                  fileInput('fileWynik', label = '',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                ),
                box(title = "Struktura DoE", status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, width = 12,
                  downloadButton('downloadRDA', 'Pobierz')
                )
              ),
              column(width=7,
                box(title = "Tabela DOE", status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, width = 12,
                  DT::dataTableOutput('tabela.DoE')
                )
              )
            ) 
          ), # koniec Tab Panel "Dane DoE"
          tabPanel(title = "Analiza Praktyczna",
            fluidRow(
              column(width=6,
                box(title = "Time Series Plot", status = "primary", solidHeader = TRUE,
                    collapsible = FALSE, collapsed = FALSE, width = 12,
                  plotOutput('rysuj.ANOG')
                )
              ),   
              column(width=6,
                box(title = "Scatter Plot", status = "primary", solidHeader = TRUE,
                    collapsible = FALSE, collapsed = FALSE, width = 12,
                  plotOutput('rysuj.SCATTER')
                )
              ) 
            )
          ), # koniec Tab Panel "Analiza Praktyczna"
          tabPanel(title = "Graficzna: Efekty glowne i interakcje",
            fluidRow(
              column(width=6,
              box(title = "Wykres efektow glownych", status = "primary", solidHeader = TRUE,
                  collapsible = FALSE, collapsed = FALSE, width = 12,
                plotOutput('MEPlot')
                )
              ),# koniec kolumny     
              column(width=6,
                box(title = "Wykres interakcji", status = "primary", solidHeader = TRUE,
                    collapsible = FALSE, collapsed = FALSE, width = 12,
                  plotOutput('IEPlot')
                )
              )     
            ), #koniec FluidRow
            fluidRow(
              column(width=6,DT::dataTableOutput('tabela.efekty'))
            )
          ), # koniec Tab Panel "Efekty glowne i interakcje"
          tabPanel(title = "Graficzna: Pareto & NPP",
            fluidRow(
              column(width=6,
                box(title = "Normal / HalfNormal Plot", status = "primary", solidHeader = TRUE,
                    collapsible = FALSE, collapsed = FALSE, width = 12,
                  plotOutput('NormalPlot')
                )
              ), # koniec kolumny
              column(width=6,
                box(title = "Pareto Plot", status = "primary", solidHeader = TRUE,
                    collapsible = FALSE, collapsed = FALSE, width = 12,
                  plotOutput('ParetoPlot')
                )
              )
            ), #koniec FluidRow
            fluidRow(
              column(width=4,
                box(title = "PSE", status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, width = 12,
                  valueBoxOutput('PSE', width=NULL)
                )
              )# koniec kolumny 
            )
          ) # koniec Tab Panel "Analiza Praktyczna"
        ) # koniec TAB Box "Analiza systemu pomiarowego"
      ) # koniec FLUID ROW calosciowy
    ) # koniec TAB ITEM DOE
  ) # koniec TAB ITEMS
) # koniec DASHBOARD BODY
)
