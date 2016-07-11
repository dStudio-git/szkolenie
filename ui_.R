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
menuItem("COV", tabName = "COV", icon = icon("reorder"), badgeLabel = "TBD", badgeColor = "yellow"),
menuItem("DoE", tabName = "DoE", icon = icon("th"), badgeLabel = "pending", badgeColor = "yellow")
)
),
dashboardBody(
tags$head(tags$style(HTML('
.skin-black .left-side, .skin-black .main-sidebar, .skin-black .wrapper {
background-color: #2F4154;
}
.skin-black .sidebar-menu>li.active>a, .skin-black .sidebar-menu>li:hover>a
{
color: rgba(255,255,255,.8);
background: #34495E;
border-left-color: #4c4c4c;
padding: 15px 15px 15px 15px;
display: block;
border-bottom: 1px solid rgba(255,255,255,.05);
}
.skin-black .sidebar-menu>li>a{
color: rgba(255,255,255,.5);
padding: 15px 15px 15px 15px;
display: block;
border-bottom: 1px solid rgba(255,255,255,.05);
}
.box.box-solid.box-primary>.box-header {
color: #FFF;
background: #F6F6F6;
font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
font-size: 18px;
font-weight: normal;
}
.box.box-solid.box-primary {
border: 1px solid #e9e9e9;
}
.nav-tabs-custom>.nav-tabs>li.active {
border-top-color: #cccccc;
font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
font-size: 18px;
font-weight: normal;
font-style: italic;
}
.nav-tabs-custom>.nav-tabs>li>a, .nav-tabs-custom>.nav-tabs>li>a:hover {
font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
font-size: 16px;
font-weight: 500;
font-style: normal;
background: 0 0;
margin: 0;
}
.box-header .box-title, .box-header>.fa, .box-header>.glyphicon, .box-header>.ion {
display: inline-block;
font-size: 14px;
font-weight: 700;
text-transform: uppercase;
margin: 0;
line-height: 1.42857;
}
.checkbox label {
font-size: 14px;
font-weight: 100;
}
body {
font-family: "Lato", "Open Sans","Helvetica Neue",Arial,sans-serif;
font-size: 14px;
font-weight: normal;
}
label {
display: inline-block;
max-width: 100%;
margin-bottom: 5px;
font-weight: 500;
}
.main-header .logo {
font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
font-size: 24px;
font-weight: normal;
}
.h1, .h5, h1, h5 {
font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
font-size: 16px;
line-height: 1.1;
color: inherit;
}
.h3, h3{
font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
font-size: 14px;
font-weight: 700;
font-style: normal;
line-height: 1.1;
color: #626262;
}
.h2, h2{
font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
font-size: 14px;
font-weight: 700;
font-style: normal;
line-height: 1.1;
color: #626262;
text-transform: uppercase;
}
.h4, h4{
font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
font-size: 18px;
font-weight: 600;
font-style: normal;
line-height: 1.1;
color: #5B90BF;
}
                                          
                                          .h6, h6{
                                          font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-size: 13px;
                                          font-weight: normal;
                                          font-style: normal;
                                          line-height: 1.1;
                                          color: inherit;
                                          }
                                          .irs-bar {
                                          height: 8px;
                                          top: 25px;
                                          border-top: 1px solid #2F4154;
                                          border-bottom: 1px solid #2F4154;
                                          background: #2F4154;
                                          }
                                          .irs-bar-edge {
                                          height: 8px;
                                          top: 25px;
                                          width: 14px;
                                          border: 1px solid #2F4154;
                                          border-right: 0;
                                          background: #2F4154;
                                          border-radius: 16px 0 0 16px;
                                          Unknown property name.-moz-border-radius: 16px 0 0 16px;
                                          }
                                          .nav-tabs-custom>.nav-tabs>li.header {
                                          font-family: "Open Sans","Helvetica Neue",Arial,sans-serif;
                                          font-weight: 500;
                                          line-height: 35px;
                                          padding: 0 10px;
                                          font-size: 24px;
                                          font-style: normal;
                                          color: #444;
                                          }
                                          .progress-bar {
                                          float: left;
                                          width: 0;
                                          height: 100%;
                                          font-size: 12px;
                                          line-height: 20px;
                                          color: #000000;
                                          text-align: center;
                                          background: #cccccc;
                                          -webkit-box-shadow: inset 0 -1px 0 rgba(0,0,0,.15);
                                          box-shadow: inset 0 -1px 0 rgba(0,0,0,.15);
                                          -webkit-transition: width .6s ease;
                                          Unknown property name.-o-transition: width .6s ease;
                                          transition: width 3s ease;
                                          }
                                          .pagination>.active>a, .pagination>.active>a:focus, .pagination>.active>a:hover, .pagination>.active>span, .pagination>.active>span:focus, .pagination>.active>span:hover {
                                          z-index: 2;
                                          color: #fff;
                                          cursor: default;
                                          background-color: #cccccc;
                                          border-color: #cccccc;
                                          }
                                          .sidebar {
                                          font-size: 13px;
                                          color: #444;
                                          font-weight: 700;
                                          }
                                          .selectize-input, .selectize-input input {
                                          color: #333333;
                                          font-family: inherit;
                                          font-size: inherit;
                                          line-height: 23px;
                                          
                                          }
                                          .box.box-solid.box-primary>.box-header .btn, .box.box-solid.box-primary>.box-header a {
                                          color: #34495E;}
                                          
                                          '))),
                
                
tabItems(
tabItem(tabName = "MSE",
fluidRow(
tabBox(side="left",width = 12, title = NULL, selected = "Dane MSE", 
                                   tabPanel(title = "Dane MSE", 
                                            fluidRow(
                                              column(width=8,
                                                     box(title = "Struktura MSE", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         column(width=4, numericInput("operatorzy", label = h5("Operatorzy"), value = 2), downloadButton('downloadData', 'Pobierz')),
                                                         column(width=4, numericInput("czesci", label = h5("Czesci:"), value = 5)),
                                                         column(width=4, numericInput("pomiary", label = h5("Pomiary"), value = 3)
                                                         )
                                                         
                                                     ) #koniec box
                                              ),
                                              column(width=4, box(title = "Wyniki pomiarow", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                                  
                                                                  fileInput('file1', label = '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                                              ))),
                                            
                                            fluidRow(
                                              column(width=12,DT::dataTableOutput('tabela')
                                                     
                                                     
                                              )) #koniec fluidRow
                                            
                                            
                                   ),
                                   tabPanel(title = "Analiza Praktyczna", br(),
                                            fluidRow(
                                              column(width = 12,plotOutput('practical', width = "100%"), br()
                                              ) #koniec column
                                            ),# koniec fluidRow
                                            fluidRow(
                                              
                                              column(width=3,
                                                     box(title = "Wprowadz zakres", status = "primary", height=240, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         uiOutput('tolerancje.DGT'),
                                                         uiOutput('tolerancje.GGT')
                                                     ) #koniec box
                                              ), # koniec column
                                              column(width=3,
                                                     box(title = "Rysuj", status = "primary", height=240, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         radioButtons("rysuj.praktyczna", label = h5("Obrys:"), 
                                                                      choices = list("tylko punkty"=0, "czesci" = 1, "+ operatorzy" = 2, "+ trendy" = 3),
                                                                      selected = NULL)
                                                         
                                                     ) #koniec box
                                              ),
                                              column(width=6,
                                                     box(title = "Interpretacja analizy praktycznej", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         h6("✔ Określ granice specyfikacji badanych częsci "),
                                                         h6("✔ Odpowiedz na pytania: (uruchom kolejne opcje pola 'Rysuj'"),
                                                         h6("   • czy uchwycono realną zmienność?"),
                                                         h6("   • które źródło zmienności dominuje?"),
                                                         h6("   • czy obserujemy trendy / nielosowe przebiegi?"),
                                                         h6("   • czy obserujemy potencjalne przyczyny specjalne?")
                                                     ) #koniec box
                                              ) #koniec kolumny       
                                            )# koniec fluidRow
                                            
                                   ), #koniec tabPanel
                                   tabPanel(title = "Analiza Graficzna", br(),
                                            fluidRow(
                                              column(width=12,
                                                     plotOutput('graficznaX',height="200px"),
                                                     plotOutput('graficznaR',height="200px"))
                                              
                                            ),
                                            br(),
                                            fluidRow(
                                              column(width=6, offset = 6,
                                                     box(title = "Interpretacja analizy graficznej", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         h6("✔ Okdpowiedz na pytania"),
                                                         h6("   • (karta R) Czy zmnienność pomiar-do-pomiaru jest SPC?"),
                                                         h6("   • (karta X) Czy istnieje dowód na zmienność część-do-części?")
                                                         
                                                     )
                                              )
                                            )),
                                   tabPanel(title = "Gage R&R",
                                            fluidRow(
                                              HTML("<div class='col-sm-4' style='min-width: 355px !important;'>"),
                                              box(
                                                title = "Wariancja", width = NULL, status="primary", solidHeader = TRUE,
                                                tableOutput('GageRNR_wariancja')),
                                              HTML("</div>"),
                                              HTML("<div class='col-sm-4' style='min-width: 510px !important;'>"),
                                              box(
                                                title = "StudyVar", width = NULL, status="primary", solidHeader = TRUE,
                                                tableOutput('GageRNR_odchylenie')
                                              ),
                                              HTML("</div>")
                                            ), #koniec fluidRow
                                            fluidRow(
                                              column(width = 4,valueBoxOutput("distinct.categories", width=NULL)
                                              ),
                                              column(width = 4,valueBoxOutput("process.tolerance", width=NULL)
                                              )
                                              
                                              ,column(width = 4, 
                                                      
                                                      numericInput("StudyVar", label = h4("Study Variation"), value = 5.15, step=0.05)
                                              )# koniec column
                                              #column(width=4, 
                                              #       infoBoxOutput("ilosciowa.procenty.pomiary", width=NULL)
                                              #)# koniec column
                                            ) #koniec fluidRow
                                            
                                   )#koniec Gage R&R
                                   
                            ) #koniec tabBox
                          )
                  ), #koniec tab "MSE"
                  
                  tabItem(tabName = "COV"
                  ),
                  tabItem(tabName = "DoE",
                          fluidRow(
                            tabBox(side="left",width = 12, title = "(DoE) Planowane Eksperymenty", selected = "Dane DoE", 
                                   tabPanel(title = "Dane DoE",
                                            fluidRow(
                                              column(width=3,
                                                     box(title = "Wyniki DoE", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         fileInput('fileWynik', label = '',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                                                     ),
                                                     box(title = "Struktura DoE", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         downloadButton('downloadRDA', 'Pobierz')
                                                     )),
                                              
                                              column(width=9,
                                                     box(title = "Tabela DOE", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         DT::dataTableOutput('tabela.DoE'))
                                              )
                                              
                                            ) # koniec fluidRow
                                            
                                            
                                   ), # koniec Tab Panel "Dane MSE"
                                   tabPanel(title = "Analiza Praktyczna",
                                            fluidRow(
                                              column(width=6,
                                                     box(title = "Time Series Plot", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         plotOutput('rysuj.ANOG')
                                                     )),# koniec kolumny     
                                              column(width=6,
                                                     box(title = "Scatter Plot", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         plotOutput('rysuj.SCATTER')
                                                     )) # koniec kolumny  
                                            ) #koniec FluidRow
                                   ), # koniec Tab Panel "Analiza Praktyczna"
                                   tabPanel(title = "Graficzna: Efekty glowne i interakcje",
                                            fluidRow(
                                              column(width=6,
                                                     box(title = "Wykres efektow glownych", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         plotOutput('MEPlot')
                                                     )),# koniec kolumny     
                                              column(width=6,
                                                     box(title = "Wykres interakcji", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         plotOutput('IEPlot')
                                                     ))# koniec kolumny     
                                              
                                            ), #koniec FluidRow
                                            fluidRow(
                                              column(width=6,
                                                     
                                                     DT::dataTableOutput('tabela.efekty')
                                              )
                                            )
                                   ), # koniec Tab Panel "Efekty glowne i interakcje"
                                   tabPanel(title = "Graficzna: Pareto & NPP",
                                            fluidRow(
                                              column(width=6,
                                                     box(title = "Normal / HalfNormal Plot", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         plotOutput('NormalPlot')
                                                     )), # koniec kolumny
                                              column(width=6,
                                                     box(title = "Pareto Plot", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         plotOutput('ParetoPlot')
                                                     ))
                                            ), #koniec FluidRow
                                            fluidRow(
                                              column(width=4,
                                                     box(title = "PSE", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12,
                                                         valueBoxOutput('PSE', width=NULL)
                                                     ))# koniec kolumny 
                                            )
                                   ) # koniec Tab Panel "Analiza Praktyczna"
                                   
                                   
                            ) # koniec TAB Box "Analiza systemu pomiarowego"
                            
                          ) # koniec FLUID ROW calosciowy
                  ) # koniec TAB ITEM DOE
                  
                  
                  
                ) # koniec TAB ITEMS
                ) # koniec DASHBOARD BODY
                )
