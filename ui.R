library(leaflet)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)
library(plotly)
library(lubridate)
library(shinyjs)
library(highcharter)

shinyUI(fluidPage(
  # Cargar una hoja de estilo customizada
  includeCSS("www/style/style.css"),
  includeCSS('www/style/carousel.css'),
  
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # Cargando Layout
  dashboardPage(
    
    skin = "blue",
    
    dashboardHeader(title="Analisis RPA Security", titleWidth = 200),
    
    dashboardSidebar(width = 200,
                     sidebarMenu(
                       HTML(paste0(
                         "<br>",
                         "<a href='https://primus.ai' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://primus.ai/wp-content/uploads/2019/12/logo-primus-home-6.png' width = '186'></a>",
                         "<br>",
                         "<a href='https://personas.bancosecurity.cl' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://personas.bancosecurity.cl/include/img/logo.png' width = '186'></a>",
                         "<br>",
                         
                         "<br>"
                       )),
                       menuItem("Home", tabName = "home", icon = icon("home")),
                       
                       menuItem("Resumen Mensual", tabName = "monthly", icon = icon("thumbtack")),
                       
                       menuItem("Tablas", tabName = "table", icon = icon("table"), startExpanded = F,
                                menuSubItem('Resumen por Dia', tabName = "tabla_por_dia", icon = icon('table')),
                                menuSubItem('Bitacora', tabName = "tabla_bitacora", icon = icon('table')),
                                menuSubItem('RAW Datos Sesiones', tabName = "raw_data_sessions", icon = icon("table")),
                                menuSubItem('RAW Datos Trabajo', tabName = 'raw_data_trabajo', icon = icon('table'))
                       ),
                       
                       menuItem('Metricas', tabName = 'metricas', icon = icon('stats', lib ='glyphicon' ), startExpanded = F,
                                menuSubItem('Medidas Particulares', tabName = 'charts2', icon=icon('stats', lib = 'glyphicon')),
                                menuSubItem("Medidas Generales", tabName = "charts1", icon = icon("stats", lib = "glyphicon")),
                                menuSubItem("Indicadores", tabName = "KPIs", icon = icon("map marked alt"))
                       ),
                       HTML(paste0(
                         "<br><br><br><br><br><br><br><br><br>",
                         "<table style='margin-left:auto; margin-right:auto;'>",
                         "<tr>",
                         "<td style='padding: 5px;'><a href='https://www.linkedin.com/company/primusai/' target='_blank'><i class='fab fa-linkedin-in'></i></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.linkedin.com/company/primusai/' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.primus.ai' target='_blank'><i class='fas fa-globe'></i></a></td>",
                         "</tr>",
                         "</table>",
                         "<br>"),
                         HTML(paste0(
                           "<script>",
                           "var today = new Date();",
                           "var yyyy = today.getFullYear();",
                           "</script>",
                           "<p style = 'text-align: center;'><small>&copy; - <a href='https://primus.ai' target='_blank'>primus.ai</a> - <script>document.write(yyyy);</script></small></p>")
                         ))
                     )
                     
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "home",
                xml2::write_html(rvest::html_node(xml2::read_html("Home.html", encoding = 'UTF-8'), "body"), file = "Home2.html", encoding = 'UTF-8'),
                # home section
                includeHTML('Home2.html')
        ),
        
        tabItem(tabName = "monthly",
                
                fluidRow(
                  box(width = 6, height = NULL, solidHeader = TRUE, status = "primary",
                      title = "Hero Plot",
                      highchartOutput('monthly_qwork_psuccs')
                  ),
                  
                  column(width = 6,
                         column(width = 6,
                                box(width = NULL, solidHeader = TRUE, status = "primary",
                                    title = ""
                                    #Plot
                                ),
                                box(width = NULL, solidHeader = TRUE, status = "primary",
                                    title = "Two"
                                    #Plot
                                )
                         ),
                         column(width = 6,
                                box(width = NULL, solidHeader = TRUE, status = "primary",
                                    title = "Three"
                                    #Plot
                                ),
                                box(width = NULL, solidHeader = TRUE, status = "primary",
                                    title = "Four"
                                    #Plot
                                )
                         )
                  )
                ),
                
                fluidRow(
                  box(width = 12, title='five', solidHeader = TRUE,
                      status = 'primary',
                      highchartOutput('monthly_rnodisp_causas', height = '50vh'))
                )
                
                
                
        ),
        
        tabItem(
          # species data section
          tabName = "table", dataTableOutput("speciesDataTable") %>% withSpinner(color = "green")
          
        ),
        
        tabItem(tabName = "charts1", 
                fluidRow(box(width = 8, height = NULL, solidHeader = TRUE, status = 'primary',
                    title = 'Uso de Licencias'),
                    box(width = 4, height = NULL, solidHeader = TRUE, status = 'primary',
                        title = 'Uso comparativo por robot(%)')),
                fluidRow(
                  column(width = 8,
                         column(width = 8,
                                box(width = 8,height = '30vh', solidHeader = TRUE, status = "primary",
                                    title = "One"
                                    #Plot
                                ),
                                box(width = 8, height = '30vh', solidHeader = TRUE, status = "primary",
                                    title = "Two"
                                    #Plot
                                )
                         ),
                         column(width = 4,
                                box(width = NULL, solidHeader = TRUE, status = "primary",
                                    title = "Three"
                                    #Plot
                                )
                         )
                  )
                )
      
        ),
        
        
        tabItem(tabName = 'raw_data_sessions', 
                fluidPage(column(width=9, box( title = 'Data sin Procesar', 
                                               width = NULL, solidHeader = TRUE, status = 'primary',
                                               collapsible = TRUE, dataTableOutput("sessions_table"))),
                          useShinyjs(),
                          inlineCSS(list("table" = "font-size: 8px")))),
        
        tabItem(tabName = 'raw_data_sessions',
                fluidPage(column(width=9, box( tabsetPanel(type = "tabs",
                                                           tabPanel("Plot", plotOutput("plot")),
                                                           tabPanel("Summary", verbatimTextOutput("summary")),
                                                           tabPanel("Table", tableOutput("table"))
                )),
                useShinyjs(),
                inlineCSS(list("table" = "font-size: 8px")))),
                
                
        ),
        
        
        tabItem(tabName = 'raw_data_trabajo', 
                fluidPage(column(width=9, box( title = 'Data sin Procesar', 
                                               width = NULL, solidHeader = TRUE, status = 'primary',
                                               collapsible = TRUE, dataTableOutput("raw_trabajo_table"))),
                          useShinyjs(),
                          inlineCSS(list("table" = "font-size: 8px")))),
        tabItem(tabName = 'tabla_por_dia', 
                fluidPage(column(width=6, box( title = 'Resumen por Dia', 
                                               width = NULL, solidHeader = TRUE, status = 'primary',
                                               collapsible = TRUE, dataTableOutput("tabla_x_dia"))),
                          column(width = 6, box(title = 'Grafico 1', width = NULL, solidHeader = TRUE,
                                                status = 'primary', callapsible = TRUE)),
                          useShinyjs(),
                          inlineCSS(list("table" = "font-size: 8px")))
                
        ),
        
        tabItem(tabName = "charts2",
                
                # Puntual
                fluidRow(column(4,align="center", offset = 2, uiOutput("categorySelectComboChart")),
                         column(3,align="center", offset = 0, dateRangeInput('dateRange',
                                                                             label = 'Seleccione la fecha:',
                                                                             start = data_trabajo$Date[1],
                                                                             end = data_trabajo$Date[length(data_trabajo$Date)]
                         ))),
                fluidRow(
                  valueBoxOutput("ExTotBox", width = 3) %>% withSpinner(color = 'gray'),
                  valueBoxOutput("ImTotBox", width = 3) %>% withSpinner(color = 'gray'),
                  valueBoxOutput("BlTotBox", width = 3) %>% withSpinner(color = 'gray'),
                  valueBoxOutput('TotCost', width = 3) %>%  withSpinner(color = 'gray')
                ), 
                
                
                
                fluidRow(
                  box(width = 6, height = NULL, solidHeader = TRUE, status = "primary",
                      title = "Hero Plot",
                      plotlyOutput('lollipop')
                  ),
                  
                  column(width = 6,
                         column(width = 6,

                                box(width = NULL, solidHeader = TRUE, status = "primary",
                                    title = "Two",
                                    dataTableOutput('lolli_table')
                                    #Plot
                                )
                         ),
                         column(width = 6,
                                box(width = NULL, solidHeader = TRUE, status = "primary",
                                    title = "Three",
                                    plotlyOutput('performance_ratio')
                                )
                                )
                         )
                ),
                
                
                
                
                
                fluidRow(
                  box(title = 'Sesiones Ok vs Recursos No Disponibles ', width = 6, solidHeader = TRUE, status = 'primary',
                               collapsible = TRUE,
                               plotlyOutput("double_axis", height = '60vh') %>% withSpinner(color = "gray")),
                column(width = 6, 
                       column(width = 6,
                              box(width = NULL, solidHeader = TRUE, status = "primary",
                                  title = "Recursos no disponibles: Causas",
                                  plotlyOutput('performance_double_axis1', height = '60vh')
                                  #Plot
                                  )
                                  ),
                       column(width = 6,
                              box(width = NULL, solidHeader = TRUE, status = "primary",
                                  title = "Three",
                                  #Plot
                                  plotlyOutput('performance_double_axis2', height = '40vh') %>% withSpinner(color = 'gray')
                              )
                ))),
                
                
                fluidRow(
                  column(6, box( title = 'Trabajo', width = NULL, solidHeader = TRUE, 
                                 status = 'primary', collapsible = TRUE,
                                 plotlyOutput('work_double_axis'))),
                  column(6, box( title = 'Excepciones de Trabajo: Causas', width = NULL, solidHeader = TRUE, 
                                 status = 'primary', collapsible = TRUE, 
                                 plotlyOutput('plotlyPlot4') %>%  withSpinner(color = 'gray')))
                )
        ),
        
        tabItem(tabName = "KPIs",

                fluidRow(
                  column(3, uiOutput("statesSelectCombo")),
                  column(3, uiOutput("categorySelectComboChoro"))
                ),
                fluidRow(
                  column(3,tableOutput('stateCategoryList') %>% withSpinner(color = "green")),
                  column(9,leafletOutput("choroplethCategoriesPerState") %>% withSpinner(color = "green"))
                )
                
        )
        
      )
      
    ) # end dashboardBody
    
  )# end dashboardPage
  
))

