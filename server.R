####################
####   PRIMUS   ####
####################




############################
### IMPORTANDO LIBRERIAS ###
############################

library(shiny)
library(tidyverse)
library(leaflet.extras)
library(rvest)
library(plotly)
library(lubridate)
library(shinyjs)
library(DT)
library(highcharter)




####################################################
### CREANDO FUNCIONES PARA UTILIZAR MAS ADELANTE ###
####################################################




### 

###########################
### LOGICA DEL SERVIDOR ###
###########################

shinyServer(function(input,output){
  
  
  ##################################################
  ### FILTROS PARA NUESTRAS TABS EN EL DASHBOARD ###
  ##################################################
  
  
  
  
  ### Filtro para robot en tabName = Charts2
  
  
  output$categorySelectComboChart = renderUI({
    selectInput("selectedCategoryChart","Seleccione un robot:", process_names)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #########################################################
  ##### GRAFICOS PARA LA SECCION METRICAS > POR ROBOT #####
  #########################################################
  
  
  ######################################
  ### REACTIVOS PARA RESUMEN MENSUAL ###
  ######################################
  
  
  
  
  
  
  
  
  
  ########################################
  ### REACTIVOS PARA RESUMEN POR ROBOT ###
  ########################################
  
  
  robotPlotlyPlot11 = reactive(data_sessions %>% 
                                 filter(Process_ID == input$selectedCategoryChart) %>% 
                                 filter(Status_Session=='SESION INICIADA OK') %>%
                                 mutate(Date = ymd(Date)) %>%
                                 filter(between(Date, as.Date(input$dateRange[1]), 
                                                as.Date(input$dateRange[2]))) %>%
                                 mutate(Session_count = 1) %>% 
                                 group_by(day = lubridate::floor_date(Date, "day")) %>%
                                 summarize(Sesion_OK = sum(Session_count)))
  
  robotPlotlyPlot12 = reactive(
    data_sessions %>% 
      filter(Process_ID == input$selectedCategoryChart) %>% 
      filter(Status_Session =='RECURSO NO DISPONIBLE') %>%
      mutate(Date = ymd(Date)) %>%
      filter(between(Date, as.Date(input$dateRange[1]), 
                     as.Date(input$dateRange[2]))) %>%
      mutate(Session_count = 1) %>% 
      group_by(day = lubridate::floor_date(Date, "day")) %>%
      summarize(Recurso_No_Disp = sum(Session_count))
  )
  
  ### Reactivo para grafico 2, en tabName = Charts2
  
  
  robotPlotlyPlot21 = reactive(
    data_sessions %>% 
      filter(Process_ID == input$selectedCategoryChart) %>% 
      filter(Status_Session == 'RECURSO NO DISPONIBLE') %>%
      mutate(Date = ymd(Date)) %>%
      filter(between(Date, as.Date(input$dateRange[1]), 
                     as.Date(input$dateRange[2]))) %>%
      mutate(
        Exception_message = case_when(
          # Este si
          grepl('license currently in use', Message, ignore.case = TRUE)~'Licencia en uso',
          # Este si 
          grepl('concurrent sessions', Message, ignore.case = TRUE) ~ 'Licencia en uso',
          # Este si 
          grepl('One or more errors', Message, ignore.case = TRUE) ~ 'Error VDIP',
          
          grepl('resource timed out', Message, ignore.case = TRUE) ~ 'TimeOut VDIP',
          # N
          grepl('Recurso ocupado', Message, ignore.case = TRUE) ~ 'Otro',
          # No es relevante
          grepl('BluePrism', Message, ignore.case = TRUE) ~ 'Otro',
          grepl('database', Message, ignore.case = TRUE) ~ 'Otro'))
    
  ) 
  
  
  ### Reactivo para grafico 3 en tabName = Charts2
  
  renderPlotlyPlot3 = reactive(
    data_trabajo %>% 
      filter(Process_ID == input$selectedCategoryChart) %>% 
      mutate(Date = ymd(Date)) %>%
      filter(between(Date, as.Date(input$dateRange[1]), 
                     as.Date(input$dateRange[2]))))
  
  ### Reactivo para grafico 4 en tabName = Charts2
  
  renderPlotlyPlot4 = reactive(
    
    
    data_excep %>% 
      filter(Process_ID == input$selectedCategoryChart) %>% 
      mutate(Date = ymd(Date)) %>%
      filter(between(Date, as.Date(input$dateRange[1]), 
                     as.Date(input$dateRange[2]))))
  
  renderPlotlyPlot5 = reactive(
    summarized_df %>% 
      filter(Process == input$selectedCategoryChart) %>% 
      mutate(Fecha = ymd(Fecha)) %>% 
      filter(between(Fecha, as.Date(input$dateRange[1]),as.Date(input$dateRange[2]))) %>% 
      mutate( suma = Excepciones_de_Logica_de_Trabajo )
  )
  
  renderlollipop = reactive(
    summarized_df %>%
      
      filter(Process == input$selectedCategoryChart) %>% 
      filter(between(as.Date(Fecha),as.Date(input$dateRange[1]), 
                     as.Date(input$dateRange[2]))) %>% 
      mutate(lollipop = case_when(
      Cantidad_de_Sesiones == 0 ~ -0.5,
      Cantidad_de_Sesiones != 0 ~ 0.5
    ))
  )
  
  render_double_axis = reactive(
    data_sessions %>% 
      filter(Process_ID == input$selectedCategoryChart) %>% 
      
      filter(between(as.Date(Date),as.Date(input$dateRange[1]), 
                     as.Date(input$dateRange[2]))) %>% 
      filter(Status_Session == 'RECURSO NO DISPONIBLE') %>%
      mutate(Date = ymd(Date)) %>%
      mutate(
        Exception_message = case_when(
          # Este si
          grepl('license currently in use', Message, ignore.case = TRUE) ~ 'Licencia en uso',
          # Este si 
          grepl('concurrent sessions', Message, ignore.case = TRUE) ~ 'Licencia en uso',
          # Este si 
          grepl('One or more errors', Message, ignore.case = TRUE) ~ 'Error VDIP',
          
          grepl('resource timed out', Message, ignore.case = TRUE) ~ 'TimeOut VDIP',
          # N
          grepl('Recurso ocupado', Message, ignore.case = TRUE) ~ 'Otro',
          # No es relevante
          grepl('BluePrism', Message, ignore.case = TRUE) ~ 'Otro',
          grepl('database', Message, ignore.case = TRUE) ~ 'Otro'))
  )
  
  render_lollipop_performance1 = reactive(
    data_sessions %>% 
      filter(Process_ID == input$selectedCategoryChart) %>% 
      filter(Status_Session == 'SESION INICIADA OK') %>% 
      filter(between(as.Date(Date), as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
  )
  render_lolli_table = reactive(
    summarized_df2 %>% 
      filter(Process == input$selectedCategoryChart) %>% 
      filter(between(as.Date(Fecha),as.Date(input$dateRange[1]), 
                     as.Date(input$dateRange[2])))
      
  )
  
  render_monthly = reactive(
    daily_summary %>% 
      filter(as.Date(Fecha))

  )

  
  #############################################
  ##### INTERACTIVO/CLICKS SOBRE GRAFICOS #####
  #####    METRICAS > POR ROBOT           #####
  #############################################
  
  lollipop <- reactiveVal()
  


  
  # when clicking on a lollipop, 
  observeEvent(event_data("plotly_click"), {
    lollipop(event_data("plotly_click")$x)
  })
  
  ###
  
  
  ##################################
  #### OUTPUTS RESUMEN MENSUAL #####
  ##################################
  
  
  output$monthly_qwork_psuccs = renderHighchart({
    highchart() %>% 
      hc_yAxis_multiples(list(title = list(text = "% Exito"),
                              lineWidth = 0, labels = list(format = "{value}%"), max = 100),
                         list(title = list(text = "Q. Trabajo"), opposite = TRUE, showlabel=TRUE)) %>%
      hc_xAxis( categories = unique(daily_summary$Fecha), style = list(fontsize = 8) ) %>%
      hc_add_series(daily_summary, "line", hcaes(x = Fecha, y = round(Porcentaje_de_Exito_medio,2)), name = "% Exito Medio", yAxis = 0) %>%
      hc_add_series(daily_summary, "line", hcaes(x = Fecha, y = Cantidad_de_Trabajo),
                    name = "Trabajo realizado",  color = "#FF7900", yAxis = 1) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T ) 
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y} "),
                 headerFormat = '<span style="font-size: 13px">Día {point.key}</span>'
      ) %>%
      hc_legend( layout = 'horizontal', align = 'center', verticalAlign = 'bottom') %>% 
      
      hc_add_theme(hc_theme_538())
    
  })
  
  output$monthly_rnodisp_causas = renderHighchart({
    
    highchart() %>% 
      hc_chart(type = "column") %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_xAxis(categories = unique(daily_bar_summary2$Date)) %>%
      hc_add_series(name="Licencia en Uso", 
                    data= daily_bar_summary2[daily_bar_summary2$Exception_message=='Licencia en uso',]$count,
                    stack = "R. no Disponibles") %>%
      hc_add_series(name="Error VDIP",
                    data = daily_bar_summary2[daily_bar_summary2$Exception_message=='Error VDIP',]$count,
                    stack = "R. no Disponibles") %>%
      hc_add_series(name="Time out VDIP",
                    data = daily_bar_summary2[daily_bar_summary2$Exception_message=='TimeOut VDIP',]$count,
                    stack = "R. no Disponibles") %>%
      hc_add_series(name="Otro",
                    data = daily_bar_summary2[daily_bar_summary2$Exception_message=='Otro',]$count,
                    stack = "R. no Disponibles") %>% 
      hc_plotOptions(column = list(
                      dataLabels = list(enabled = F),
                      #stacking = "normal",
                      enableMouseTracking = T ) 
                    )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y} "),
                 headerFormat = '<span style="font-size: 13px">Día {point.key}</span>') %>% 
      hc_add_theme(hc_theme_538())
                 
    
    
    
  })
  
  

  
  
  
  
  
  
  #######################################
  #### OUTPUTS METRICAS > POR ROBOT #####
  #######################################
  
  
  output$lollipop <- renderPlotly({
    
    
    
      
      line1 <- list(
        type = "line",
        line = list(color ='green'),
        xref = "x",
        yref = "y"
      )
      line2 <- list(
        type = "line",
        line = list(color ='red'),
        xref = "x",
        yref = "y"
      )
      
      lines <- list()
      for ( date in renderlollipop()[renderlollipop()$lollipop>0,]$Fecha) {
        line1[["x0"]] <- date
        line1[["x1"]] <- date
        line1[['y0']] = 0
        line1[['y1']] <- renderlollipop()[renderlollipop()$Fecha==date,]$lollipop
        lines <- c(lines, list(line1))
      }
      for ( date in renderlollipop()[renderlollipop()$lollipop<0,]$Fecha) {
        line2[["x0"]] <- date
        line2[["x1"]] <- date
        line2[['y0']] = 0
        line2[['y1']] <- renderlollipop()[renderlollipop()$Fecha==date,]$lollipop
        lines <- c(lines, list(line2))
      }
    
    fig  = plot_ly()
    fig  = fig %>%  add_trace(x = renderlollipop()[renderlollipop()$lollipop==0.5,]$Fecha,
                              y = renderlollipop()[renderlollipop()$lollipop==0.5,]$lollipop,
                              type = 'scatter',
                              mode = 'markers',
                              marker = options(color='red'),
                              name = 'Corrio')
    fig = fig %>%  add_trace(x = renderlollipop()[renderlollipop()$lollipop==-0.5,]$Fecha,
                             y = renderlollipop()[renderlollipop()$lollipop==-0.5,]$lollipop,
                             type = 'scatter',
                             mode = 'markers',
                             name = 'No Corrio',
                             marker = options(color = 'green'))
    fig = fig %>% layout(fig, shapes = lines)
    fig = fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = TRUE, showline = TRUE, showticklabels = TRUE,
                                      linecolor = 'black', linewidth = 3,side='top',
                                      domain = c(0, 0.85), tickfont = list(size = 8)),
                         yaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)) 
    
    fig <- fig %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                        xanchor = "center",  # use center of legend as anchor
                                        x = 0.4, font = list(size = 10)))
    
    fig
  })
  
  output$lolli_performance1 = renderPlotly({
    data_aux = render_lollipop_performance1()
    date1 = as.vector(unique(data_aux$Date))[1]
  
    seconds1 = seconds(hms(data_aux[as.Date(data_aux$Date) == as.Date(date1),]$Hour[1]) - hms(data_schedule[data_schedule$Process_ID==unique(as.vector(data_aux$Process_ID)),]$Work))
    
    vectr = c(100,0)
    labels = c(' ',' ')
    
    fig1 <- plot_ly(type='pie', labels=labels, values=vectr, 
                   textinfo='none',
                   insidetextorientation='center',
                   marker = list(colors = c('lightgreen', 'lightgreen')),
                   showlegend = F,
                   hoverinfo = 'none')
    fig1 = fig1 %>%
      layout(fig1, annotations=list(text=paste('Atraso de: ',seconds_to_period(seconds1)), "showarrow"=F, font = list(fontsize = 10)))
    fig1 = fig1 %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                         paper_bgcolor = 'rgba(0, 0, 0, 0)',
                         margin = list(l=20))
    fig1 = fig1 %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                       xanchor = "center",  # use center of legend as anchor
                                       x = 0.5)) 
    
    if (is.null(lollipop()[1])) return(fig1)
    date_lolli = lollipop()[1]
    
    data_lolli = data_aux %>% 
      filter(as.Date(Date)==as.Date(date_lolli))
    
    seconds2 = seconds(hms(data_aux[as.Date(data_aux$Date) == as.Date(date_lolli),]$Hour[1]) - hms(data_schedule[data_schedule$Process_ID==unique(as.vector(data_aux$Process_ID)),]$Work))
    
    vectr = c(100,0)
    labels = c(' ',' ')

    fig2 <- plot_ly(type='pie', labels=labels, values=vectr, 
                    textinfo='none',
                    insidetextorientation='center',
                    marker = list(colors = c('lightgreen', 'lightgreen')),
                   showlegend = F,
                    hoverinfo = 'none')
    fig2 = fig2 %>%
      layout(fig2, annotations=list(text=paste('Atraso de: ',seconds_to_period(seconds2)), "showarrow"=F, font = list(fontsize = 10), colors = list('white')))
    fig2 = fig2 %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                           paper_bgcolor = 'rgba(0, 0, 0, 0)',
                           margin = list(l=20))
    fig2 = fig2 %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                         xanchor = "center",  # use center of legend as anchor
                                         x = 0.5)) 
    
    

      fig3 <- plot_ly(type='pie', labels=labels, values=vectr, 
                     textinfo='none',
                     insidetextorientation='center',
                     marker = list(colors = c('red', 'red')),
                     showlegend = F,
                     hoverinfo = 'none')
      fig3 = fig3 %>%
        layout(fig3, annotations=list(text=paste('Atraso de: ',seconds_to_period(seconds2)), "showarrow"=F, font = list(fontsize = 10), colors = list('white')))
      fig3 = fig3 %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                           paper_bgcolor = 'rgba(0, 0, 0, 0)',
                           margin = list(l=20))
      fig3 = fig3 %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                         xanchor = "center",  # use center of legend as anchor
                                         x = 0.5)) 
      if (as.numeric(seconds2)<3600) return(fig2)
      if (as.numeric(seconds2)>=3600) return(fig3)
    
    
  })
  
  output$lolli_table <- renderDataTable({
    
    data1 = render_lolli_table() %>% 
      select(Fecha,Process, Porcentaje_de_Exito, Cantidad_de_Trabajo, Cantidad_de_Sesiones,
             Horario_Calendario, Horario_Real_Inicio, Diferencia_Horaria)
    colnames(data1) = c('Fecha','Proceso','Porcentaje de Exito', 'Cant. de Trabajo', 'Cant. de Sesiones'
                        , 'Horario de Calendario', 'Horario de Inicio', 'Diferencia')
    
    
    
    d1 = datatable( t(data1[as.Date(data1$Fecha) == as.Date(data1$Fecha[1]),]),
               rownames = TRUE,
               filter = "none",
               style = "bootstrap",
               autoHideNavigation = TRUE,
               selection = "none",
               extensions = "Responsive",
               options = list(dom = 't'),
               colnames= c('event','value')
    )
    
    if (is.null(lollipop()[1])) return(d1)
    d2 =  datatable( t(data1[as.Date(data1$Fecha) == as.Date(lollipop()[1]),]),
                             rownames = TRUE,
                             filter = "none",
                             style = "bootstrap",
                             autoHideNavigation = TRUE,
                             selection = "none",
                             extensions = "Responsive",
                             options = list(dom = 't'),
                             colnames= c('event','value'))
    d2
                  
  })
  
  output$performance_ratio = renderPlotly({
    data1 = render_lolli_table()
    data1$ratio = ifelse(data1$Cantidad_de_Sesiones !=0,
                         round(data1$Cantidad_de_Trabajo/data1$Cantidad_de_Sesiones,2),
                         0)
    fig = plot_ly(x = data1$ratio, y=data1$Fecha, fill='tozeroy',type = 'scatter', mode = 'lines',name='Ratio',
                  line=list(shape = 'spline', smoothing = 1.3, width = 2, color = 'lightblue'))
    fig1 = fig %>% add_trace(x=data1$ratio[1],y=data1$Fecha[1], type = 'scatter', mode = 'markers', showlegend=FALSE,
                             marker = list(size = 10, width=1, color = 'lightgreen'))
    fig1 = fig1 %>% layout(yaxis = list(showgrid = FALSE, zeroline = TRUE, showline = TRUE, showticklabels = TRUE,
                                      linecolor = 'black', linewidth = 3,side='top',
                                      domain = c(0, 0.85), tickfont = list(size = 8)),
                         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = FALSE),tickfont = list(size = 8))
    fig1 = fig1 %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                       xanchor = "center",  # use center of legend as anchor
                                       x = 0.5)) 
    
    if (is.null(lollipop()[1])) return(fig1)
    
    fig2 = fig %>% add_trace(x=data1[as.Date(data1$Fecha)==as.Date(lollipop()[1]),]$ratio[1],
                             y=data1[as.Date(data1$Fecha)==as.Date(lollipop()[1]),]$Fecha[1], type = 'scatter', mode = 'markers',
                                     marker = list(size = 10, width=1, color = 'lightgreen'))
    fig2 = fig2 %>% layout(yaxis = list(showgrid = FALSE, zeroline = TRUE, showline = TRUE, showticklabels = TRUE,
                                               linecolor = 'black', linewidth = 3,side='top',
                                               domain = c(0, 0.85), tickfont = list(size = 8)),
                                  xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = FALSE),tickfont = list(size = 8))
    fig2 = fig2 %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                       xanchor = "center",  # use center of legend as anchor
                                       x = 0.5))

    fig2
    
    
  })
  
  
  
  output$ExTotBox <- renderValueBox({
    valueBox(
      VB_style( paste0( '',format(sum(robotPlotlyPlot11()$Sesion_OK)+sum(robotPlotlyPlot12()$Recurso_No_Disp)
                                  ,big.mark=','), " "), "font-size: 65%;"  ),
      "Numero de Sesiones", 
      icon = icon('stats', lib = 'glyphicon'), # icon("sign-in"),
      color = "aqua"
    )
  })
  
  ###
  output$ImTotBox <- renderValueBox({
    valueBox(
      VB_style( paste0( '',format(dim(renderPlotlyPlot3())[1],big.mark=','), " "), "font-size: 65%;"  ),
      "Cantidad de Trabajo", 
      icon = icon('stats', lib = 'glyphicon'), # icon("sign-in"),
      color = "aqua"
    )
  })
  
  ###
  output$BlTotBox <- renderValueBox({
    valueBox(
      VB_style( paste0('', format(round((sum(robotPlotlyPlot11()$Sesion_OK)/(sum(robotPlotlyPlot11()$Sesion_OK)+sum(robotPlotlyPlot12()$Recurso_No_Disp))),2)*100,big.mark=','), "%"),"font-size: 65%;"  ),
      "Total FTE", 
      icon = icon("balance-scale"),
      color = ifelse( round((sum(robotPlotlyPlot11()$Sesion_OK)/(sum(robotPlotlyPlot11()$Sesion_OK)+sum(robotPlotlyPlot12()$Recurso_No_Disp))),2)*100>60, 'green', 'red' )
    )
  })
  
  output$TotCost = renderValueBox({
    valueBox(
      VB_style( paste0('', format(round((sum(robotPlotlyPlot11()$Sesion_OK)/(sum(robotPlotlyPlot11()$Sesion_OK)+sum(robotPlotlyPlot12()$Recurso_No_Disp))),2)*100,big.mark=','), "%"),"font-size: 65%;"  ),
      "Total Costo: $", 
      icon = icon("balance-scale"),
      color = ifelse( round((sum(robotPlotlyPlot11()$Sesion_OK)/(sum(robotPlotlyPlot11()$Sesion_OK)+sum(robotPlotlyPlot12()$Recurso_No_Disp))),2)*100>60, 'green', 'red' )
      
      
    )
  })
  
  
  output$plotlyPlot1 = renderPlotly({
    
    fig = plot_ly()
    fig = fig %>% 
      add_trace(robotPlotlyPlot11(), y = robotPlotlyPlot11()$Sesion_OK,
                x = robotPlotlyPlot11()$day, type = 'bar', 
                name = 'Sesiones OK')
    fig = fig %>% add_trace(robotPlotlyPlot12(),y = robotPlotlyPlot12()$Recurso_No_Disp,
                            x =robotPlotlyPlot12()$day, type = 'bar',
                            name = 'Recurso no Disp')
    fig = fig %>% layout(yaxis = list(title = 'Cantidad de Sesiones'), barmode = 'stack')
    fig = fig %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                         paper_bgcolor = "rgba(0, 0, 0, 0)")
    fig = fig %>% layout( xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
    fig
    
  })
  

  
  output$plotlyPlot3 = renderPlotly({
    fig = plot_ly()
    aux = renderPlotlyPlot3() %>% select(Date,SecondsWorked) %>%   
      group_by(Date) %>%
      summarize(SecondsWorked = sum(SecondsWorked))
    
    
    fig = fig %>% add_trace(x = aux$Date, y = round((aux$SecondsWorked)/60,2), fill='tozeroy',
                            type = 'scatter', mode = 'lines+markers', name='RPA0003', 
                            line=list(shape = 'spline', smoothing = 1.3, width = 3, color = 'blue'),
                            marker = list(size=10, symbol = 'circle')
    )
    fig = fig %>% layout(yaxis = list(title = 'Minutos Trabajados'))
    fig = fig %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                         paper_bgcolor = "rgba(0, 0, 0, 0)")
    fig = fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE),
                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
    
    fig
    
  })
  
  output$plotlyPlot4 = renderPlotly({
    fig = plot_ly()
    for(type in as.vector(unique(renderPlotlyPlot4()$excep))){
      aux = as.data.frame(table(renderPlotlyPlot4()[renderPlotlyPlot4()$excep==type,]$Date))
      fig = fig %>% add_trace(
        x=aux$Var1, y = aux$Freq, color = type, type = 'bar')
    }
    fig = fig %>% layout(yaxis = list(title = 'Cantidad de Excepciones'))
    fig = fig %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                         paper_bgcolor = "rgba(0, 0, 0, 0)")
    fig = fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
    fig = fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = TRUE, showline = TRUE, showticklabels = TRUE,
                                      linecolor = 'black', linewidth = 3,side='top',
                                      domain = c(0, 0.85), tickfont = list(size = 8)))
    fig = fig %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                       xanchor = "center",  # use center of legend as anchor
                                       x = 0.5)) 
    fig = fig %>%  layout(barmode = 'stack')

    
    
    fig
  })
  
  # Cambiar 
  
  output$plotlyPlot5 = renderPlotly({
    a = renderPlotlyPlot5()
    
    fig = plot_ly()
    fig = fig %>%  add_trace(a, x = a[a$Porcentaje_de_Exito >= a$try,]$Fecha,
                             y = a[a$Porcentaje_de_Exito >= a$try,]$Porcentaje_de_Exito,
                             type = 'bar', name = 'Sobre lo esperado')
    fig = fig %>%  add_trace(a, x = a[a$Porcentaje_de_Exito < a$try & a$Porcentaje_de_Exito >40,]$Fecha, 
                             y =a[a$Porcentaje_de_Exito < a$try & a$Porcentaje_de_Exito >40,]$Porcentaje_de_Exito,
                             type = 'bar', name = 'Bajo lo esperado')
    fig = fig %>%  add_trace(a, x = a[a$Porcentaje_de_Exito <= 40,]$Fecha, 
                             y = a[a$Porcentaje_de_Exito <= 40,]$Porcentaje_de_Exito,
                             type = 'bar', name = 'Critico')
    fig = fig %>%  add_trace(a, x = a$Fecha, y = a$try, 
                             type = 'scatter', mode = 'lines+markers',
                             name = 'Valor Esperado')
    fig = fig %>% layout(yaxis = list(title = 'Cantidad de Sesiones OK!'))
    fig = fig %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                         paper_bgcolor = "rgba(0, 0, 0, 0)")
    
    fig = fig %>% layout( xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
    fig = fig %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                       xanchor = "center",  # use center of legend as anchor
                                       x = 0.5))   
    
    fig
  })
  
  
  
  #### TABLAS - OJO AQUÃÂ­, ESTO ES LARGUISIMO
  
  output$tabla_x_dia = renderDataTable({
    datatable( resumen_x_dia,
               rownames = F,
               extensions = 'Buttons',
               options = list(dom = 'Bfltp', 
                              scrollX = TRUE,
                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                              pageLength = 24,
                              lengthMenu = list(c(24, 48,  96, 192, -1), list('24','48','96', '192', 'all')),
                              searchHighlight = TRUE,
                              search = list(regex = TRUE, caseInsensitive = FALSE ) ),
               colnames= c('','Porcentaje de Exito', ' Cantidad de Sesiones', 'Cantidad de Trabajo',
                           'Problemas VDI / Infraestructura', 'Excepciones Logicas de Trabajo')) %>% 
      
      formatStyle(
        'Porcentaje_de_Exito',
        backgroundColor = styleEqual(
          'Porcentaje de Exito'  , 'lightblue')) %>% 
      formatStyle(
        'Cantidad_de_Trabajo',
        color = styleInterval('Cantidad de Trabajo', c('normal', 'bold')),
        backgroundColor = styleInterval('Cantidad de Trabajo', c('normal', 'gray')))%>%
      formatStyle(
        'Cantidad_de_Sesiones',
        backgroundColor = styleEqual(
          'Cantidad de Sesiones'  , 'lightblue'))%>%
      formatStyle(
        'Cantidad_de_Trabajo',
        backgroundColor = styleEqual(
          'Cantidad de Trabajo'  , 'lightblue'))%>%
      formatStyle(
        'Problemas_VDI_INFRAESTRUCTURA',
        backgroundColor = styleEqual(
          'Problemas VDI/Infraestructura'  , 'lightblue'))%>%
      formatStyle(
        'Excepciones_de_Logica_de_Trabajo',
        backgroundColor = styleEqual(
          'Excepciones de Logica de Trabajo'  , 'lightblue'))
    
    
    
  })
  
  output$sessions_table <- renderDataTable({
    datatable( data_sessions,
               rownames = FALSE,
               filter = "top",
               style = "bootstrap",
               autoHideNavigation = TRUE,
               selection = "none",
               extensions = "Responsive",
               options = list(searchHighlight = TRUE),
               colnames= c('ID de Sesion', 'ID de Proceso', 'Recurso', 'Status',
                           'Mensaje', 'Fecha', 'Hora')
    )
    
  })
  
  output$raw_trabajo_table <- renderDataTable({
    data_trabajo %>% 
      filter(grepl('security.cl', Resources)) %>% 
      datatable(
        rownames = FALSE,
        filter = "top",
        style = "bootstrap",
        autoHideNavigation = TRUE,
        selection = "none",
        extensions = "Responsive",
        options = list(searchHighlight = TRUE),
        colnames = c('Item_ID', 'Process_ID', 'Resources', 'Completed_DateTime',
                     'Exception_DateTime', 'Exception_Reason', 'Worktime',
                     'Date', 'Hour')
        
      )
    
  })
  
  output$double_axis = renderPlotly({
    
     fig = plot_ly(type = "scatter", mode = "lines") %>%
      add_trace(y = robotPlotlyPlot11()$Sesion_OK,
                x = robotPlotlyPlot11()$day, name = 'Sesiones OK', yaxis='y' )%>%
      add_trace(y = robotPlotlyPlot12()$Recurso_No_Disp,
                x =robotPlotlyPlot12()$day, name = "Recursos no Disp", yaxis = "y2") %>%
      layout(
             yaxis = list(title = "Cantidad de Sesiones Ok"),
             yaxis2 = list(title = "Cant. Recursos no Disponibles",
                           overlaying = "y",
                           side = "right"
             )
      )
     
     fig = fig %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                                           xanchor = "center",  # use center of legend as anchor
                                                           x = 0.4))
     fig = fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = TRUE, showline = TRUE, showticklabels = TRUE,
                                       linecolor = 'black', linewidth = 3,side='top',
                                       domain = c(0, 0.85), tickfont = list(size = 8)),
                          yaxis = list(zeroline = FALSE, showline = TRUE, showticklabels = TRUE, showgrid = FALSE),
                          
                          yaxis2 = list(zeroline = FALSE, showline = TRUE, showticklabels = TRUE, showgrid = FALSE))
     fig
  })
  
  output$performance_double_axis1 = renderPlotly({

    
    fig = plot_ly()
    for (excp in unique(render_double_axis()$Exception_message)){
      aux = as.data.frame(table(render_double_axis()[render_double_axis()$Exception_message==excp,]$Date))
      
      fig = fig %>% add_trace(aux, y=aux$Var1, x = aux$Freq, name = paste(excp),
                              type ='bar')
      fig = fig %>%  layout(barmode = 'stack')
    }
    fig = fig %>% layout(yaxis = list(showgrid = FALSE, zeroline = TRUE, showline = TRUE, showticklabels = TRUE,
                                      linecolor = 'black', linewidth = 3,side='top',
                                      domain = c(0, 0.85), tickfont = list(size = 8)),
                         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = FALSE),tickfont = list(size = 8))
    fig = fig %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                       xanchor = "center",  # use center of legend as anchor
                                       x = 0.5)) 
    
    
    
    
  })
  
  
  output$performance_double_axis2 = renderPlotly({
    fig = plot_ly()
    aux = as.data.frame(table(robotPlotlyPlot21()$Exception_message))
    c = cbind(aux,prop.table(aux$Freq)*100)
    colnames(c) = c('Msges','Cantidad','Porcentaje')
    
    fig = c %>% plot_ly(labels = ~Msges, values = ~Porcentaje)
    fig = fig %>% add_pie(hole = 0.6)
    fig = fig %>% layout( xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig = fig %>%
      layout(fig, annotations=list(text=paste('Total:\n',sum(c$Cantidad)), "showarrow"=F))
    fig = fig %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                         paper_bgcolor = 'rgba(0, 0, 0, 0)',
                         margin = list(l=20))
    fig = fig %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                       xanchor = "center",  # use center of legend as anchor
                                       x = 0.5)) 
    
    fig

  })
  
  output$work_double_axis = renderPlotly({
    data <- renderPlotlyPlot5() 

    fig1 = plot_ly()
    fig1 = fig1 %>% add_trace(
                              x= data$Fecha,
                              y = data$suma,
                              name = paste('Excepciones de Trabajo'),type = 'scatter',
                              mode ='lines+markers', yaxis = 'y2')

    
    fig1 = fig1 %>% add_trace(x = data$Fecha, y = data$Cantidad_de_Trabajo, name = 'Cantidad de Trabajo',
                              type = 'scatter', mode = 'lines+markers', yaxis = 'y')
    fig1 = fig1 %>% layout(
      yaxis = list(title = "Trabajos realizados"),
      yaxis2 = list(title = "Excepciones de Trabajo",
                    overlaying = "y",
                    side = "right"
      )
    )
    
    fig1 = fig1 %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                       xanchor = "center",  # use center of legend as anchor
                                       x = 0.4))
    fig1 = fig1 %>% layout(xaxis = list(showgrid = FALSE, zeroline = TRUE, showline = TRUE, showticklabels = TRUE,
                                      linecolor = 'black', linewidth = 3,side='top',
                                      domain = c(0, 0.85), font = list(size = 8)),
                         yaxis = list(zeroline = FALSE, showline = TRUE, showticklabels = TRUE, showgrid = FALSE),
                         
                         yaxis2 = list(zeroline = FALSE, showline = TRUE, showticklabels = TRUE, showgrid = FALSE))
    fig1

  })
  
  
})
