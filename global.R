#######################
###### FUNCIONES ######
#######################

library(shiny)
library(tidyverse)
library(leaflet.extras)
library(rvest)
library(plotly)
library(lubridate)
library(shinyjs)
library(DT)

## KPI Box styling ##

VB_style <- function(msg = 'Hello', style="font-size: 100%;"){
  tags$p(msg, style = style)
}


## Funcion para escribir sobre la pÃ¡gina de tablas ##
howto_hs_finder <- function(){
  
  tags$h1('How to:')
  
  tags$ol(
    tags$li( "Escibe algo aqui."),
    tags$li( "otra aqui"),
    tags$li( "Mas"),
    tags$li( "Y de ser necesario mas" ),
    tags$li( "Incluso podemos agregar un link "),
    tags$a( "aquÃ­",
            href= "www.primus.ai",
            target = "_blank"),".")
  
}



#####################
#### LECTURA DE #####
####   DATOS    #####
#####################




data_trabajo = read.csv(file = 'data/Work_CustomDataCollectio.csv')
data_sessions = read.csv(file = 'data/Session_CustomDataCollectio.csv')
data_schedule = read.csv(file = 'data/Scheduler_CustomDataCollectio (1).csv')
data_business = read.csv(file = 'data/Business_CustomDataCollectio.csv')

## Creando nombre de columnas

colnames(data_trabajo) = c('Item_ID', 'Process_ID', 'Resources', 'Completed_DateTime',
                           'Exception_DateTime', 'Exception_Reason', 'Worktime',
                           'Date', 'Hour')
colnames(data_sessions) = c('Session_ID', 'Process_ID', 'Resources', 'Status_Session',
                            'Message', 'Date', 'Hour')
colnames(data_schedule) = c('Process_ID','Scheduled Days','Init Time','End Time',
                            'Day Requested Schdule','Hour Requested')
colnames(data_schedule) = c('Process_ID', 'ID', 'Work', 'Time', 'Req_Date', 'Req_Hour')

## PequeÃ±o preprocesamiento

data_trabajo = data_trabajo[-c(1:28),] %>% 
  filter(grepl('security.cl', Resources)) %>% 
  mutate(Worktime = str_sub(Worktime, 3,-1)) %>% 
  mutate(Worktime = hms(Worktime)) %>% 
  mutate(SecondsWorked = period_to_seconds(Worktime)) %>% 
  mutate(Completed_DateTime = str_sub(Completed_DateTime,1,
                                      nchar(Completed_DateTime)-1)) %>% 
  mutate(Exception_DateTime = ymd_hms(Exception_DateTime)) 

data_sessions = data_sessions %>% 
  filter(grepl('security.cl', Resources))

data_sessions = data_sessions %>% 
  mutate(Hour = hms(Hour))

data_schedule = data_schedule %>% 
  mutate(Work = as.character(Work))
data_schedule$Work = paste0(data_schedule$Work, ':00')

data_schedule = data_schedule %>% 
  mutate(Work = hms(as.character(Work)))
  



##########################
####### CREACIÃN DE ######
####### DF/SNIPPETS ######
#######   PARA      ######
####### LLAMAR EN   ######
#######  SERVER.R   ######
##########################

## Usado para filtrar algunas cosas 

process_names = sort(as.character(unique(data_trabajo[,c("Process_ID")])))


## Utilizado principalmente en un grÃ¡fico

data_excep = data_trabajo %>% 
  mutate(excep = case_when(
    grepl('failed to perform step', Exception_Reason, ignore.case = TRUE)~'Falló en un paso',
    grepl('Se excedio tiempo de espera', Exception_Reason, ignore.case = TRUE) ~ 'Tiempo de espera excedido',
    grepl('Automatically set exception', Exception_Reason, ignore.case = TRUE) ~ 'Excepcion Automatica',
    grepl('error irreparable', Exception_Reason, ignore.case = TRUE) ~ 'Error de Proceso',
    grepl('formulario de ingreso', Exception_Reason, ignore.case = TRUE) ~ 'Error Formulario de Ingreso',
    grepl('elemento no encontrado', Exception_Reason, ignore.case = TRUE) ~ 'Elemento no Encontrado',
    grepl('restart application', Exception_Reason, ignore.case = TRUE) ~ 'Restart Applicacion',
    grepl('no se pudo generar el enlace', Exception_Reason, ignore.case = TRUE) ~ 'Error de enlace')) %>% 
  filter(is.na(Exception_DateTime)==FALSE) 

## Creamos una tabla resumen para utilizar replicar en una tabla
## Y tambiÃ©n serÃ¡ usado en otros grÃ¡ficos

resumen_x_dia = data.frame(Fecha=character(),
                           Porcentaje_de_Exito=character(), 
                           Cantidad_de_Sesiones=character(),
                           Cantidad_de_Trabajo = character(),
                           Problemas_VDI_INFRAESTRUCTURA = character(),
                           Excepciones_de_Logica_de_Trabajo =character(),
                           stringsAsFactors=FALSE)

for (date in as.vector(unique(data_sessions$Date))){
  nombre_columnas = c(paste('Fecha:',date),'Porcentaje de Exito', ' Cantidad de Sesiones', 'Cantidad de Trabajo',
                      'Problemas VDI / Infraestructura', 'Excepciones Logicas de Trabajo')
  rob = as.vector(numeric(length(unique(data_trabajo$Process_ID))+1))
  p_exito = as.vector(numeric(length(unique(data_trabajo$Process_ID))+1))
  cant_ses = as.vector(numeric(length(unique(data_trabajo$Process_ID))+1))
  cant_trab = as.vector(numeric(length(unique(data_trabajo$Process_ID))+1))
  p_vdi = as.vector(numeric(length(unique(data_trabajo$Process_ID))+1))
  exc_l = as.vector(numeric(length(unique(data_trabajo$Process_ID))+1))
  
  rob[1] = paste('Fecha:',date)
  p_exito[1] = 'Porcentaje de Exito'
  cant_ses[1] = 'Cantidad de Sesiones'
  cant_trab[1] = 'Cantidad de Trabajo'
  p_vdi[1] = 'Problemas VDI/Infraestructura'
  exc_l[1] = 'Excepciones de Logica de Trabajo'
  i = 1
  for (robot in as.vector(unique(data_trabajo$Process_ID))){
    
    aux_trabajo = data_trabajo %>%
      mutate(Date = as.character(Date)) %>% 
      filter(Date == as.character(date)) %>% 
      filter(Process_ID == as.character(robot))
    
    aux_sessions = data_sessions %>% 
      mutate(Date = as.character(Date)) %>% 
      filter(Date == as.character(date)) %>% 
      filter(Process_ID == as.character(robot))
    
    recursos_no_disp = as.numeric(count(aux_sessions[aux_sessions$Status_Session == 'RECURSO NO DISPONIBLE',]))
    sesion_ok = as.numeric(count(aux_sessions[aux_sessions$Status_Session == 'SESION INICIADA OK',]))
    q_sessions = sesion_ok+recursos_no_disp
    problemas_vdi = recursos_no_disp
    q_trabajo = dim(aux_trabajo[is.na(aux_trabajo$Item_ID)==FALSE,])[1]
    excepciones_logicas = dim(aux_trabajo[aux_trabajo$Exception_Reason!='',])[1]
    porcentaje_exito = ifelse(q_sessions!=0, round((sesion_ok/q_sessions)*100,2),0)
    
    rob[i+1] = as.character(robot)
    p_exito[i+1] = as.character(paste(porcentaje_exito, '%'))
    cant_ses[i+1] = as.character(q_sessions)
    cant_trab[i+1] = as.character(q_trabajo)
    p_vdi[i+1] =as.character(problemas_vdi)
    exc_l[i+1] =as.character( excepciones_logicas)
    
    i = i + 1
    
  }
  
  df2 = data.frame(rob, p_exito, cant_ses, cant_trab, p_vdi, exc_l)
  colnames(df2) = colnames(resumen_x_dia)
  resumen_x_dia = rbind(resumen_x_dia,df2)
}


###

summarized_df = data.frame(Fecha=character(),
                           Recurso_NO_Disp = character(),
                           Sesion_OK = character(),
                           Process = character(),
                           Porcentaje_de_Exito=character(), 
                           Cantidad_de_Sesiones=character(),
                           Cantidad_de_Trabajo = character(),
                           Problemas_VDI_INFRAESTRUCTURA = character(),
                           Excepciones_de_Logica_de_Trabajo =character(),
                           stringsAsFactors=FALSE)

for (date in as.vector(unique(data_sessions$Date))){
  rec_nodisp = as.vector(NULL)
  sesionok = as.vector(NULL)
  date_vector = as.vector(NULL)
  rob = as.vector(numeric(length(unique(data_trabajo$Process_ID))))
  p_exito = as.vector(numeric(length(unique(data_trabajo$Process_ID))))
  cant_ses = as.vector(numeric(length(unique(data_trabajo$Process_ID))))
  cant_trab = as.vector(numeric(length(unique(data_trabajo$Process_ID))))
  p_vdi = as.vector(numeric(length(unique(data_trabajo$Process_ID))))
  exc_l = as.vector(numeric(length(unique(data_trabajo$Process_ID))))
  
  i = 1
  for (robot in as.vector(unique(data_trabajo$Process_ID))){
    
    aux_trabajo = data_trabajo %>%
      mutate(Date = as.character(Date)) %>% 
      filter(Date == as.character(date)) %>% 
      filter(Process_ID == as.character(robot))
    
    aux_sessions = data_sessions %>% 
      mutate(Date = as.character(Date)) %>% 
      filter(Date == as.character(date)) %>% 
      filter(Process_ID == as.character(robot))
    
    recursos_no_disp = as.numeric(count(aux_sessions[aux_sessions$Status_Session == 'RECURSO NO DISPONIBLE',]))
    sesion_ok = as.numeric(count(aux_sessions[aux_sessions$Status_Session == 'SESION INICIADA OK',]))
    q_sessions = sesion_ok+recursos_no_disp
    problemas_vdi = recursos_no_disp
    q_trabajo = dim(aux_trabajo[is.na(aux_trabajo$Item_ID)==FALSE,])[1]
    excepciones_logicas = dim(aux_trabajo[aux_trabajo$Exception_Reason!='',])[1]
    porcentaje_exito = ifelse(q_sessions!=0, round((sesion_ok/q_sessions)*100,2),0)
    
    rec_nodisp[i] = recursos_no_disp
    sesionok[i] = sesion_ok
    date_vector[i] = as.character(date)
    rob[i] = as.character(robot)
    p_exito[i] = as.numeric(porcentaje_exito)
    cant_ses[i] = as.numeric(q_sessions)
    cant_trab[i] = as.numeric(q_trabajo)
    p_vdi[i] =as.numeric(problemas_vdi)
    exc_l[i] =as.numeric( excepciones_logicas)
    
    i = i + 1
    
  }
  
  df2 = data.frame(date_vector,rec_nodisp, sesionok ,rob, p_exito, cant_ses, cant_trab, p_vdi, exc_l)
  colnames(df2) = colnames(summarized_df)
  summarized_df = rbind(summarized_df,df2)
}


summarized_df = summarized_df %>% 
  mutate(weekday = as.character(weekdays(ymd(Fecha)))) %>% 
  mutate(month = as.character(month(ymd(Fecha))))
summarized_df[summarized_df$Porcentaje_de_Exito==0, ]$Porcentaje_de_Exito = 100









summarized_df$try = 0
for (dia in as.vector(unique(summarized_df$weekday))){
  for (mes in as.vector(unique(summarized_df$month))){
    for (robot in as.vector(unique(summarized_df$Process))){
      mean = mean(summarized_df[(summarized_df$weekday==dia)&
                                  (summarized_df$month==mes)&
                                  (summarized_df$Process==robot),]$Porcentaje_de_Exito)
      if (dim(summarized_df[(summarized_df$weekday==dia)&
                            (summarized_df$month==mes)&
                            (summarized_df$Process==robot),])[1]!=0){
        summarized_df[(summarized_df$weekday==dia)&
                        (summarized_df$month==mes)&
                        (summarized_df$Process==robot),]$try = mean}
      
      
      
    }
    
  }
}



summarized_df$Porcentaje_de_Trabajo = 0
summarized_df$Porcentaje_de_Trabajo = 
  ifelse(
    (summarized_df$Problemas_VDI_INFRAESTRUCTURA+
       summarized_df$Cantidad_de_Trabajo+
       summarized_df$Excepciones_de_Logica_de_Trabajo)!=0,
    round((summarized_df$Cantidad_de_Trabajo/(summarized_df$Problemas_VDI_INFRAESTRUCTURA+
                                                summarized_df$Cantidad_de_Trabajo+
                                                summarized_df$Excepciones_de_Logica_de_Trabajo))*100,2),0)
summarized_df$Porcentaje_de_Error_Trabajo = 100-summarized_df$Porcentaje_de_Trabajo


summarized_df$Porcentaje_de_Error = 
  ifelse(
    summarized_df$Cantidad_de_Trabajo==0 & summarized_df$Cantidad_de_Trabajo+
      summarized_df$Excepciones_de_Logica_de_Trabajo != 0,
    round(((summarized_df$Cantidad_de_Trabajo+
              summarized_df$Excepciones_de_Logica_de_Trabajo)/(summarized_df$Problemas_VDI_INFRAESTRUCTURA+
                                                                 summarized_df$Cantidad_de_Trabajo+
                                                                 summarized_df$Excepciones_de_Logica_de_Trabajo))*100,2),0)




summarized_df$diff = 0
summarized_df$diff = summarized_df$Porcentaje_de_Exito - summarized_df$try


######

summarized_df2 = data.frame(Fecha=character(),
                           Recurso_NO_Disp = character(),
                           Sesion_OK = character(),
                           Process = character(),
                           Porcentaje_de_Exito=character(), 
                           Cantidad_de_Sesiones=character(),
                           Cantidad_de_Trabajo = character(),
                           Problemas_VDI_INFRAESTRUCTURA = character(),
                           Excepciones_de_Logica_de_Trabajo =character(),
                           Horario_Calendario = character(),
                           Horario_Real_Inicio = character(),
                           Diferencia_Horaria = character(),
                           stringsAsFactors=FALSE)

for (date in as.vector(unique(data_sessions$Date))){
  rec_nodisp = as.vector(NULL)
  sesionok = as.vector(NULL)
  date_vector = as.vector(NULL)
  rob = as.vector(numeric(length(unique(data_trabajo$Process_ID))))
  p_exito = as.vector(numeric(length(unique(data_trabajo$Process_ID))))
  cant_ses = as.vector(numeric(length(unique(data_trabajo$Process_ID))))
  cant_trab = as.vector(numeric(length(unique(data_trabajo$Process_ID))))
  p_vdi = as.vector(numeric(length(unique(data_trabajo$Process_ID))))
  exc_l = as.vector(numeric(length(unique(data_trabajo$Process_ID))))
  horario_calendario_vect = as.vector(NULL)
  horario_real_inicio_vect = as.vector(NULL)
  diferencia_horaria_vect = as.vector(NULL)
  
  i = 1
  for (robot in as.vector(unique(data_trabajo$Process_ID))){
    
    aux_trabajo = data_trabajo %>%
      mutate(Date = as.character(Date)) %>% 
      filter(Date == as.character(date)) %>% 
      filter(Process_ID == as.character(robot))
    
    aux_sessions = data_sessions %>% 
      mutate(Date = as.character(Date)) %>% 
      filter(Date == as.character(date)) %>% 
      filter(Process_ID == as.character(robot))
    
    recursos_no_disp = as.numeric(count(aux_sessions[aux_sessions$Status_Session == 'RECURSO NO DISPONIBLE',]))
    sesion_ok = as.numeric(count(aux_sessions[aux_sessions$Status_Session == 'SESION INICIADA OK',]))
    q_sessions = sesion_ok+recursos_no_disp
    problemas_vdi = recursos_no_disp
    q_trabajo = dim(aux_trabajo[is.na(aux_trabajo$Item_ID)==FALSE,])[1]
    excepciones_logicas = dim(aux_trabajo[aux_trabajo$Exception_Reason!='',])[1]
    porcentaje_exito = ifelse(q_sessions!=0, round((sesion_ok/q_sessions)*100,2),0)
    
    rec_nodisp[i] = recursos_no_disp
    sesionok[i] = sesion_ok
    date_vector[i] = as.character(date)
    rob[i] = as.character(robot)
    p_exito[i] = as.numeric(porcentaje_exito)
    cant_ses[i] = as.numeric(q_sessions)
    cant_trab[i] = as.numeric(q_trabajo)
    p_vdi[i] =as.numeric(problemas_vdi)
    exc_l[i] =as.numeric( excepciones_logicas)
    horario_calendario_vect[i] = as.character(hms(data_schedule[data_schedule$Process_ID==robot,]$Work))
    horario_real_inicio_vect[i] = as.character(hms(aux_sessions[aux_sessions$Status_Session=='SESION INICIADA OK',]$Hour[1]))
    diferencia_horaria_vect[i] = as.character(hms(horario_real_inicio_vect[i])-hms(horario_calendario_vect[i]))
    
    
    i = i + 1
    
  }
  
  df2 = data.frame(date_vector,rec_nodisp, sesionok ,rob, p_exito, cant_ses, 
                   cant_trab, p_vdi, exc_l, horario_calendario_vect,
                   horario_real_inicio_vect, diferencia_horaria_vect)
  colnames(df2) = colnames(summarized_df2)
  summarized_df2 = rbind(summarized_df2,df2)
}


### DFs QUE HAY QUE ELIMINAR






arr = data_sessions %>% 
  filter(Process_ID == 'RPA0010') %>% 
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


#####

daily_summary = summarized_df[, -which(names(summarized_df) == "weekday")]
daily_summary =  daily_summary[, -which(names(daily_summary) == "Process")] 
daily_summary = daily_summary %>% group_by_if(is.numeric %>% Negate) %>%
  summarize_all(sum) %>% 
  mutate(Porcentaje_de_Exito_medio = Porcentaje_de_Exito/3) %>% 
  mutate(Porcentaje_de_Trabajo_medio = Porcentaje_de_Exito/length(unique(data_sessions$Process_ID)))

########

daily_bar_summary = data_sessions %>% 
  
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
      grepl('database', Message, ignore.case = TRUE) ~ 'Otro')) %>% 
  select(Date, Exception_message) %>% 
  mutate(Date = as.character(Date))


#mutate(count = 1) %>% 
#group_by_if(is.numeric %>% Negate) %>%
#summarize_all(sum)

excp_vector = as.vector(NULL)
date_vector = as.vector(NULL)
count_vector = as.vector(NULL)
i=1
for (date in unique(as.vector(daily_bar_summary$Date))){
  
  for (exc in unique(daily_bar_summary$Exception_message)){
    
    if ((exc %in% as.vector(daily_bar_summary[as.Date(daily_bar_summary$Date)==as.Date(date),]$Exception_message))==FALSE){
      date_vector[i] = date
      count_vector[i] = 0
      excp_vector[i] = exc
      
      
    }
    else {
      v = daily_bar_summary[as.Date(daily_bar_summary$Date)==as.Date(date),]
      
      date_vector[i] = date
      count_vector[i] = length(as.vector(v[v$Exception_message==exc,]$Exception_message))
      excp_vector[i] = exc
    }
    
    i = i+1
  }
}

daily_bar_summary2<- data.frame(date_vector, excp_vector,count_vector)
colnames(daily_bar_summary2) = c('Date','Exception_message','count')


