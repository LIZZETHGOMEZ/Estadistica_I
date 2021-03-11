    
    # Martes 09 de Marzo 2021 (Ayudantía DC)
    setwd("C:/Users/GOMEZ/Documents/Ciencias/Inferencia/Estadistica_I/Data_Science_Intro")
    df <- read.csv("telecom.csv")
    
    dim(df) #Tamaño de la tabla #filas, columnas
    colnames(df) #Nombre de las columnas
    categoricas <- c('state', 'area.code', 'internacional.plan', 'voice.mail', 'churn')
    continuas <- c("account.length", 'number.vmail.messages', 'total.day.minutes', 
                   "total.day.calls", "total.day.charge", "total.eve.minutes", 
                   "total.eve.calls", "total.eve.charge", "total.night.minutes", 
                   "total.night.calls", "total.night.charge", "total.intl.minutes", 
                   "total.intl.calls", "total.intl.charge", "customer.service.calls")
    
    sum(duplicated(df))
    
    # Llenar espacios vacíos con NAs
    for(i in colnames(df)){
        df[i][df[i] == ""] = NA
    }


    #Contar la proporción de NAs para cada variable
    completitud = data.frame() #Creamos un nuevo df
    for (i in 1:length(colnames(df))) {
        #Por cada variable, la pondremos como renglón en la columna de variable, 
        #y pondremos su porcentaje en la columna de cmpletitud.
        completitud[i,'variable'] = colnames(df)[i] #renglón i, columna variable
        completitud[i, 'completitud'] = (sum(is.na(df[,i]))/dim(df)[1])*100
    }
    
    
    
    
    
    
    
