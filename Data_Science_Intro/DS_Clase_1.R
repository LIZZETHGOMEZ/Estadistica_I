    
    # Martes 09 de Marzo 2021 (Ayudantía DC)
    setwd("C:/Users/GOMEZ/Documents/Ciencias/Inferencia/Intro_DS")
    df <- read.csv("telecom.csv")
    
    dim(df) #Tamaño de la tabla #filas, columnas
    colnames(df) #Nombre de las columnas
    categoricas <- c('state', 'area.code', 'internacional.plan', 'voice.mail', 'churn')
    continuas <- df[colnames(df) != categoricas]
    
    sum(duplicated(df))
    
    # Llenar espacios vacíos con NAs
    for(i in colnames(df)){
        df[i][df[i] == ""] = NA
    }


    #Contar la porporción de NAs por columnas
    completitud = data.frame() #Creamos un nuebo df
    for (i in 1:length(colnames(df))) {
        #Para cada renglón vamos a poner el nombre de la columna y el porcentaje 
        #de datos nulos
        completitud[i,'variable'] = colnames(df)[i] #renglón i, columna variable
        completitud[i, 'completitud'] = (sum(is.na(df[,i]))/dim(df)[1])*100
    }
    
    
    
    
    
    
    
