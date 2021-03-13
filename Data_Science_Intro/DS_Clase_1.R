    
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
    
    
    
    # 12 de Marzo 2021 Continuación 
    # Quitamos las variables que tienen más del 20% de los missing values
    
    # Revisar codigo....
    # 
    
    
    table(df$state)
    
    #===================================================================
    #  GRAFICOS
    
    
    # VARIABLES CATEGÓRICAS
    
    # Grafico de Barras
    #-----------------------------------------------
    # Variable churn: nos dice si dejaron la empresa o no
    # False = Siguen en la empresa
    # TRUE = Dejaron la empresa
    # Veamos la porporción de clientes que se mantienen con la empresa
    # Guardamos la tala de frecuencias:
    churn_table <- table(df$churn)
    churn_table    
    barplot(churn_table,main = "Proporción de clientes que siguen en la empresa",
            xlab = "Canceló su servicio?", ylab = "Frecuencia", 
            col = "orange", border = "blue")   
    # Vamos a cuadricular el canva:
    grid(10)
    
    # Coloreamos por respuesta:
    barplot(churn_table,main = "Proporción de clientes que siguen en la empresa",
            xlab = "Canceló su servicio?", ylab = "Frecuencia", 
            col = c("indianred1","cornflowerblue")) 
    # En la leyenda(Posición donde se quiera, name, fill( le pone los colores para las etiquetas))
    legend(x = "topright", legend = c("False", "True"), fill =  c("indianred1","cornflowerblue"))
    
    
    #-------------------------------------------------
    # Variable: area.code
    table(df$area.code)
    churn_area.code <- table(df$area.code, df$churn)
    churn_area.code #Hay 3 areas ( 408, 415, 510)
    # Veamos la porporción de cada area:
    barplot(churn_area.code,main = "Churn vs area.code",
            xlab = "Canceló su servicio?", ylab = "Frecuencia", 
            col = c("indianred1","cornflowerblue", "green")) 
    
    # Notemos que la gráfica anterior está horrible no tiene ni etiquetas de
    # que sgnifican los colores, entonces vamos a agregarlos
    
    grid(10)
    legend(x="topright", legend = rownames(churn_area.code),
           fill =c("indianred1","cornflowerblue", "green"), title = "Area Code")
    
    
    #============================================================================
    # GRÁFICOS CON GGPLOT
    # ggplot(base, ejes) + tipo de gráfico() +
    library(ggplot2)
    ggplot(data = df, aes(x = churn))+
        geom_bar(color = "red", fill = "orange") +
        xlab("¿Canceló su servicio?") +
        ylab("Frecuencia")+
        ggtitle("Gráfico de Barras")

    # Ahora colores por respuesta:
    # Aquí ponemos a las categóricas como factor y así jalará cada color por factor
    ggplot(data = df, aes(x = churn, fill = as.factor(churn)))+
        geom_bar() +
        xlab("¿Canceló su servicio?") +
        ylab("Frecuencia")+
        ggtitle("Gráfico de Barras") +
        labs(fill = "")
    
    
    # Ahora con area.code
    # Lo cual me dividirá por colores como factores de acuerdo al tipod de area.code
    ggplot(data = df, aes(x = churn, fill = as.factor(area.code)))+
        geom_bar() +
        xlab("¿Canceló su servicio?") +
        ylab("Frecuencia")+
        ggtitle("Gráfico de Barras") +
        labs(fill = "")
    
    
