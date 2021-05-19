    # ################################################################
    #               Lunes 17 de Mayo de 2021
    #            Nocion de lo que es la Inferencia
    #         Datos de las eleciones presidenciales
    # ###############################################################
    
    
    setwd("C:/Users/GOMEZ/Documents/Ciencias/Inferencia/Estadistica_I/Clases")
    
    #fread es la funcion de fast read para leer muy rapido bases que son muy pesadas
    BASE_PRESIDENTE <- data.table::fread("BASE_PRESIDENTE.csv") 
    BASE_PRESIDENTE
    
    # Se pide encontrar la votación que AMLO tendra en la eleccion
    
    # Paso 1: Creamos uns nueva columna "AMLO" que tendra los votos totales del candidato
    # debido a que hubo muchas coaliciones, todos estos votos se suman
    
    BASE_PRESIDENTE[ , AMLO:= MORENA + ES + PT + PT_MORENA_ES + PT_MORENA + PT_ES + MORENA_ES]
    
    # Sumamos toda la columna creada
    sum(BASE_PRESIDENTE$AMLO) #Notemos que hay filas con NA por ello arroja NA
    
    # Veamos los NAs
    is.na(BASE_PRESIDENTE$AMLO)
    
    # Traemos a las filas unicamente en donde la funcion anterior es TRUE
    # Es decir veamos si en relidad hay NAs
    BASE_PRESIDENTE[is.na(BASE_PRESIDENTE$AMLO), ]
    
    # Ahora traemos los que no son NA
    BASE_PRESIDENTE[!is.na(BASE_PRESIDENTE$AMLO), ]
    
    # Limpiamos la base al reescribir la base sin los Nas
    BASE_PRESIDENTE <- BASE_PRESIDENTE[!is.na(BASE_PRESIDENTE$AMLO), ]

    # Ahora si sumamos la columna AMLO
    # Es el numero de votos con el que se gano la eleccion
    sum(BASE_PRESIDENTE$AMLO)    
    
    # El dato anterior es el que se quiere inferir, pues es el que no se sabe el dia de la eleccion
    
    
    
    
    
    
    
    
    
    