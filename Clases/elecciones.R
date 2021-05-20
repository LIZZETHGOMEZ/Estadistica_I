    # ##########################################################################
    #                     Lunes 17 de Mayo de 2021
    #                 Nocion de lo que es la Inferencia
    #               Datos de las eleciones presidenciales
    # ##########################################################################
    
    
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
    
    # Sacamos los votos reales, es decir quitando los votos nulos
    BASE_PRESIDENTE[ , TOTAL_FINAL:= TOTAL_VOTOS - NUM_VOTOS_NULOS]
    BASE_PRESIDENTE    
    
    # Sacamos los votos totales que no fueron nulos
    sum(BASE_PRESIDENTE$TOTAL_FINAL)
    
    # Porcentaje de votantes a favor de AMLO:
    (sum(BASE_PRESIDENTE$AMLO)/sum(BASE_PRESIDENTE$TOTAL_FINAL)) *100 
    
    #===========================================================================
    # Para inferir lo anterior se requiere de tomar una muestra de las casillas
    # Se realiza en este caso y para fines didacticos una muestra aleatoria
    # La funcion se llena con el tamaño de la población, el tamaño de la muestra y sin reemplazo
    
    sample.int(length(BASE_PRESIDENTE$AMLO),3, replace = FALSE)
    
    # Ahora sacaremos una muestra aleatoria de tamaño 1, que sera la casilla que usaremos
    BASE_PRESIDENTE[sample.int(length(BASE_PRESIDENTE$AMLO),1, replace = FALSE),]
    
    #Como nos interesa unicamente los votos de AMLO:
    BASE_PRESIDENTE[sample.int(length(BASE_PRESIDENTE$AMLO),1, replace = FALSE),]$AMLO
    
    
    
    # ###########################################################################
    #  19 de Mayo de 2021
    #  Continuacion de nocion de Inferencia
    
    # Simulacion de 10 obs de una Bernoulli con parametro 0.3
    rbinom(10,1,0.3) #El modelo siempre arroja 0 y 1
    
    #Entonces en promedio cuanto arroja si repetimos el experimento? esa es la esperanza
    mean(rbinom(100,1,0.3)) # veamos que a mayor repeticion del exp. nos dara 0.3
  
    
    # Continuacion con el ejemplo de las elecciones
    # Tenemos la muestra de tamaño 1, y ya tenemos la casilla seleccionada y los votos para AMLO de esa casilla
    # Ya se hizo el muestreo y por lo cual esa ya no es una V.A
    # Ahora la estimación es multiplicarlo por la N que en este caso sera el total de casillas
    total_casillas <- length(BASE_PRESIDENTE$AMLO)

    BASE_PRESIDENTE[sample.int(length(BASE_PRESIDENTE$AMLO),1, replace = FALSE),]$AMLO*total_casillas
    sum(BASE_PRESIDENTE$AMLO) #Notemos que es muy parecido al numero anterior
    
    
    # Repetimos muchas veces el experimento, que es lo que dice la teoria
    ESTIMACIONES <- NULL
    
    for(i in 1:100000){
        ESTIMACIONES[i] <- BASE_PRESIDENTE[sample.int(length(BASE_PRESIDENTE$AMLO),1, replace = FALSE),]$AMLO*total_casillas
    }
    
    #Comprobamos que en pomedio nos da lo que buscamos, que es el total de votos de AMLO
    mean(ESTIMACIONES)
    sum(BASE_PRESIDENTE$AMLO)
    
    hist(ESTIMACIONES)
    abline( v = sum(BASE_PRESIDENTE$AMLO),col = 2, lwd = 2)
    # Notemos que el estimador está muy cercano a la linea, que es el valor buscado
    # Pero no tiene chiste si la varianza del estimador es muy grande, lo cual signififcaria que
    # no le vamos a pegar al numero buscado.
    # Entonces con varianza pequeña se puede estar mas seguro que le va a pegar a la estimacion correcta.
    # Entonces una buena estimacion por diseño recae en el tipo de muestreo
    # Es deicir encontrar un estimador con la minima varianza posible. 
    
    # Saquemos la varianza, pero de una muestra mas grande
    # Con muestra mas grande tenemos mas informacion y eso reduce variabilidad
    sample <- BASE_PRESIDENTE[sample.int(length(BASE_PRESIDENTE$AMLO),2000, replace = FALSE),]
    
    # Saquemos en promedio los votospor casilla de AMLO
    mean(sample$AMLO)
    
    # Con base en la teoria lo multiplicamos por el total de la pob
       
    
    