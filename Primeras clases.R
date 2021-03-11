
    # 02 de marzo de 2021
    # Clase de Inferencia, Intro a R
    # =========================================================================
    
    # R usa tipado dinámico, que es cuando modificamos las variables asignadas
    # Número de Filas nrow()
    # Número de Columnas ncol()
    # Número de variables length()
    # Conocer la estructura de los datos str()
    # arrja tipo de datos, num de obs etc.
    
    # 04 de Marzo de 2021
    # =========================================================================
    setwd("C:/Users/GOMEZ/Documents/Ciencias/Inferencia")
    library("readxl")
    library("dplyr")
    library("psych")
    base<- read_excel("BASE.xlsx")
    
    
    base <- read.excel()
    base$Tipo_de_Inmueble
    table(base$Tipo_de_Inmueble)
    barplot(table(base$Tipo_de_Inmueble))
    barplot(table(base$Provincia))
    
    median(base$Superficie)
    quantile(base$superficie,0.5)
    var(base$Superficie)
    sd(base$Superficie)
    
    # En box plot, la base del rectángulo es el percentil 0.25, la línea es el 0.05
    # y el techo sería el 0.75
    boxplot(base$Superficie)
    
    
    # Lunes 06 de Marzo 2021
    # Gráfico
    boxplot(base$Superficie, ylim = c(0,300))
    abline(h = quantile(base$Superficie,0.25))
    abline(h = quantile(base$Superficie,0.75), col = 2)
    abline(h = quantile(base$Superficie,0.5), col = 3)
    
    
    psych::skew(base$Superficie)
    
    
    # Calcular manualmente la Simetría
    x <- base$Superficie
    n <- length(x)
    sum((x-mean)^{3})/(n*sd(x)^{3})

    
    # Restamos al vector x el promedio,entrada por entrada
    
    # Gráfico de Histograma
    # Toma el valor mínimo y máximo, lo divide en partes iguales
    # y ubica cuantas observaciones caen en cada intervalo.
    hist(base$Superficie)
    hist(base$Superficie, breaks = 50)
    
    
    # =======================================================================
    # 10 de Marzo 2021
    base<- readxl::read_excel("BASE.xlsx")
    
    hist(base$Superficie, freq = FALSE, breaks = 50)
    # Graficamos un modelo normal atrás de la curva:
    # dnorm me da la densidad del modelo que queremos graficar
    # En este caso es la normal (x,media, varianza)
    curve(dnorm(x,100,10), col = 2, add = TRUE) #Centramos la media en 100 con varianza 10
    
    plot(ecdf(base$Superficie)) #ecdf es la función F(x)
    
    #Simulamos observaciones random del modelo normal
    muestra <- rnorm(100, 0,1) # 100 observaciones, media=0, var = 1
    
    #Histograma de la muestra anterior
    hist(muestra, freq = FALSE)
    curve(dnorm(x,0,1), col = 2, add = TRUE, lwd = 2)
    
    plot(ecdf(muestra))
    curve(pnorm(x,0,1), col = 2, add = TRUE, lwd = 2)
    
    # TEOREMA DE GILVENKO-CANTELI
    #Vamos aproximandonos a la función distribución empírica agrandando la muestra
    muestra <- rnorm(1000, 0,1) # 100 observaciones, media=0, var = 1
    plot(ecdf(muestra)) #Práticamente es la función de distribución empírica
    curve(pnorm(x,0,1), col = 2, add = TRUE, lwd = 2)
    
    
    
    
    