    # ############################################################################
    #                       Viernes 04 de Junio 2021 
    #                       Ayudantia con Sergio
    #                    Enfoque hacia la Ciencia de Datos
    # #############################################################################
    

    datos <- read.csv("practica.csv", header = T)
    head(datos)
    summary(datos)    
    
    plot(datos) # No es de gran utilidad si tenemos muchas variables.
    plot(datos$Lav, datos$Ref, pch = 16, #pch indica la forma del punto
         main = "Grafico de dispesión Lavs vs Ref",
         xlab = "% de viviendas con lavadora",
         ylab = "% de viviendas con refri",
         col = "pink")
    grid(10)
     
    # Obtenemos la correlacion de las variables
    cor(datos$Lav, datos$Ref)
    
    # Grafico de una sola variable
    plot(datos$Lav,pch = 16, main = "Grafico de dispersion de lavs",
         xlab = " porcentaje de viviendas con lavadora",
         col = "blue")

    
    # Visaulizar los solo los datos de Aguascalientes
    datos[datos$ENT == 1,]    
    plot(datos[datos$ENT == 1,]$Lav, datos[datos$ENT == 1,]$Ref, pch = 16,
         main = "Grafico de dispersion Lav vs Ref",
         xlab = " propiedades con lavadoras",
         ylab = "Propiedades con refrigerador",
         col = "green", xlim = c(0,1), ylim = c(0,1))
    
    # Agregamos más entidades al grafico
    points(datos[datos$ENT == 2,]$Lav, datos[datos$ENT == 2,]$Ref, pch = 16, col = "red")
    
    points(datos[datos$ENT == 3,]$Lav, datos[datos$ENT == 3,]$Ref, pch = 16, col = "pink")

    
    # Le metemos todos los estados
    for (i in 2:32) {
        points(datos[datos$ENT == i,]$Lav,datos[datos$ENT == i,]$Ref,pch = 16, col = i)
    }  
    
    
    # Lo mismo usando ggplot
    library(ggplot2)
    
    ggplot(data = datos, aes(x = Lav, y = Ref)) +
        geom_point(color = 'royalblue2') +
        xlab('% de viviendas con lavadora') +
        ylab('% de viviendas con refrigerador') + 
        ggtitle("Grafico de dispersión")
    
    
    ggplot(data = datos, aes(x = Lav, y = Ref)) +
        geom_point(aes(color = as.factor(Entidad)), size = 4, shape = 20, alpha = 0.5)+
        xlab('% de viviendas con lavadora') +
        ylab('% de viviendas con refrigerador') + 
        ggtitle("Grafico de dispersión")

    
    # BOX PLOT
    boxplot(datos$Lav, main = "Box plot de Lav", border = "palegreen4", col = "palegreen")
    # Agregamos un lineas de referencia
    abline(a = quantile(datos$Lav, probs = 0.25), b= 0, col = "blue", lwd = 1)
    abline(a = quantile(datos$Lav, probs = 0.5), b= 0, col = "red", lwd = 1)
    abline(a = quantile(datos$Lav, probs = 0.75), b= 0, col = "blue", lwd = 1)
    
    
    # Varios boxplots en el mismo canvas
    boxplot(datos[,c(6:12)], main ="Box plot de al gunas variables",
            col = c("palegreen","red", "cyan","pink"))  
    
    
    # Con ggplot
    # Creamos un grafico de todas las entidades con lavadoras
    ggplot(data = datos, aes(x = as.factor(ENT), y = Lav)) +
        geom_boxplot(aes(color = as.factor(ENT)), alpha = 0.5) +
        xlab("porcentaje de viviendas con lavadora") +
        ylab("porcentaje de viviendas")

    
    # CORRELOGRAMA
    cor(datos[,6:16])
    
    install.packages("corrplot")
    library(corrplot)
    
    Matcor <- cor(datos[,6:16])
    corrplot(Matcor)
    
    # Con ggplot
    library(ggcorrplot)
    
    
    