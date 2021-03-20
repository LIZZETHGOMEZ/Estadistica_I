    
# Tarea 1 de Inferencia 2021
# Lizzeth Gomez Rodriguez
#
    library("readxl")
    library("dplyr")
    library("psych")
    library("ggplot2")
    
    #--------------------------------------------
    # 1. Leer los datos
    setwd("C:/Users/GOMEZ/Documents/Ciencias/Inferencia/Estadistica_I")
    df <- readxl::read_excel("BASE.xlsx")
    
    #--------------------------------------------------
    # 2. TABULADOS DE VARIABES CATEGORICAS Y GRAFICOS
    head(df)
    
    # Tipo de Inmueble
    table_A <- table(df$Tipo_de_Inmueble)
    table_A
    barplot(table_A)
    
    # Operaci?n
    table_B <- table(df$Operacion)
    table_B
    barplot(table_B)
    
    # Provincia
    table_C <- table(df$Provincia)
    table_C
    barplot(table_C)
    
    # Tabulado extra de todas las categoricas
    table <- table(df$Tipo_de_Inmueble,df$Operacion, df$Provincia)
    ftable(table)
    
    # Pregunta, como podemo visualizar las tres figuras en una sola impresion?
    # Digamos en el mismo Canva
    
    #----------------------------------------------------------------------
    # 3.GRAFICA POR PROVINCIA
    pie(table_C,radius = 1)
    
    ggplot2::ggplot(df, aes(x = "", fill= Provincia))+
        geom_bar(width = 1)+
        coord_polar("y")+
        labs(title = "Grafico Pie de Provincias")+
        theme_void()
    #La neta no se como meter los porcentajes aqui
    
    
    #----------------------------------------------------------
    # 4.MEDIDAS DE TENDENCIA CENTRAL
    summary(df$Superficie) #Media y mediana
    
    # Superficie
    # Percentiles 10%, 20% 50% 75%
    quantile(df$Superficie)
    quantile(df$Superficie, 0.9) # percentil 90%
    
    # Precio de Venta
    # Percentiles 10%, 20% 50% 75%
    quantile(df$Precio_Venta)
    quantile(df$Precio_Venta, 0.9) # percentil 90%
    
    #--------------------------------------------------------------
    # 5.MEDIDAS DE DISPERSION
    # Superficie
    var(df$Superficie) #Varianza
    sd(df$Superficie)  #Desviacion Estandar
    IQR(df$Superficie) #Rango Intercuartil
    
    #Precio de Venta
    var(df$Precio_Venta) #Varianza
    sd(df$Precio_Venta)  #Desviacion Estandar
    IQR(df$Precio_Venta) #Rango Intercuartil
    
    #-----------------------------------------------------------
    # 6.COEFICIENTE DE ASIMETR?A
    # Superficie
    psych::skew(df$Superficie)
    # Como el CA < 0 ent. est? sesgada a la derecha.
    # Sin embargo se observa que el coeficiente est? muy cercano a cero.
    
    # Precio de Venta
    psych::skew(df$Precio_Venta)
    # Como CA > 0 ent. el sesgo es a la izquierda 
    
    #--------------------------------------------------------------
    # 7.COEFICIENTE DE KURTOSIS 
    # Superficie
    psych::kurtosi(df$Superficie)
    #Como la kurtosis resulto menor a cero, entonces la distribucion es platocurtica
    
    # Precio de venta
    psych::kurtosi(df$Precio_Venta)
    # Como CA < 0 ent. la distribuci?n es platocurtica 
    
    
    

    
    
    
    