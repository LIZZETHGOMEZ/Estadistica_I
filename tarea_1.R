    
# Tarea 1 de Inferencia 2021
# Lizzeth Gómez Rodríguez

    #--------------------------------------------
    # 1. Leer los datos
    setwd("C:/Users/GOMEZ/Documents/Ciencias/Inferencia")
    library("readxl")
    library("dplyr")
    library("psych")
    library("ggplot2")
    df <- readxl::read_excel("BASE.xlsx")
    
    #--------------------------------------------------
    # 2. TABULADOS DE VARIABES CATEGÓRICAS Y GRÁFICOS
    head(df)
    
    # Tipo de Inmueble
    table_A <- table(df$Tipo_de_Inmueble)
    table_A
    barplot(table_A)
    
    # Operación
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
    
    # Pregunta, cómo podemo visualizar las tres figuras en una sola impresión?
    # Digamos en el mismo Canva
    
    #----------------------------------------------------------------------
    # 3.GRÁFICA POR PROVINCIA
    pie(table_C,radius = 1)
    
    ggplot2::ggplot(df, aes(x = "", fill= Provincia))+
        geom_bar(width = 1)+
        coord_polar("y")+
        labs(title = "Gráfico Pie de Provincias")+
        theme_void()
    #La neta no sé cómo meter los porcentajes aquí
    
    
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
    # 5.MEDIDAS DE DISPERSIÓN
    # Superficie
    var(df$Superficie) #Varianza
    sd(df$Superficie)  #Desviación Estándar
    IQR(df$Superficie) #Rango Intercuartil
    
    #Precio de Venta
    var(df$Precio_Venta) #Varianza
    sd(df$Precio_Venta)  #Desviación Estándar
    IQR(df$Precio_Venta) #Rango Intercuartil
    
    #-----------------------------------------------------------
    # 6.COEFICIENTE DE ASIMETRÍA
    # Superficie
    psych::skew(df$Superficie)
    # Como el CA < 0 ent. está sesgada a la derecha.
    # Sin embargo se observa que el coeficiente está muy cercano a cero.
    
    # Precio de Venta
    psych::skew(df$Precio_Venta)
    # Como CA > 0 ent. el sesgo es a la izquierda 
    
    #--------------------------------------------------------------
    # 7.COEFICIENTE DE KURTOSIS 
    # Superficie
    psych::kurtosi(df$Superficie)
    #Como la kurtosis resulto menor a cero, entonces la distribución es platocurtica
    
    # Precio de venta
    psych::kurtosi(df$Precio_Venta)
    # Como CA < 0 ent. la distribución es platocurtica 
    
    
    

    
    
    
    