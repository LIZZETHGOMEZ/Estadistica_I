    # ##########################################################################
    #                     Viernes 21 de Mayo de 2021
    #                          Ayudantia
    #                     Simulaciones de V.A.
    # ##########################################################################
    
    # ==================================================================================
    # Suma de Variables Aleatorias Pisson:
    # Generamos una muestra aleatoria de una Poisson de parámetros lambda = 3 y 11
    
    # 100 simulaciones con lambda =3
    poi3 <- rpois(10000,3) 
    
    # Vemos la distribucion:
    hist(poi3)
    
    # Ahora con lambda = 11
    poi11 <-rpois(10000, 11)
    hist(poi3)
    
    
    # Se va aumentando el numero de simulaciones y vemos como empieza a comportarse como esperariamos, cercano a lambda
    
    
    # Veamos que la convulcion de la distribucion, es decir la suma de ambas, sera la mismadistribucion con lambas sumados
    # Poi3(3) + poi11(11) = Poi14 = (14)
    poi14 <- rpois(10000, 14)
    hist(poi14, col = 'red')
    hist(poi3 + poi11, col = 'green')
    
    hist(poi3 + poi11, col = 'green', add = T) # el add = T agrega la grafica a la ultima que se corrio
    
    #================================================================================================
    # LEY DE LOS GRANDE NUMEROS
    # LEY DE LOS GRANDES NUMEROS sn/n --> E(X) sn = X1 + x2 + ... + Xn
    
    # TEOREMA CENTRAL DEL LIMITE 
    # Si tienes X1, X2, ... , Xn V.A.
    # [sn - nE(X1)]/sqrt(nvar(X1)) ----> Distribucion Normal
    
    # Exponencial
    n <- 10000
    
    # Rate = lambda
    exp1 <- rexp(n,5)
    hist(exp1)    
    
    sum(exp1)/n
    # Veamos que la ley de los grandes numeros nos dice que en promedio se converge a la esperanza
    # Aqui al simular con un numero grande cada vez se acerca mas al valor de la exponencial e = 2.71828
    
    
    
    # Aproximacion de la Binomial a la Poisson
    # Bin(n,p); Poi(lambda), n grande, p relativamente
    
    n <-10000
    p <- 0.5
    bin <- rbinom(10000,n,p)   
    hist(bin, col = "Red")    
    
    pois <- rpois(10000, n*p)
    hist(pois, col = "Green", add = T)
    
    # Nota, entre mas centrada este P, mejor sera la simulacion, cambiar  P de 0.3 a 0.5
    
    