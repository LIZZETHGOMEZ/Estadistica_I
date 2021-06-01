    

    # ############################################################################
    #                   Introducción a la Inferencia
    #                           Lunes 31 de Mayo
    # ############################################################################
     
     
    
    # Simulacion del Modelo Bernoulli
    # X1,X2, ..., Xn m.a. Bernoulli(theta)
    # E(X) = theta
    # Var(X) = theta*(1-theta)
    
    n <- 100
    theta <- 0.4
    muestra <-rbinom(n, size = 1, theta)
    
    # Supongamos que no conocemos theta
    # Queremos estimar Mu = E(X) = theta = 0.4
    # Sigma_2 = Var(X) = theta*(1-theta) = 0.4*0.6 = 0.24
    
    Mu_estimacion = mean(muestra)
    var_estimacion = var(muestra)
    
    # Como el e•stimador es consistente, las estimaciones tienen que acercarse mas,
    # al repetirlas varias veces
    # Repetimos el codigo anterior con n= 20, n = 100 ...
    # Vemos que cuando n es cada vez mas grande, efectivamente mi estimador es mejor
    # se acerca mas al valor buscado (0.4)

     
    
    # ================================================================================
    # Simulacion del Modelo Poisson 
    # (Es lo mismo, pero ahora con distribucion poisson)
    # X1,X2, ..., Xn m.a. Poisson(theta)
    # E(X) = theta
    # Var(X) = theta*(1-theta)
    
    n <-4000 
    theta <- 0.4
    muestra <-rpois(n, theta)
    
    # S upongamos que no conocemos theta
    # Queremos estimar Mu = E(X) = theta = 0.4
    # Sigma_2 = Var(X) = theta*(1-theta) = 0.4*0.6 = 0.24
    
    # Recordar que en este modelo, la media y la varianza son la misma
    Mu_estimacion = mean(muestra)
    var_estimacion = var(muestra)

    # Notemos que en este modelo es mas lento, se necesito de mayores repeticiones 
    # para acercarse al valor buscado.
    
    
     
    
    
    
    
    
    