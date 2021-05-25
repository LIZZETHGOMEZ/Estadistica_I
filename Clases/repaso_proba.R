    # #############################################################################
    #                   Lunes 24 de Mayo de 2021
    #                   Repaso de probabilidad
    # ##############################################################################
    
    # Chi-cuadrada Simulacion con un grado de libertad
    curve(dchisq(x, df = 1), from = 0, to = 7)
    
    simulacion <- rnorm(1000,0,1)
    hist(simulacion^{2}, freq = FALSE) # Vemos la distribucion
    
    # Estamos verificando que la distribucion de una normal (0,1) sigue una densidad Chi-cuadrada con un grado de libertad
    hist(simulacion^{2}, freq = FALSE, breaks = 100)
    curve(dchisq(x, df = 1), add =TRUE, col = 2, lwd = 2)
    
    
    # Distribucion empirica de la simulacion ecdf()
    plot(ecdf(simulacion^{2}))
    curve(pchisq(x, df = 1), add= TRUE, col = 2, lwd =2)
    
    
    # -------------------------------------------------------------------------------
    # Teorema: se tiene una normal (0,1) y una Chi-cuadrada independiente:
    # Z ~ N(0,1); W ~ Chi(n) independiente
    # T = Z/sqr(W/n)
    # T ~ t-student(n)
    
    # Hacemos la simulacion
    Z <- rnorm(1000,0,1)
    W <- rchisq(1000,4)
    T <- Z/sqrt(W/4)    
    
    plot(ecdf(T))    
    curve(pt(x, df = 4), add = TRUE, col = 2, lwd = 2)
    
    # Se verifica que una chi-cuadrada con 4 grados de libertad sale una t-student
    
    # ------------------------------------------------------------------------------
    # Teorema W ~ chi(n1) y R ~ chi(n2) indep
    # F = (W/n1)/(R/n2)
    # F ~ F(n1,n2)
    
    
    
    
    