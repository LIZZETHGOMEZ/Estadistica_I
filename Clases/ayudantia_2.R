    # ##########################################################################
    #                     Martes 25 de Mayo de 2021
    #                            Ayudantia
    #                     Continuacion de Proba
    # ##########################################################################
    
    # Simulaciones
    
    # Distribucion de la suma de dos binomiales
    X <- rbinom(1000, 20, 0.3)
    Y <- rbinom(1000, 30, 0.3)
    
    # X + Y ~ bin(50, 0.3)
    # E[X+Y] = n+p = 50*0.3
    mean(X+Y) 
    hist(X)
    
    X <- rbinom(1000, 20, 0.6)
    Y <- rbinom(1000, 30, 0.3)
    mean(X+Y) #Esperanza
    
    
    # POISSON
    PoisSample <- rpois(10000,2)
    hist(PoisSample)
    mean(PoisSample == 0)
    
    dpois(0,2)
    
    # Distribucion Geometrica
    rbinom(10000,1,0.4) #Esta es una Bernoulli
    
    # La repetimos varias veces
    set.seed(15)
    which(rbinom(10000,1,0.4) ==1) 
    
    #Extraemos la primera simulacion, el primer exito, varias veces
    simGeo <- replicate(10000, which(rbinom(10000,1,0.4) ==1)[1])
    simGeo    
    hist(simGeo)    
    
    # Aqui verificamos como podemos aporximar una geometrica a través de una binomial
    geom <- rgeom(10000, 0.4)
    hist(simGeo, add = T, col = "blue")
    
    
    # Binomial y Poisson
    binom <- rbinom(10000,1000,.002)
    poi <- rpois(10000,1000*.002)
    hist(binom, col = "red")  
    hist(poi, add = T, col = "blue")

    # BINOMIAL Y NORMAL
    binom <- rbinom(100000,1000, 0.2)
    normal <- rnorm(100000, 200, sqrt(160))    
    
    mean(binom <= 190)    
    mean(normal <= 190)
    