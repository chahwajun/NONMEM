
#Vancomycin
vancomycin <- function() {
  ini({
    # Population PK parameters
    t.CL <- 2.82
    label("1, CL")
    t.V1 <- 31.8
    label("2, V1")
    t.Q <- 11.7
    label("3, Q")
    t.V2 <- 75.4
    label("4, V2")
    # Residual Error    
    Additive <- fix(0, 1e-06)
    label("5, Additive Residual Variability")
    Proportional <- c(0, 0.284)
    label("6, Proportional Residual Variability")
    # Between-subject variability
    e.CL ~ 0.685
    e.V2 ~ 0.216
  })
  
  model({
    # Compartment
    cmt(central)
    cmt(peri)
    CL <- t.CL * exp(e.CL)
    V1 <- t.V1 
    Q  <- t.Q  
    V2 <- t.V2 * exp(e.V2)
    
    k1 <- Q/V1
    k2 <- Q/V2
    ke <- CL/V1
    scale1 <- V1/1000
    
    #DES
    d/dt(central) <- peri*k2 - central*(ke+k1)
    d/dt(peri) <- central*k1 - peri*k2
    
    f <- central/scale1
    ipred <- f
    ipred ~ add(Additive) + prop(Proportional) + combined2()
  })
}


# Cyclosporin

cyclosporin <- function() {
  ini({
    # Population PK parameters
    t.CL <- 28.5
    label("1, CL/F")
    t.V1 <- 133
    label("2, V/F")
    t.Ka <- 1.28
    label("3, Ka")
    
    # Covariate effects on CL
    beta.POD <- -1.24
    label("4, POD week effect on CL")
    beta.TBIL <- -0.252
    label("5, Total bilirubin effect on CL")
    beta.WT <- 0.188
    label("6, Weight effect on CL")
    beta.AGE <- -0.191
    label("7, Age effect on CL")
    
    # Residual Error
    prop.err <- 0.1
    label("8, Proportional Residual Error")
    add.err <- 40
    label("9, Additive Residual Error")
    
    # Between-subject variability
    e.CL ~ 0.04
    e.V1 ~ 0.04
  })
  
  model({
    # Compartment
    cmt(depot)
    cmt(central)
    
    # Covariate effects on CL
    CL <- (t.CL - (beta.POD * (POD_week)) - ((beta.TBIL * (TBIL - 10))) + (beta.WT * (WT - 60)) -(beta.AGE * (AGE - 40))
    ) * exp(e.CL)  
    
    V1 <- t.V1 * exp(e.V1)
    Ka <- t.Ka
    
    ke <- CL/V1
    
    #DES
    d/dt(depot) <- -Ka * depot
    d/dt(central) <- Ka * depot - ke * central
    
    cp <- central/V1 * 1000 
    
    ipred <- cp
    ipred ~ add(add.err) + prop(prop.err)
  })
}



vancomycin <- rxode(vancomycin)
cyclosporin <- rxode(cyclosporin)

