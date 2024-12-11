model <- function() {
  ini({
    t.CL <- 2.82
    label("1, CL")
    t.V1 <- 31.8
    label("2, V1")
    t.Q <- 11.7
    label("3, Q")
    t.V2 <- 75.4
    label("4, V2")
    Additive <- fix(0, 1e-06)
    label("5, Additive Residual Variability")
    Proportional <- c(0, 0.284)
    label("6, Proportional Residual Variability")
    e.CL ~ 0.685
    e.V2 ~ 0.216
  })
  
  model({
    cmt(central)
    cmt(peri)
    CL <- t.CL * exp(e.CL)
    V1 <- t.V1 * exp(e.K1)
    Q  <- t.Q  * exp(e.Q)
    V2 <- t.V2 * exp(e.V2)
    
    k1 <- Q/V1
    k2 <- Q/V2
    ke <- CL/V1
    scale1 <- V1/1000
    d/dt(central) <- peri*k2 - central*(ke+k1)
    d/dt(peri) <- central*k1 - peri*k2
    f <- central/scale1
    ipred <- f
    ipred ~ add(Additive) + prop(Proportional) + combined2()
  })
}