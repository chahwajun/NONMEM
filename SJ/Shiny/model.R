model <- function(){
  dfObs <- 480
  dfSub <- 144
  sigma <- lotri({
    eps1 ~ 1.62
    eps2 ~ 0.588
    eps3 ~ 1.47
    eps4 ~ 5.57
  })
  ini({
    t.Q12 <- c(0, 8.39e-07)
    label("Q12")
    t.Q13 <- c(0, 2.3e-06)
    label("Q13")
    t.Q14 <- c(0, 1.92e-06)
    label("Q14")
    t.V1 <- fix(0.0015)
    label("V1 VIT VAROIS_19")
    t.V2 <- fix(6.86e-05)
    label("V2 RET VAROIS_19")
    t.V3 <- fix(2e-04)
    label("V3 OPT VAROIS_19")
    t.V4 <- fix(1.08e-05)
    label("V4 IRIS VAROIS_19 Est.")
    t.CL1 <- c(0, 4.34e-07)
    label("CL1")
    t.CL2 <- c(0, 3.99e-06)
    label("CL2")
    t.CL3 <- c(0, 0.000105)
    label("CL3")
    t.CL4 <- c(0, 9.7e-05)
    label("CL4")
    theta12 <- fix(1e-04)
    label("add ")
    theta13 <- c(0, 1.64)
    label("prop")
    theta14 <- fix(1e-04)
    label("add ")
    theta15 <- c(0, 1.15)
    label("prop")
    theta16 <- fix(1e-04)
    label("add ")
    theta17 <- c(0, 1.82)
    label("prop")
    theta18 <- fix(1e-04)
    label("add ")
    theta19 <- c(0, 3.54)
    label("prop")
    F1 <- c(0, 0.0959, 1)
    label("F1 20")
    t.Q41 <- c(0, 3.87e-05)
    label("Q41 21")
    t.Q31 <- c(0, 9.56e-05)
    label("Q31 22")
    e.Q12 ~ fix(0)
    e.Q13 ~ fix(0)
    e.Q14 ~ fix(0)
    e.V1 ~ fix(0)
    e.V2 ~ fix(0)
    e.V3 ~ fix(0)
    e.V4 ~ fix(0)
    e.CL1 ~ fix(0)
    e.CL2 ~ fix(0)
    e.CL3 ~ fix(0)
    e.CL4 ~ fix(0)
    e.Q41 ~ fix(0)
  })
  model({
    cmt(VIT)
    cmt(RET)
    cmt(OPT)
    cmt(IRIS)
    q12 <- t.Q12 * exp(e.Q12)
    q13 <- t.Q13 * exp(e.Q13)
    q14 <- t.Q14 * exp(e.Q14)
    q41 <- t.Q41 * exp(e.Q41)
    q31 <- t.Q31 * exp(e.Q41)
    v1 <- t.V1 * exp(e.V1)
    v2 <- t.V2 * exp(e.V2)
    v3 <- t.V3 * exp(e.V3)
    v4 <- t.V4 * exp(e.V4)
    cl1 <- t.CL1 * exp(e.CL1)
    cl2 <- t.CL2 * exp(e.CL2)
    cl3 <- t.CL3 * exp(e.CL3)
    cl4 <- t.CL4 * exp(e.CL4)
    rxf.rxddta1. <- F1
    f(VIT) <- rxf.rxddta1.
    
    scale1 <- v1/1000
    scale2 <- v2/1000
    scale3 <- v3/1000
    scale4 <- v4/1000
    k12 <- q12/v1
    k21 <- q12/v2
    k13 <- q13/v1
    k31 <- q31/v3
    k14 <- q14/v1
    k41 <- q41/v4
    k10 <- cl1/v1
    k20 <- cl2/v2
    k30 <- cl3/v3
    k40 <- cl4/v4
    d/dt(VIT) <- -(k10 + k12 + k13 + k14) * VIT + k21 * RET + 
      k31 * OPT + k41 * IRIS
    d/dt(RET) <- k12 * VIT - (k20 + k21) * RET
    d/dt(OPT) <- k13 * VIT - (k30 + k31) * OPT
    d/dt(IRIS) <- k14 * VIT - (k40 + k41) * IRIS
    f <- VIT/scale1
    if (CMT == 1) {
      ipred <- f
      w <- sqrt(theta12^2 + (theta13 * ipred)^2)
      ires <- DV - ipred
      iwres <- ires/w
      y <- ipred + w * eps1
    }
    if (CMT == 2) {
      ipred <- f
      w <- sqrt(theta14^2 + (theta15 * ipred)^2)
      ires <- DV - ipred
      iwres <- ires/w
      y <- ipred + w * eps2
    }
    if (CMT == 3) {
      ipred <- f
      w <- sqrt(theta16^2 + (theta17 * ipred)^2)
      ires <- DV - ipred
      iwres <- ires/w
      y <- ipred + w * eps3
    }
    if (CMT == 4) {
      ipred <- f
      w <- sqrt(theta18^2 + (theta19 * ipred)^2)
      ires <- DV - ipred
      iwres <- ires/w
      y <- ipred + w * eps4
    }
  })
}

model <- rxode2(model)