mapb2 <- function(eta) {
  # 1. 시뮬레이션 수행
  predicted <- model() |> 
    rxSolve(
      c(t.CL = 2.82,
        t.V1 = 31.8,
        t.Q = 11.7,
        t.V2 = 75.4,
        e.CL = eta[1],
        e.V2 = eta[2],
        Additive = 1e-06,
        Proportional = 0.284),
      dosing()
    ) |>
    as_tibble() |>
    filter(time %in% sampling_history()$Time) |>
    pull(ipred)
  
  # 2. 관측값
  y <- sampling_history()$Concentration
  
  # 3. 오차 계산
  EPS2SD <- 0.253
  sig2 <- (predicted * EPS2SD)^2
  sqwres <- sum(log(sig2) + (1/sig2)*(y - predicted)^2)
  
  # 4. eta penalty 계산
  omega <- matrix(c(0.685, 0, 0, 0.216), 2, 2)
  omega.inv <- solve(omega)
  nOn <- diag(t(eta) %*% omega.inv %*% eta)
  
  return(sqwres + nOn)
}