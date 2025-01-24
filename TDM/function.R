objective_fn <- function(eta, observed_data, dosing_info, model) {
  # 파라미터 설정 
  params <- c(
    t.CL = 2.82,
    t.V1 = 31.8,
    t.Q = 11.7,
    t.V2 = 75.4,
    e.CL = eta[1],  # CL 개별화
    e.V2 = eta[2],  # V2 개별화
    Additive = 1e-06,
    Proportional = 0.284
  )
  
  # 시뮬레이션 수행
  sim_result <- model |> 
    rxSolve(params, dosing_info)
  
  # 예측값 추출
  predicted <- sim_result |> 
    as_tibble() |> 
    filter(time %in% observed_data$Time) |> 
    pull(ipred)  # ipred 사용
  
  # 잔차 계산 
  residual_error <- 0
  for(i in 1:length(predicted)) {
    sig2 <- (predicted[i] * 0.284)^2  # Proportional error
    residual_error <- residual_error + 
      log(sig2) + (observed_data$Concentration[i] - predicted[i])^2/sig2
  }
  
  # eta penalty (from model's BSV)
  omega <- matrix(c(0.685, 0, 0, 0.216), 2, 2)  # BSV from model
  omega.inv <- solve(omega)
  penalty <- t(eta) %*% omega.inv %*% eta
  
  return(residual_error + penalty)
}


estimate_eta <- function(observed_data, dosing_info, model) {
  # 초기값
  initial_eta <- c(0, 0)
  
  # 최적화
  result <- optim(
    par = initial_eta,
    fn = objective_fn,
    observed_data = observed_data,
    dosing_info = dosing_info,
    model = model,
    method = "L-BFGS-B"
  )
  
  return(result$par)
}