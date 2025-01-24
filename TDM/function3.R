# 최적화 실행 함수
run_optimization <- function(sampling_history, model, dosing) {
  # sampling_history는 reactive 함수이므로 ()를 붙여서 값을 가져옴
  current_sampling <- sampling_history()
  
  # 관찰값이 충분한지 확인
  if (nrow(current_sampling) < 1) return(NULL)
  
  # 초기값 설정
  ini <- c(0, 0)  # eta의 초기값
  
  # 최적화 실행
  FIT <- optim(
    par = ini,
    fn = function(eta) mapb2(eta, current_sampling, model, dosing),  # current_sampling을 값으로 전달
    method = "L-BFGS-B",
    lower = c(-2, -2),  # eta 값의 하한
    upper = c(2, 2),    # eta 값의 상한
    control = list(
      maxit = 500,      # 최대 반복 횟수
      trace = TRUE,     # 진행상황 출력
      factr = 1e7       # 수렴 기준
    )
  )
  
  # 최적화 결과 확인
  if (FIT$convergence != 0) {
    warning("Optimization did not converge properly")
  }
  
  return(FIT)
}

# mapb2 함수도 수정
mapb2 <- function(eta, sampling_data, model, dosing) {  # sampling_history 대신 sampling_data
  # 기본 모수 설정
  base_params <- list(
    t.CL = 2.82,
    t.V1 = 31.8,
    t.Q = 11.7,
    t.V2 = 75.4,
    Additive = 1e-06,
    Proportional = 0.284
  )
  
  # eta 값 추가
  params <- c(
    base_params,
    list(e.CL = eta[1], e.V2 = eta[2])
  )
  
  tryCatch({
    # 모델 시뮬레이션 실행
    out <- model |> 
      rxSolve(params, dosing) |>
      as_tibble() |>
      mutate(DV = central/V1)
    
    # 관찰된 농도값 사용
    obs_data <- sampling_data  # reactive 함수 호출 제거
    if (nrow(obs_data) == 0) return(Inf)
    
    # 잔차 계산
    residuals <- numeric(nrow(obs_data))
    for(i in 1:nrow(obs_data)) {
      pred <- subset(out$DV, out$time == obs_data$Time[i])
      if(length(pred) == 0) next
      obs <- obs_data$Concentration[i] / 1000  # mg/L로 변환
      
      # 비례오차 모델을 사용한 가중치
      weight <- 1 / (pred * params$Proportional)^2
      residuals[i] <- weight * (obs - pred)^2
    }
    
    # 모수 편차에 대한 페널티
    omega <- matrix(c(0.685, 0, 0, 0.216), 2, 2)
    omega.inv <- solve(omega)
    eta_penalty <- sum(diag(t(eta) %*% omega.inv %*% eta))
    
    # 최종 목적함수 값
    obj_value <- sum(residuals, na.rm = TRUE) + eta_penalty
    
    return(obj_value)
    
  }, error = function(e) {
    return(Inf)  # 에러 발생시 무한대 반환
  })
}