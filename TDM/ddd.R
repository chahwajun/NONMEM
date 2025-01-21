observeEvent(input$report2, {
  req(input$targettrough2, input$infusiontime2, input$interval2)  
  
  target_trough <- as.numeric(input$targettrough2)
  infusion_time <- as.numeric(input$infusiontime2)
  interval <- as.numeric(input$interval2)
  
  init <- c(central = 0, peri = 0)
  params <- c(CL = 2.82, V1 = 31.8, Q = 11.7, V2 = 75.4, e.CL = 0, e.V2 = 0)
  
  result <- optim(
    par = 100,
    fn = function(dose) {
      sim <- model |> 
        rxSolve(
          params, 
          init, 
          events = et(amt = dose, dur = infusion_time) |> 
            add.sampling(seq(0, interval*2, by = 0.1)
            )
        )
      if (nrow(sim) == 0) return(Inf)
      trough <- tail(sim$central, 1)
      return((trough - target_trough)^2)
    },
    method="L-BFGS-B"
  )
  
  if (result$convergence == 0) {
    recommended_dose(round(result$par, 2)) # Update reactive value
  } else {
    recommended_dose(NULL) # Reset if optimization fails
  }
})

output$recommended_dose <- renderText({
  if (!is.null(recommended_dose())) {
    paste("Recommended Dose:", recommended_dose(), "mg")
  } else {
    "Optimization failed. Please check inputs."
  }
})


output$population <- renderPlot({
  dose <- recommended_dose()
  
  if (!is.null(dose)) {
    target_trough <- as.numeric(input$targettrough2)
    infusion_time <- as.numeric(input$infusiontime2)
    interval <- as.numeric(input$interval2)
    
    init <- c(central = 0, peri = 0)
    params <- c(CL = 2.82, V1 = 31.8, Q = 11.7, V2 = 75.4, e.CL = 0, e.V2 = 0)
    
    sim <- model |> 
      rxSolve(
        params, 
        init, 
        events = et(amt = dose, dur = infusion_time) |> 
          add.sampling(seq(0, interval*2, by = 0.1))
      )
    
    sampling_data <- sampling_history() 
    
    
    ggplot(sim, aes(x = time, y = central/V1)) +
      geom_line(color = "black", size = 1) + 
      geom_point(data = sampling_data, aes(x = Time, y = Concentration), color = "blue", size = 2) +
      geom_hline(yintercept = target_trough, linetype = "dashed", color = "red", size = 0.8) +  theme_bw() +
      labs(x = "Time (hours)",y = "Vancomycin Concentration (mg/L)")  + scale_x_continuous(breaks = seq(0,interval*2,4))
  } else {
    NULL
  }
  
})