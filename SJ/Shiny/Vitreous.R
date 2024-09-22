library(shiny)
library(tidyverse)
library(bslib)
library(rxode2)
library(nonmem2rx)

model <- nonmem2rx("SB15_V32.out")

sidebar <- card(
  card_title("Simulation Regimen"),
  checkboxGroupInput("regimen", label = NULL, choices = list("Single Dose"=1,"Multiple Dose"=2), selected = 1),
  conditionalPanel(
    condition = "input.regimen == 1",
    sliderInput("dose1", label = "Dose (mg)", step = 0.1, min=0, max = 5, value = 50, width = "100%")
  ),
  conditionalPanel(
    condition = "input.regimen == 2",
    sliderInput("dose2", label = "Dose (mg)",step = 0.1, min=0, max = 5, value = 50, width = "100%"),
    sliderInput("time2", label = "Number of Dose", step = 1, min = 1, max = 100, value = 50, width = "100%"),
    sliderInput("interval2", label = "Interval (h)", step = 1, min = 1, max = 1344, value = 100, width = "100%")
  ),
  actionButton("simulate", label = "Simulate"),
  hr(),
  h6("Observation Input"),
  fileInput("file1",label = "Observation"),
  hr(),
  h6("NONMEM output file"),
)

body <- card(
  card_title("Simulation Plot"),
  plotOutput("simlationplot"),
  plotOutput("logsimulationplot"),
  tableOutput("table1")
)

ui <- page_navbar(
  title = "Simulation",
  hr(),
  layout_columns(
    col_widths = c(3,9,12),
    sidebar,
    body
  ),
  hr()
)

server <- function(input, output, session) {
  
  obs_data <- reactive({
    file <- input$file1
    if (is.null(file)) {
      return(data.frame(DAY = numeric(0), DV = numeric(0), GROUP = character(0)))
    }
    ext <- tools::file_ext(file$datapath)
    data <- read.csv(file$datapath, header = TRUE)
  })
  
  dosing <- eventReactive(input$simulate, {
    if (input$regimen == 1) {
      eventTable(amount.units = "mg", time.units = "hr") |>
        add.dosing(dose = as.double(input$dose1), nbr.doses = 1) |>
        add.sampling(seq(0, 24 * 7 * 16, by = 24))
    } else if (input$regimen == 2) {
      eventTable(amount.units = "mg", time.units = "hr") |>
        add.dosing(dose = as.double(input$dose2), nbr.doses = as.double(input$time2), dosing.interval = as.double(input$interval2)) |>
        add.sampling(seq(0, input$time2*input$interval2+240, by = 24))
    }
  })
  
  rxsolve <- eventReactive(input$simulate, {
    rxSolve(model, dosing(), nSub = 500)
  })
  
  simulation_data <- reactive( {
    results <- rxsolve()
    obs <- obs_data()
    iqr <- results |>
      group_by(time) |>
      summarise(
        ipred_max = quantile(ipred, 0.95),
        ipred_min = quantile(ipred, 0.05)
      ) |> 
      mutate(day = time / 24)
    
    median <- results |>
      group_by(time) |>
      summarise(ipred_median = median(ipred)) |> 
      mutate(day = time / 24)
    
    obs_tidy <- obs |> 
      group_by(DAY, GROUP) |> 
      summarize(DV = mean(DV)
                )
    list(iqr = iqr, median = median, obs_tidy=obs_tidy, obs= obs)
  })
  
  output$simlationplot <- renderPlot({

    data <- simulation_data()
    
    plot <- ggplot() +
      geom_ribbon(data = data$iqr, aes(x = day, ymin = ipred_min, ymax = ipred_max),fill = "#337AB7" , alpha = 0.3) +
      geom_line(data = data$median, aes(x = day, y = ipred_median, color = "Median Concentration")) + theme_bw() +
      labs(x = "Time (Day)", y = "Aflibercept Concentration (ng/mL)", color = NULL) + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, (input$time2*input$interval2)+240, by = 7), limits = c(0,NA)) +
      scale_color_manual(values = c("Median Concentration"="cyan4")) 
    
    if (is.null(input$file1)) {
      plot 
    }
    else if(!(is.null(input$file1))) {
      plot+ geom_point(data= data$obs, aes(x = DAY, y= DV, group=GROUP), alpha = 0.2) +
        geom_line(data = data$obs_tidy, aes(x = DAY, y = DV, group=GROUP), color = "darkred")+
        scale_color_manual(values = c("Median Concentration"="cyan4"))
    }
      
  })
  
  output$logsimulationplot <- renderPlot({
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_ribbon(data = data$iqr, aes(x = day, ymin = ipred_min, ymax = ipred_max),fill = "#337AB7" , alpha = 0.3) +
      geom_line(data = data$median, aes(x = day, y = ipred_median, color = "Median Concentration")) + theme_bw() +
      labs(x = "Time (Day)", y = "Aflibercept Concentration (ng/mL)", color = NULL) + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, (input$time2*input$interval2)+240, by = 7), limits = c(0,NA)) +
      scale_color_manual(values = c("Median Concentration"="cyan4")) 
    
    if (is.null(input$file1)) {
      plot +  scale_y_log10() 
      
    }
    else if(!(is.null(input$file1))) {
      plot+ geom_point(data= obs_data(), aes(x = DAY, y= DV, color=GROUP), alpha = 0.5) +
        scale_color_manual(values = c("Median Concentration"="cyan4"))+ 
        geom_line(data = data$obs_tidy, aes(x = DAY, y = DV, group=GROUP), color = "darkred")+
        scale_y_log10() 
    }
  })
}

shinyApp(ui, server)
