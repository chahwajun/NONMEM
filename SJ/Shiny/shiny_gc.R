library(shiny)
library(tidyverse)
library(bslib)
library(rxode2)
library(nonmem2rx)
library(plotly)

source("model.R")

sidebar <- card(
    textInput("dose1", label = "Dose (vg)",value = 300000000),
    sliderInput("num", "Number of Dose", min = 1, max = 5, value = 3),
    sliderInput("interval", "Dosing Interval (Month)", min=0, max = 60, value = 30),
    textInput("Intercept1", "Intercept",value = -1.47403),
    textInput("Slope1","Slope", value = 0.08019),
    verbatimTextOutput("Formula"),
    h4("Dose (vg)"),
    verbatimTextOutput("Dose"),
    h4("Calculated F"),
    verbatimTextOutput("F1"),
  actionButton("simulate", label = "Simulate"),
  hr(),
  h6("Observation Input"),
  fileInput("file1",label = "Observation"), 
  hr()
)

body1 <- layout_columns(
  col_widths = c(6,6,12),
  card(plotlyOutput("simlationplot1"),
       plotlyOutput("simlationplot2")
  ),
  card(plotlyOutput("simlationplot3"),
       plotlyOutput("simlationplot4")
  ),
)

body2 <- layout_columns(
  col_widths = c(6,6,12),
  card(plotlyOutput("simlationplotlog1"),
       plotlyOutput("simlationplotlog2")
  ),
  card(plotlyOutput("simlationplotlog3"),
       plotlyOutput("simlationplotlog4")
  )
)

ui <- page_navbar(
  title = "Simulation",
  hr(),
  layout_columns(
    col_widths = c(3,9,12),
    sidebar,
    navset_underline(
      hr(),
      nav_panel(title = "Normal",
                body1),
      nav_panel(title = "Log",
                body2)
    )
  ),
  hr()
)

server <- function(input, output, session) {
  
  obs_data <- reactive({
    file <- input$file1
    if (is.null(file)) {
      return(data.frame(WEEK = numeric(0), DV = numeric(0), GROUP = character(0),SITE = numeric(0)))
    }
    ext <- tools::file_ext(file$datapath)
    data <- read.csv(file$datapath, header = TRUE)
  }) 
  
  dosing <- eventReactive(input$simulate, {
      eventTable(amount.units = "mg", time.units = "hr") |>
        add.dosing(dose = log(as.double(input$dose1)), nbr.doses = 1) |>
        add.sampling(seq(0, as.double(input$num)*as.double(input$interval)*24*30*2, by = 1*24*30))

  })
  
  rxsolve <- eventReactive(input$simulate, {
    group_value <- 2
    rxSolve(model, c(GROUP = group_value, Intercept = as.double(input$intercept1), Slope = as.double(input$slope1)), dosing(), nSub = 1)
  })
  
  simulation_data <- reactive({
    results <- rxsolve()
    
    obs <- obs_data()
    
    VIT <- results |> 
      select(time, scale1, VIT) |> 
      mutate(month = time/(24*30))
    
    RET <- results |> 
      select(time, scale2, RET)|> 
      mutate(month = time/(24*30))
    
    IRIS <- results |> 
      select(time, scale4, IRIS)|> 
      mutate(month = time/(24*30))
    
    OPT <- results |> 
      select(time, scale3, OPT)|> 
      mutate(month = time/(24*30))
    
    obs1 <- obs |> 
      filter(SITE ==2)
    obs2 <- obs |> 
      filter(SITE ==4)
    obs3 <- obs |> 
      filter(SITE ==6)
    obs4 <- obs |> 
      filter(SITE ==3)
    

    obs_tidy1 <- obs1 |> 
      group_by(WEEK, GROUP) |> 
      summarize(DV = mean(DV)
      ) |> 
      mutate(month = WEEK/4)
    obs_tidy2 <- obs2 |> 
      group_by(WEEK, GROUP) |> 
      summarize(DV = mean(DV)
      )|> 
      mutate(month = WEEK/4)
    obs_tidy3 <- obs3 |> 
      group_by(WEEK, GROUP) |> 
      summarize(DV = mean(DV)
      )|> 
      mutate(month = WEEK/4)
    obs_tidy4 <- obs4 |> 
      group_by(WEEK, GROUP) |> 
      summarize(DV = mean(DV)
      )|> 
      mutate(month = WEEK/4)

  
    list(VIT = VIT, RET=RET,  IRIS=IRIS, OPT=OPT,
         obs_tidy1=obs_tidy1,obs_tidy2=obs_tidy2,obs_tidy3=obs_tidy3,obs_tidy4=obs_tidy4,
         obs1= obs1,obs2= obs2,obs3= obs3,obs4= obs4)
  })
  
  output$simlationplot1 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$VIT, aes(x = month, y = VIT/scale1), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Gene count (copies/host DNA)", color = NULL, title="Vitreous") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ), limits = c(0,NA)) +
      geom_hline(yintercept = input$hline, color = "darkred")
    
    if(!(is.null(input$file1))) {
      plot <- plot + geom_point(data = data$obs1, aes(x = WEEK, y = DV, color = GROUP), alpha = 0.5) +
        geom_line(data = data$obs_tidy1, aes(x = WEEK, y = DV, group = GROUP, color = GROUP)) 
    }
    plot <- ggplotly(plot)
  })
  
  output$simlationplot2 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$RET, aes(x = month, y = RET/scale2), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Gene count (copies/host DNA)", color = NULL, title="Retina") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((1344)+24*7)/7, by = 2), limits = c(0,NA)) +
      geom_hline(yintercept = input$hline, color = "darkred")
    
    if(!(is.null(input$file1))) {
      plot <- plot + geom_point(data = data$obs2, aes(x = WEEK, y = DV, color = GROUP), alpha = 0.5) +
        geom_line(data = data$obs_tidy2, aes(x = WEEK, y = DV, group = GROUP, color = GROUP)) 
    }
    plot <- ggplotly(plot)
  })
  
  output$simlationplot3 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$OPT, aes(x = month, y = OPT/scale3), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Gene count (copies/host DNA)", color = NULL, title="Optic Nerve") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((1344)+24*7)/7, by = 2), limits = c(0,NA)) +
      geom_hline(yintercept = input$hline, color = "darkred")
    
    if(!(is.null(input$file1))) {
      plot <- plot + geom_point(data = data$obs3, aes(x = WEEK, y = DV, color = GROUP), alpha = 0.5) +
        geom_line(data = data$obs_tidy3, aes(x = WEEK, y = DV, group = GROUP, color = GROUP)) 
    }
    plot <- ggplotly(plot)
  })
  output$simlationplot4 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$IRIS, aes(x = month, y = IRIS/scale4), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Gene count (copies/host DNA)", color = NULL, title="Iris") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((1344)+24*7)/7, by = 2), limits = c(0,NA)) +
      geom_hline(yintercept = input$hline, color = "darkred")
    
    if(!(is.null(input$file1))) {
      plot <- plot + geom_point(data = data$obs4, aes(x = WEEK, y = DV, color = GROUP), alpha = 0.5) +
        geom_line(data = data$obs_tidy4, aes(x = WEEK, y = DV, group = GROUP, color = GROUP)) 
    }
    plot <- ggplotly(plot)
  })
  
  
  output$simlationplotlog1 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$VIT, aes(x = month, y = VIT/scale1),color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Gene count (copies/host DNA)", color = NULL, title="Vitreous") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((1344)+24*7)/7, by = 2), limits = c(0,NA)) +
      geom_hline(yintercept = input$hline, color = "darkred")+ scale_y_log10()
    
    
    if(!(is.null(input$file1))) {
      plot <- plot+ geom_point(data= data$obs1, aes(x = WEEK, y= DV, color=GROUP), alpha = 0.5) +
        geom_line(data = data$obs_tidy1, aes(x = WEEK, y = DV, group=GROUP, color = GROUP)) + scale_y_log10()
    }
    plot <- ggplotly(plot)
  })
  
  output$simlationplotlog2 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$RET, aes(x = month, y = RET/scale2),color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Gene count (copies/host DNA)", color = NULL, title = "Retina") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((1344)+24*7)/7, by = 2), limits = c(0,NA)) +
      geom_hline(yintercept = input$hline, color = "darkred")+ scale_y_log10()
    
    
    if(!(is.null(input$file1))) {
      plot <- plot+ geom_point(data= data$obs2, aes(x = WEEK, y= DV, color=GROUP), alpha = 0.5) +
        geom_line(data = data$obs_tidy2, aes(x = WEEK, y = DV, group=GROUP, color = GROUP)) 
    }
    plot <- ggplotly(plot)
  })
  

  output$simlationplotlog3 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$OPT, aes(x = month, y = OPT/scale3),color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Gene count (copies/host DNA)", color = NULL, title = "Optic Nerve") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((1344)+24*7)/7, by = 2), limits = c(0,NA)) +
      geom_hline(yintercept = input$hline, color = "darkred")+ scale_y_log10()
    
    
    if(!(is.null(input$file1))) {
      plot <- plot+ geom_point(data= data$obs3, aes(x = WEEK, y= DV, color=GROUP), alpha = 0.5) +
        geom_line(data = data$obs_tidy3, aes(x = WEEK, y = DV, group=GROUP, color = GROUP)) + scale_y_log10()
    }
    plot <- ggplotly(plot)
  })
  output$simlationplotlog4 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$IRIS, aes(x = month, y = IRIS/scale4),color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Gene count (copies/host DNA)", color = NULL, title = "Iris") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((1344)+24*7)/7, by = 2), limits = c(0,NA)) +
      geom_hline(yintercept = input$hline, color = "darkred")+ scale_y_log10()
    
    
    if(!(is.null(input$file1))) {
      plot <- plot+ geom_point(data= data$obs4, aes(x = WEEK, y= DV, color=GROUP), alpha = 0.5) +
        geom_line(data = data$obs_tidy4, aes(x = WEEK, y = DV, group=GROUP, color = GROUP)) + scale_y_log10()
    }
    plot <- ggplotly(plot)
  })


  output$F1 <- renderPrint({
    req(input$Slope1, input$Intercept1, input$dose1) 
    slope <- as.numeric(input$Slope1)
    intercept <- as.numeric(input$Intercept1)
    dose <- as.numeric(input$dose1)
    
    result <- slope * log(dose) + intercept
    return(result)
  })
  
  output$Dose <- renderPrint({
    req(input$dose1)
    num <- as.numeric(input$dose1)
    paste(format(num, scientific = TRUE), "vg")
  })
  output$Formula <- renderPrint({
    paste0("F","=","Intercept","+","Slope*LNDose")
  })
  
}

shinyApp(ui, server)
