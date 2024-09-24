library(shiny)
library(tidyverse)
library(bslib)
library(rxode2)
library(nonmem2rx)
library(plotly)

model <- nonmem2rx("VARS_904.out")

sidebar <- card(
  checkboxGroupInput("bolusinfusion", label = "Infusion/Bolus", choices = list("Bolus"=1, "Infusion"=2), selected = 1
                     ),
  conditionalPanel(
    condition = "input.regimen == 1 && input.bolusinfusion == 1",
    sliderInput("dose1", label = "Dose (mg)", step = 0.01, min=0, max = 3, value = 1, width = "100%")
  ),
  checkboxGroupInput("regimen", label = NULL, choices = list("Single Dose"=1,"Multiple Dose"=2), selected = 1),
  conditionalPanel(
    condition = "input.regimen == 1&& input.bolusinfusion == 2",
    sliderInput("dose1", label = "Dose (mg)", step = 0.01, min=0, max = 3, value = 1, width = "100%"),
    sliderInput("infusion1",label = "Infusion Hour (h)", step = 1, min = 0, max= 1344, value = 672, width = "100%")
  ),
  
  conditionalPanel(
    condition = "input.regimen == 2",
    sliderInput("dose2", label = "Dose (mg)",step = 0.01, min=0, max = 3, value = 1, width = "100%"),
    sliderInput("time2", label = "Number of Dose", step = 1, min = 0, max = 30, value = 10, width = "100%"),
    sliderInput("interval2", label = "Interval (h)", step = 1, min = 1, max = 1344, value = 672, width = "100%")
  ),

  
  actionButton("simulate", label = "Simulate"),
  sliderInput("hline","Target Concentration (ng/mL)", step=100, min =0, max=7000, value = 4000, width="100%"),
  hr(),
  h6("Observation Input"),
  fileInput("file1",label = "Observation"),
  hr()
)

body1 <- layout_columns(
  col_widths = c(6,6,12),
  card(plotlyOutput("simlationplot1"),
       plotlyOutput("simlationplot3")
       ),
  card(plotlyOutput("simlationplot2"),
       plotlyOutput("simlationplot4")
  )
  
)

body2 <- layout_columns(
  col_widths = c(6,6,12),
  card(plotlyOutput("simlationplotlog1"),
       plotlyOutput("simlationplotlog3")
  ),
  card(plotlyOutput("simlationplotlog2"),
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
    if (input$regimen == 1 & input$bolusinfusion ==1) {
      eventTable(amount.units = "mg", time.units = "hr") |>
        add.dosing(dose = as.double(input$dose1), nbr.doses = 1) |>
        add.sampling(seq(0, 24 * 7 * 16, by = 24))
    }
    else if (input$regimen == 1 & input$bolusinfusion ==2){
      eventTable(amount.units = "mg", time.units = "hr") |>
        add.dosing(dose = as.double(input$dose1), nbr.doses = 1, rate = (as.double(input$dose1)/as.double(input$infusion1)) ) |>
        add.sampling(seq(0, 24 * 7 * 16, by = 24))
    }
    
    else if (input$regimen == 2) {
      eventTable(amount.units = "mg", time.units = "hr") |>
        add.dosing(dose = as.double(input$dose2), nbr.doses = as.double(input$time2), dosing.interval = as.double(input$interval2)) |>
        add.sampling(seq(0, (input$time2*input$interval2+ 24*7), by = 24))
    }
  })
  
  rxsolve <- eventReactive(input$simulate, {
    rxSolve(model, dosing(), nSub = 1)
  })
  
  simulation_data <- reactive({
    results <- rxsolve()
    
    obs <- obs_data()
  
    VIT <- results |> 
      select(time, scale1, VIT) |> 
      mutate(day = time/24)
    
    AQ <- results |> 
      select(time, scale2, AQ)|> 
      mutate(day = time/24)
    
    RET <- results |> 
      select(time, scale3, RET)|> 
      mutate(day = time/24)
    
    SERUM <- results |> 
      select(time, scale4, SERUM)|> 
      mutate(day = time/24)
    
    obs1 <- obs |> 
      filter(SITE ==2)
    obs2 <- obs |> 
      filter(SITE ==1)
    obs3 <- obs |> 
      filter(SITE ==4)
    obs4 <- obs |> 
      filter(SITE ==7)
    
    obs_tidy1 <- obs1 |> 
      group_by(WEEK, GROUP) |> 
      summarize(DV = mean(DV)
      )
    obs_tidy2 <- obs2 |> 
      group_by(WEEK, GROUP) |> 
      summarize(DV = mean(DV)
      )
    obs_tidy3 <- obs3 |> 
      group_by(WEEK, GROUP) |> 
      summarize(DV = mean(DV)
      )
    obs_tidy4 <- obs4 |> 
      group_by(WEEK, GROUP) |> 
      summarize(DV = mean(DV)
      )
    
    
    list(VIT = VIT, AQ = AQ, RET=RET, SERUM=SERUM, 
         obs_tidy1=obs_tidy1,obs_tidy2=obs_tidy2,obs_tidy3=obs_tidy3,obs_tidy4=obs_tidy4,
         obs1= obs1,obs2= obs2,obs3= obs3,obs4= obs4)
  })
  
  output$simlationplot1 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$VIT, aes(x = day/7, y = VIT/scale1), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Aflibercept Concentration (ng/mL)", color = NULL, title="Vitreous") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((input$time2*input$interval2)+24*7)/7, by = 2), limits = c(0,NA)) +
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
      geom_line(data = data$AQ, aes(x = day/7, y = AQ/scale2), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Aflibercept Concentration (ng/mL)", color = NULL, title="Aqueous Humor") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((input$time2*input$interval2)+24*7)/7, by = 2), limits = c(0,NA)) +
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
      geom_line(data = data$RET, aes(x = day/7, y = RET/scale3), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Aflibercept Concentration (ng/mL)", color = NULL, title="Retina") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((input$time2*input$interval2)+24*7)/7, by = 2), limits = c(0,NA)) +
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
      geom_line(data = data$SERUM, aes(x = day/7, y = SERUM/scale4), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Aflibercept Concentration (ng/mL)", color = NULL, title="Serum") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((input$time2*input$interval2)+24*7)/7, by = 2), limits = c(0,NA)) +
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
      geom_line(data = data$VIT, aes(x = day/7, y = VIT/scale1),color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Aflibercept Concentration (ng/mL)", color = NULL, title="Vitreous") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((input$time2*input$interval2)+24*7)/7, by = 2), limits = c(0,NA)) +
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
      geom_line(data = data$AQ, aes(x = day/7, y = AQ/scale2),color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Aflibercept Concentration (ng/mL)", color = NULL, title="Aqueous Humor") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((input$time2*input$interval2)+24*7)/7, by = 2), limits = c(0,NA)) +
      geom_hline(yintercept = input$hline, color = "darkred")+ scale_y_log10()
    
    
    if(!(is.null(input$file1))) {
      plot <- plot+ geom_point(data= data$obs2, aes(x = WEEK, y= DV, color=GROUP), alpha = 0.5) +
        geom_line(data = data$obs_tidy2, aes(x = WEEK, y = DV, group=GROUP, color = GROUP)) + scale_y_log10()
    }
    plot <- ggplotly(plot)
  })
  output$simlationplotlog3 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$RET, aes(x = day/7, y = RET/scale3),color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Aflibercept Concentration (ng/mL)", color = NULL, title = "Retina") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((input$time2*input$interval2)+24*7)/7, by = 2), limits = c(0,NA)) +
      geom_hline(yintercept = input$hline, color = "darkred")+ scale_y_log10()
    
    
    if(!(is.null(input$file1))) {
      plot <- plot+ geom_point(data= data$obs3, aes(x = WEEK, y= DV, color=GROUP), alpha = 0.5) +
        geom_line(data = data$obs_tidy3, aes(x = WEEK, y = DV, group=GROUP, color = GROUP)) 
    }
    plot <- ggplotly(plot)
  })
  
  output$simlationplotlog4 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$SERUM, aes(x = day/7, y = SERUM/scale4),color = "#337AB7") + theme_bw() +
      labs(x = "Time (Week)", y = "Aflibercept Concentration (ng/mL)", color = NULL, title = "Serum") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, ((input$time2*input$interval2)+24*7)/7, by = 2), limits = c(0,NA)) +
      geom_hline(yintercept = input$hline, color = "darkred")+ scale_y_log10()
    
    
    if(!(is.null(input$file1))) {
      plot <- plot+ geom_point(data= data$obs4, aes(x = WEEK, y= DV, color=GROUP), alpha = 0.5) +
        geom_line(data = data$obs_tidy4, aes(x = WEEK, y = DV, group=GROUP, color = GROUP)) + scale_y_log10()
    }
    plot <- ggplotly(plot)
  })
}

shinyApp(ui, server)
