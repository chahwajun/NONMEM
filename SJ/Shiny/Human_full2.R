library(shiny)
library(tidyverse)
library(bslib)
library(rxode2)
library(nonmem2rx)
library(plotly)


model <- nonmem2rx("SG_111.ctl")  

sidebar <- card(
  textInput("dose", label = h2("Dose (vg/eye)", style ="font-size: 14px;"),value = 100000000),
  sliderInput("num", h2("Number of Dose",  style ="font-size: 14px;"), min = 1, max = 5, value = 3, width = "100%"),
  sliderInput("interval", h2("Dosing Interval (Month)", style ="font-size: 14px;"), min=0, max = 60, value = 30,width = "100%"),
  actionButton("simulate", label = "Simulate"),
  hr(),
  sliderInput("abline1", h2("Target level 1 (ng/mL)", style ="font-size: 14px;"), min = 0, max = 1000, step = 100, width = "100%", value = 800),
  sliderInput("abline2", h2("Target level 2 (ng/mL)", style ="font-size: 14px;"), min = 1000, max = 5000, step = 100, width = "100%", value = 2300)
)

abline <- card(
  h2("Adjustable Parameters (Fold)", style = "font-size: 14px;"),
  sliderInput("ktr",h2("KTR (RET1)", style ="font-size: 14px;"), min = 0, max = 5, value = 1,step = 0.1, width = "100%"), 
  sliderInput("ktr2", h2("KTR2 (RET2)", style ="font-size: 14px;"), min = 0, max = 5, value = 1, step = 0.1, width = "100%"), 
  sliderInput("ktr3",h2("KTR3 (VIT 1)", style ="font-size: 14px;"), min = 0, max = 5, value = 1, step = 0.1, width = "100%"), 
  sliderInput("ktr4",h2("KTR4 (VIT2)", style ="font-size: 14px;"), min = 0, max = 5, value = 1, step = 0.1, width = "100%"), 
  sliderInput("ktr5",h2("KTR5 (IRIS 1)",style ="font-size: 14px;"), min = 0, max = 5, value = 1, step = 0.1, width = "100%"),
  sliderInput("ktr6",h2("KTT6 (IRIS 2)",style ="font-size: 14px;"), min = 0, max = 5, value = 1, step = 0.1, width = "100%"), 
  sliderInput("cl39",h2("CL39 (OPT)",style ="font-size: 14px;"), min = 0, max = 5, value = 1, step = 0.1, width = "100%")
)

simulation <- navset_card_underline(
  nav_panel(
    title = h2("Linear", style = "font-size: 20px;"),
    layout_columns(
      col_widths = c(4,4,4,12),
      div(
        plotlyOutput("simlationplot1"),
        plotlyOutput("simlationplot3"),
      ),
      div(
        plotlyOutput("simlationplot2"),
        plotlyOutput("simlationplot5")
      ),
      div(
        plotlyOutput("simlationplot4"),
        plotlyOutput("simlationplot6"),
      )
    )
  ),
  nav_panel(
    title = h2("Log", style = "font-size: 20px;"),
    layout_columns(
      col_widths = c(4,4,4,12),
      div(
        plotlyOutput("simlationplotlog1"),
        plotlyOutput("simlationplotlog3")
      ),
      div(
        plotlyOutput("simlationplotlog2"),
        plotlyOutput("simlationplotlog5")
      ),
      div(
        plotlyOutput("simlationplotlog4"),
        plotlyOutput("simlationplotlog6"),
      )
    )
  )
)


ui <- page_fillable(
  h1("Human G001 Simulation"),
  hr(),
  layout_columns(
    col_widths = c(2,2,8,12),
    sidebar,
    abline,
    simulation
  ),
  hr()
)

server <- function(input, output, session){
  
  dosing <- eventReactive(input$simulate, {
    
    eventTable( time.units = "hr") |>
      add.dosing(dose = log(as.double(input$dose)), nbr.doses = as.double(input$num), dosing.interval = as.double(input$interval)*24*30) |>
      add.sampling(seq(0, max(60*24*40,as.double(input$num)*as.double(input$interval)*24*30), by = 24))
  })
  
  rxsolve <- eventReactive(input$simulate, {
    rxSolve(model, c(LNDOSE = log(as.double(input$dose)), t.KTR3 = 0.0000060*as.double(input$ktr3),
                     t.KTR = 0.000125 *as.double(input$ktr), t.CL39 = 0.00000024*as.double(input$cl39),
                     t.KTR5 = 0.004790*as.double(input$ktr5), t.KTR2 = 0.001120 *as.double(input$ktr2),
                     t.KTR4 = 0.002520 *as.double(input$ktr4), t.KTR6 = 0.000994 *as.double(input$ktr6)
    ), dosing(), nSub = 1)
  })
  
  simulation_data <- reactive({
    results <- rxsolve()
    VIT <- results |> 
      select(time, scale5, VIT) |> 
      mutate(month = time/(24*30))
    AQ <- results |> 
      select(time, scale6, AQ) |> 
      mutate(month = time/(24*30))
    RET <- results |> 
      select(time, scale7, RET)|> 
      mutate(month = time/(24*30))
    SERUM <- results |> 
      select(time, scale8, SERUM)|> 
      mutate(month = time/(24*30))
    OPT <- results |> 
      select(time, scale9, OPT)|> 
      mutate(month = time/(24*30))
    IRIS <- results |> 
      select(time, scale10, IRIS)|> 
      mutate(month = time/(24*30))
    
    list(VIT =VIT, AQ=AQ, RET=RET, SERUM=SERUM, OPT=OPT, IRIS=IRIS)
  })
  
  
  output$simlationplot1 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$VIT, aes(x = month, y = VIT/scale5), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Month)", y = "SB15 Concentration (ng/mL)", color = NULL, title="Vitreous") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      )  + scale_x_continuous(breaks = seq(0, max(60,as.double(input$num)*as.double(input$interval)),12), limits = c(0, max(60, as.double(input$num) * as.double(input$interval)))) + 
      geom_hline(yintercept = input$abline1, color = "red", linetype = "dashed") + 
      geom_hline(yintercept = input$abline2, color = "black",linetype = "dashed")
    plot <- ggplotly(plot)
  })
  
  output$simlationplot2 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$AQ, aes(x = month, y = AQ/scale6), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Month)", y = "SB15 Concentration (ng/mL)", color = NULL, title="Aqueous humor") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      )  + scale_x_continuous(breaks = seq(0, max(60,as.double(input$num)*as.double(input$interval)),12), limits = c(0, max(60, as.double(input$num) * as.double(input$interval)))) + 
      geom_hline(yintercept = input$abline1, color = "red",linetype = "dashed") + 
      geom_hline(yintercept = input$abline2, color = "black",linetype = "dashed")
    plot <- ggplotly(plot)
  })
  
  output$simlationplot3 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$RET, aes(x = month, y = RET/scale7), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Month)", y = "SB15 Concentration (ng/g)", color = NULL, title="Retina") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      )  + scale_x_continuous(breaks = seq(0, max(60,as.double(input$num)*as.double(input$interval)),12), limits = c(0, max(60, as.double(input$num) * as.double(input$interval)))) + 
      geom_hline(yintercept = input$abline1, color = "red",linetype = "dashed") + 
      geom_hline(yintercept = input$abline2, color = "black",linetype = "dashed")
    plot <- ggplotly(plot)
  })
  
  output$simlationplot4 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$SERUM, aes(x = month, y = SERUM/scale8), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Month)", y = "SB15 Concentration (ng/mL)", color = NULL, title="Serum") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      )  + scale_x_continuous(breaks = seq(0, max(60,as.double(input$num)*as.double(input$interval)),12), limits = c(0, max(60, as.double(input$num) * as.double(input$interval))))+ 
      geom_hline(yintercept = input$abline1, color = "red",linetype = "dashed") + 
      geom_hline(yintercept = input$abline2, color = "black",linetype = "dashed")
    plot <- ggplotly(plot)
  })
  output$simlationplot5 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$OPT, aes(x = month, y = OPT/scale9), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Month)", y = "SB15 Concentration (ng/g)", color = NULL, title="Optic nerve") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      )  +scale_x_continuous(breaks = seq(0, max(60,as.double(input$num)*as.double(input$interval)),12), limits = c(0, max(60, as.double(input$num) * as.double(input$interval)))) + 
      geom_hline(yintercept = input$abline1, color = "red",linetype = "dashed") + 
      geom_hline(yintercept = input$abline2, color = "black",linetype = "dashed")
    plot <- ggplotly(plot)
  })
  
  output$simlationplot6 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$IRIS, aes(x = month, y = IRIS/scale10), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Month)", y = "SB15 Concentration (ng/g)", color = NULL, title="IRIS") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      )  + scale_x_continuous(breaks = seq(0, max(60,as.double(input$num)*as.double(input$interval)),12), limits = c(0, max(60, as.double(input$num) * as.double(input$interval)))) + 
      geom_hline(yintercept = input$abline1, color = "red",linetype = "dashed") + 
      geom_hline(yintercept = input$abline2, color = "black",linetype = "dashed")
    plot <- ggplotly(plot)
  })
  
  output$simlationplotlog1 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$VIT, aes(x = month, y = VIT/scale5), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Month)", y = "SB15 Concentration (ng/g)", color = NULL, title="Vitreous") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      )  + scale_x_continuous(breaks = seq(0, max(60,as.double(input$num)*as.double(input$interval)),12), limits = c(0, max(60, as.double(input$num) * as.double(input$interval)))) + 
      geom_hline(yintercept = input$abline1, color = "red",linetype = "dashed") + 
      geom_hline(yintercept = input$abline2, color = "black",linetype = "dashed") + scale_y_log10()
    plot <- ggplotly(plot)
  })
  
  output$simlationplotlog2 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$AQ, aes(x = month, y = AQ/scale6), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Month)", y = "SB15 Concentration (ng/mL)", color = NULL, title="Aqueous humor") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      ) + scale_x_continuous(breaks = seq(0, max(60,as.double(input$num)*as.double(input$interval)),12), limits = c(0, max(60, as.double(input$num) * as.double(input$interval))))+ 
      geom_hline(yintercept = input$abline1, color = "red",linetype = "dashed") + 
      geom_hline(yintercept = input$abline2, color = "black",linetype = "dashed") + scale_y_log10()
    plot <- ggplotly(plot)
  })
  
  output$simlationplotlog3 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$RET, aes(x = month, y = RET/scale7), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Month)", y = "SB15 Concentration (ng/g)", color = NULL, title="Retina") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      )  + scale_x_continuous(breaks = seq(0, max(60,as.double(input$num)*as.double(input$interval)),12), limits = c(0, max(60, as.double(input$num) * as.double(input$interval)))) +  
      geom_hline(yintercept = input$abline1, color = "red",linetype = "dashed") + 
      geom_hline(yintercept = input$abline2, color = "black",linetype = "dashed") + scale_y_log10()
    plot <- ggplotly(plot)
  })
  
  output$simlationplotlog4 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$SERUM, aes(x = month, y = SERUM/scale8), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Month)", y = "SB15 Concentration (ng/mL)", color = NULL, title="Serum") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      )  + scale_x_continuous(breaks = seq(0, max(60,as.double(input$num)*as.double(input$interval)),12), limits = c(0, max(60, as.double(input$num) * as.double(input$interval)))) + 
      geom_hline(yintercept = input$abline1, color = "red",linetype = "dashed") + 
      geom_hline(yintercept = input$abline2, color = "black",linetype = "dashed") + scale_y_log10()
    plot <- ggplotly(plot)
  })
  output$simlationplotlog5 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$OPT, aes(x = month, y = OPT/scale9), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Month)", y = "SB15 Concentration (ng/g)", color = NULL, title="Optic nerve") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      )  + scale_x_continuous(breaks = seq(0, max(60,as.double(input$num)*as.double(input$interval)),12), limits = c(0, max(60, as.double(input$num) * as.double(input$interval)))) + 
      geom_hline(yintercept = input$abline1, color = "red",linetype = "dashed") + 
      geom_hline(yintercept = input$abline2, color = "black",linetype = "dashed") + scale_y_log10()
    plot <- ggplotly(plot)
  })
  
  output$simlationplotlog6 <- renderPlotly({
    
    data <- simulation_data()
    
    plot <- ggplot() +
      geom_line(data = data$IRIS, aes(x = month, y = IRIS/scale10), color = "#337AB7") + theme_bw() +
      labs(x = "Time (Month)", y = "SB15 Concentration (ng/g)", color = NULL, title="IRIS") + 
      theme(axis.text.x = element_text(vjust = 0.5, size = 12),
            axis.text.y = element_text(vjust = 0.5, size = 12),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.title = element_text(size = 12)
      )  + scale_x_continuous(breaks = seq(0, max(60,as.double(input$num)*as.double(input$interval)),12), limits = c(0, max(60, as.double(input$num) * as.double(input$interval)))) + 
      geom_hline(yintercept = input$abline1, color = "red",linetype = "dashed") + 
      geom_hline(yintercept = input$abline2, color = "black",linetype = "dashed") + scale_y_log10()
    plot <- ggplotly(plot)
  })
}



shinyApp(ui, server)