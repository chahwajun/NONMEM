library(shiny)
library(tidyverse)
library(bslib)
library(rxode2)
library(nonmem2rx)
library(plotly)

#model <- nonmem2rx("SG_11.ctl")  

sidebar <- card(
  textInput("dose", label = "Dose (vg/eye)",value = 100000000),
  sliderInput("num", "Number of Dose", min = 1, max = 5, value = 3, width = "100%"),
  sliderInput("interval", "Dosing Interval (Month)", min=0, max = 60, value = 30,width = "100%"),
  actionButton("simulate", label = "Simulate"),
  hr(),
    sliderInput("abline1", "Protein Concentration (ng/mL)", min = 0, max = 1000, step = 100, width = "100%", value = 800),
    sliderInput("abline2", "Protein Concentration (ng/mL)", min = 1000, max = 5000, step = 100, width = "100%", value = 2300),
    sliderInput("abline3", "Protein Concentration (ng/mL)", min = 5000, max = 10000, step = 1000, width = "100%", value = 7000)
)

abline <- card(
  h3("Adjustable Parameters (Fold)", style = "font-size: 20px;"),
  sliderInput("ktr","KTR", min = 0, max = 5, value = 1,step = 0.1, width = "100%"), 
  sliderInput("ktr2","KTR2", min = 0, max = 5, value = 1, step = 0.1, width = "100%"), 
  sliderInput("ktr3","KTR3", min = 0, max = 5, value = 1, step = 0.1, width = "100%"), 
  sliderInput("ktr4","KTR4", min = 0, max = 5, value = 1, step = 0.1, width = "100%"), 
  sliderInput("ktr5","KTR5", min = 0, max = 5, value = 1, step = 0.1, width = "100%"),
  sliderInput("ktr6","KTR6", min = 0, max = 5, value = 1, step = 0.1, width = "100%"), 
  sliderInput("cl39","CL39", min = 0, max = 5, value = 1, step = 0.1, width = "100%")
)

simulation <- navset_card_underline(
    nav_panel(
      title = h2("Linear", style = "font-size: 20px;"),
      layout_columns(
        col_widths = c(4,4,4,12),
        div(
          tableOutput("rxode2")
        ),
        div(
          plotlyOutput("simlationplot3"),
          plotlyOutput("simlationplot4")
        ),
        div(
          plotlyOutput("simlationplot5"),
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
          plotlyOutput("simlationplotlog2")
        ),
        div(
          plotlyOutput("simlationplotlog3"),
          plotlyOutput("simlationplotlog4")
        ),
        div(
          plotlyOutput("simlationplotlog5"),
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
      add.sampling(seq(0, as.double(input$num)*as.double(input$interval)*24*30, by = 24))
  })
    
    rxsolve <- eventReactive(input$simulate, {
      rxSolve(model, c(LNDOSE = log(as.double(input$dose)), ktr3 = 0.0000060*as.double(input$ktr3),
                       ktr = 0.0001100*as.double(input$ktr), cl39 = 0.00000025*as.double(input$cl39),
                       ktr5 = 0.0111000*as.double(input$ktr5), ktr2 = 0.0008550 *as.double(input$ktr2),
                       ktr4 = 0.0023500 *as.double(input$ktr4), ktr6 = 0.0008470*as.double(input$ktr6)
                       ), dosing(), nSub = 1)
    })
    
    simulation_data <- reactive({
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
    })
    
    output$rxode2 <- renderTable({
      rxsolve()
    })
    
}



shinyApp(ui, server)