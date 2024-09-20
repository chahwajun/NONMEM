library(shiny)
library(tidyverse)
library(nonmem2rx)
library(bslib)
library(rxode2)

model <- nonmem2rx(file = "1_9.out")

sidebar <- card(
  card_title("Simulation Regimen"),
  checkboxGroupInput("regimen", label = NULL, choices = list("Single Dose"=1,"Multiple Dose"=2), selected = 1),
  conditionalPanel(
    condition = "input.regimen == 1",
    sliderInput("dose1", label = "Dose", step = 1, min=1, max = 100, value = 50, width = "100%")
  ),
  conditionalPanel(
    condition = "input.regimen == 2",
    sliderInput("dose2", label = "Dose", step = 1, min = 1, max = 100 , value = 50, width = "100%"),
    sliderInput("time2", label = "Time", step = 1, min = 1, max = 50, value = 50, width = "100%"),
    sliderInput("Interval2", label = "Interval", step = 1, min = 1, max = 100, value = 50, width = "100%")
  ),
  actionButton("simulate", label = "Simulate"),
  hr(),
  h6("Observation Input"),
  fileInput("obs",label = "Observation"),
  hr(),
  h6("NONMEM output file"),
  uiOutput("nonmem")
)
body <- card(
  card_title("Simulation Plot"),
  plotOutput("simlationplot")
)
ui <- page_navbar(
  title = "Simulation ",
  hr(),
  layout_columns(
    col_widths = c(3,9,12),
    sidebar,
    body
  ),
  hr()
)


server <- function(input, output, session) {
  
  output$nonmem <- renderUI({
    HTML(paste(capture.output(print(model)), collapse = "<br/>"))
    
  })
  
}

shinyApp(ui, server)
