library(shiny)
library(tidyverse)
library(bslib)
library(mrgsolve)


sidebar <- card()
ui <- page_navbar(
  title = "DEMO",
  hr(),
  nav_panel(
    title = "Patients Info",
    layout_columns(
      col_widths = c(6, 6,12),
      card(
        h4("Patients"),
        layout_columns(
          col_widths = c(6,6,12),  # 세 요소의 너비를 조정
          textInput("first", "First"),
          textInput("last", "Last")
        ),
        dateInput("birth", "Birth", format = "yyyy-mm-dd", startview = "year"),
      ),
      layout_columns(
        col_widths = c(6,6,12),
        textInput("id", "Patients ID"),
        
      )
      
    )
  ),
  
  nav_panel(
    title = "Drug Monitoring",
    sidebar
  ),
  nav_panel(
    title = "Steady State"
  )
)

server <- function(input, output, session) {
  
  
  
}

shinyApp(ui, server)
