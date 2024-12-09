library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(rxode2)

side <- card(
  downloadBttn(
    "report",
    label = "Export Patients Report",
    style = "bordered"
  )
)

ui <- page_navbar(
  title = "DEMO",
  nav_panel(
    includeCSS("tdm.css"),
    title = "Patient Info",
    layout_columns(
      col_widths = c(6,6,6,6),
      card(
        h5("Patients"),
        layout_columns(
          col_widths = c(2,4,2,4,2,4),
          h3("First"),textInput("rirst", label = NULL),
          h3("Last"), textInput("last", label = NULL),
          h3("Birthdate"),airDatepickerInput("date",label = NULL, view = c("days", "months", "years"))
        ),
        layout_columns(
          col_widths = c(2,4,2,4),
          h3("Patient ID"),textInput("patient",label = NULL),
          h3("Sex"),radioGroupButtons("sex",label = NULL,choices = c("Male", "Female"))
        ),
        hr(),
        layout_columns(
          col_widths = c(2,4),
          "Drug", pickerInput("drug", label = NULL, choices = c("Vancomycin"),
                              options = list(`live-search`= TRUE, title = "Select Drug")
                              )
        ),
        layout_columns(
          col_widths = c(2,4),
          "Route", pickerInput("route", label = NULL, choices = c("Intermittent IV-Injuection", "Continuous IV-Injection"),
                               options = list(`live-search`= TRUE, title = "Select Drug Route")
                              )
        )
      ),
      card(
        h4("Lab Results")
      ),
      card(
        h4("Population PK"),

      ),
      card(
        h5("Renal Function")
      ),
    )
  ),
  nav_panel(
    title = "Drug Monitering",
    layout_columns(
      col_widths = c(2,5,5),
      side,
      card(),
      card()
    )
  ),
  nav_panel(
    title = "Steady State"
  )
      
)

server <- function(input, output, session){
  
  
  
}


shinyApp(ui, server)