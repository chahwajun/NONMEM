library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(rxode2)

source("model.R")
model <- rxode(model)

side <- card(
  downloadBttn(
    "report",
    label = "Export Patients Report",
    style = "bordered"
  ),
  textInput("dfa", label = NULL)
)
input <- list(
  pickerInput("route", label = "Route", choices = c("Intermittent IV-Injuection", "Continuous IV-Injection"),
              options = list(`live-search`= TRUE, title = "Select Drug Route")
  ),
  pickerInput("target", label = "Target", choices = c("Trough", "AUIC"),
              options = list(`live-search`= TRUE, title = "Select Drug Route")
              ),
  textInput("targettrough",label = "Desired Trough (ng/mL)"),
  textInput("interval2", label = "Interval (hr)"),
  airDatepickerInput("date2",label = "Next Dose Time", view = c("days", "months", "years"), language = "ko",timepicker = TRUE
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
          h3("Birthdate"),airDatepickerInput("date1",label = NULL, view = c("days", "months", "years"),language = "ko")
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
        card(
          h4("Lab Results"),
          h5("Calculate Renal Function Using"),
          layout_columns(
            col_widths = c(4, 4), 
            radioButtons(
              "lab",
              label = NULL,
              choices = c("Scr" = "scr", "Exact CrCl" = "crcl"),
              selected = "scr"
            ),
            div(
              class = "input-row",
              conditionalPanel(
                condition = "input.lab == 'scr'",
                textInput("cr_scr", label = NULL, placeholder = "Enter Scr value", width = "80%")
              ),
              conditionalPanel(
                condition = "input.lab == 'crcl'",
                textInput("cr_crcl", label = NULL, placeholder = "Enter CrCl value", width = "80%")
              ),
              conditionalPanel(
                condition = "input.lab == 'scr'",
                h3("mg/dL")
              ),
              conditionalPanel(
                condition = "input.lab == 'crcl'",
                h3("mL/min")
              )
            )
          )
        ),
        card(
          h4("Model"),
          layout_columns(
            col_widths = c(2,4,4,10),
            h3("Model"), pickerInput("model", label = NULL, choices = c("Bae 2019"))
          )
        )
      ),
      card(
        h4("Population PK"),
        
      ),
      card(
        h5("Renal Function"),
        tableOutput("renal")
      )
    )
  ),
  nav_panel(
    title = "Drug Monitering",
    layout_columns(
      fillable = TRUE, 
      col_widths = c(3, 9),
      side,
      list(
        layout_columns(
          col_widths = c(6, 6),
          card(
            h5("Therapeutic Target"),
            layout_columns(
              col_widths = c(4,4),
              list(
                h6("Input"),
                input
              ),
              list(
                h6("Dosing Recommendation"),
                navset_underline(
                  nav_panel(
                    title = "Population"
                  ),
                  nav_panel(
                    title = "Individual"
                  )
                )
              )
            )
          ),
          card(h5("Serum Level Graph"))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(h5("Dosage History")),
          card(h5("Serum Drug Level"))
        )
      )
    )
  ),
  
  nav_panel(
    title = "Steady State"
  )
)

server <- function(input, output, session){
  
  output$renal <- renderTable({
    if (input$lab == "scr" && input$sex == "Male"){
      
    }
  })
  
  
}


shinyApp(ui, server)