library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(rxode2)
library(DT)
source("model.R")
side <- card(
  downloadBttn(
    "report1",
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
          col_widths = c(2,4,2,4,2,4,2,4,2,4),
          h3("First"),textInput("rirst", label = NULL),
          h3("Last"), textInput("last", label = NULL),
          h3("Birthdate"),airDatepickerInput("date1",label = NULL, view = c("days", "months", "years"),language = "ko"),
          h3("Age"), textOutput("current_age"),
          h3("Weight"), textInput("weight", label = NULL),
          h3("Height"), textInput("height", label = NULL)
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
      list(
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
                textInput("cr_scr", label = NULL, placeholder = "Enter Serum Concentration", width = "80%")
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
                    title = "Population",
                    actionBttn("report2", label = "Add Dose"),
                    
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
          card(
            h5("Dosage History"),
            DTOutput("dosageTable"),
            actionButton("deleteRow", label = "Delete Selected Row", width = "25%")
               ),
          card(
            h5("Serum Drug Level")
               )
        )
      )
    )
  ),
  
  nav_panel(
    title = "Steady State"
  )
)

server <- function(input, output, session) {
  
  current_age <- reactive({
    if (!is.null(input$date1)) {
      birth_date <- as.Date(input$date1)
      current_date <- Sys.Date()
      age <- as.numeric(difftime(current_date, birth_date, units = "weeks")) %/% 52
      return(age) 
    } else {
      return(NULL)
    }
  })
  
  output$current_age <- renderText({
    if (!is.null(current_age())) {
      paste("Age:", current_age())
    } else {
      NULL
    }
  })
  
  output$renal <- renderTable({
    
    if (input$sex == "Male" && input$lab == "scr") {
      renal <- tibble(
        "A" = "Crcl",
        "B" = (140-current_age())*as.double(input$weight)/(72*as.double(input$cr_scr))
      )
    } else{
      NULL
    }
  })
  
  dosage_history <- reactiveVal(data.frame(
    DateTime = character(),
    Dose = numeric(),
    Unit = character(),
    Route = character(),
    Interval = character(),
    stringsAsFactors = FALSE
  ))
  

  observeEvent(input$report2, {
    if (!is.null(input$date2) && !is.null(input$interval2)) {
      new_dose <- data.frame(
        DateTime = format(input$date2, "%Y-%m-%d %H:%M"),
        Dose = as.numeric(input$targettrough),
        Unit = "mg",
        Route = input$route,
        Interval = input$interval2,
        stringsAsFactors = FALSE
      )
      

      current_history <- dosage_history()
      updated_history <- rbind(current_history, new_dose)
      updated_history <- updated_history[order(updated_history$DateTime), ]
      dosage_history(updated_history)
    }
  })
  

  observeEvent(input$deleteRow, {
    selected <- input$dosageTable_rows_selected
    if (!is.null(selected)) {
      current_history <- dosage_history()
      updated_history <- current_history[-selected, ]
      dosage_history(updated_history)
    }
  })
  

  output$dosageTable <- renderDT({
    datatable(
      dosage_history(),
      editable = TRUE,
      selection = "single",
      options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 15),
        ordering = TRUE,
        searching = TRUE
      )
    )
  })
  

  observeEvent(input$dosageTable_cell_edit, {
    info <- input$dosageTable_cell_edit
    str(info) 
    
    updated_history <- dosage_history()
    updated_history[info$row, info$col] <- info$value
    dosage_history(updated_history)
  })
  
  observeEvent(input$report2, {
    
    if (!is.null(input$targettrough) && !is.null(input$interval2)) {

      theta <- list(clearance = 5, volume = 50)
      interval <- as.numeric(input$interval2) 
      target_ctrough <- as.numeric(input$targettrough) 
      
      model <- rxode(model)
      
      init <- c(central = 0)
      params <- c(clearance = theta$clearance, volume = theta$volume)
      
      # Simulate for the dosing interval
      
      sim <- model %>% rxSolve(params, init, events = eventTable() %>%
                                 add.dosing(dose = 100, nbr.doses = 1) %>% # Removed unused 'interval' argument
                                 add.sampling(seq(0, interval, by = 0.1)))
      
      # Find the dose that meets target Ctrough
      recommended_dose <- optim(
        par = 100, # Initial dose guess
        fn = function(dose) {
          sim <- model %>% rxSolve(params, init, events = eventTable() %>%
                                     add.dosing(dose = dose, nbr.doses = 1) %>% # Removed unused 'interval' argument
                                     add.sampling(seq(0, interval, by = 0.1)))
          trough <- tail(sim$cp, 1) # Last concentration (Ctrough)
          return((trough - target_ctrough)^2) # Minimize squared difference
        }
      )
      
      # Display recommended dose
      showModal(modalDialog(
        title = "Recommended Dose",
        paste("The recommended dose to achieve the target trough is:", round(recommended_dose$par, 2), "mg"),
        easyClose = TRUE
      ))
    }
  })
  
}

shinyApp(ui, server)