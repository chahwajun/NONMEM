library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(rxode2)
library(DT)

source("model.R")
model <- rxode(model)

side <- card(
  downloadBttn(
    "report1",
    label = "Export Patients Report",
    style = "bordered"
  ),
  textInput("dfa", label = NULL)
)
input <- list(
  pickerInput("route2", label = "Route", choices = c("Intermittent IV-Injuection", "Continuous IV-Injection"),
              options = list(`live-search`= TRUE, title = "Select Drug Route")
  ),
  pickerInput("target", label = "Target", choices = c("Trough", "AUIC"),
              options = list(`live-search`= TRUE, title = "Select Drug Route")
              ),
  textInput("targettrough2",label = "Trough Concentration(mg/L)"),
  textInput("infusiontime2", label = "Infusion Time"),
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
                    textOutput("recommended_dose")
                  ),
                  nav_panel(
                    title = "Individual"
                  )
                )
              )
            )
          ),
          card(h5("Serum Level Graph"),
               plotOutput("population")
               )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            h5("Dosage History"),
            DTOutput("dosageTable"),
            actionButton("deleteRow2", label = "Delete Selected Row", width = "25%")
               ),
          card(
            h5("Serum Drug Level"),
            DTOutput("sampling2"),
            layout_columns(
              col_widths = c(3,3),
              actionButton("addRow2", label = "Add Concentration", width = "100%"),
              actionButton("deleteRow22", label = "Delete Concentration", width = "100%")
            )
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
    } else if(input$sex == "Female" && input$lab == "scr"){
      renal <- tibble(
        "A" = "Crcl",
        "B" = (140-current_age())*as.double(input$weight)/(72*as.double(input$cr_scr))*0.85
      )
    }
    
  })
  
  dosage_history <- reactiveVal(data.frame(
    DateTime = character(),
    Ctrough = numeric(),
    Infusion_Time= numeric(),
    Route = character(),
    Interval = numeric(),
    stringsAsFactors = FALSE
  ))
  

  observeEvent(input$report2, {
    if (!is.null(input$date2) && !is.null(input$interval2) && !is.null(input$infusiontime2)&& !is.null(input$route2)) {
      new_dose <- data.frame(
        DateTime = format(input$date2, "%Y-%m-%d %H:%M"),
        Ctrough = as.numeric(input$targettrough2),
        Infusion_Time = as.numeric(input$infusiontime2),
        Route = input$route2,
        Interval = input$interval2,
        stringsAsFactors = FALSE
      )
      

      current_history <- dosage_history()
      updated_history <- rbind(current_history, new_dose)
      updated_history <- updated_history[order(updated_history$DateTime), ]
      dosage_history(updated_history)
    }
  })
  

  observeEvent(input$deleteRow2, {
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
  
  recommended_dose <- reactiveVal(NULL) 
  
  observeEvent(input$report2, {
    req(input$targettrough2, input$infusiontime2, input$interval2)  
    
    target_trough <- as.numeric(input$targettrough2)
    infusion_time <- as.numeric(input$infusiontime2)
    interval <- as.numeric(input$interval2)
    
    init <- c(central = 0, peri = 0)
    params <- c(CL = 2.82, V1 = 31.8, Q = 11.7, V2 = 75.4, e.CL = 0, e.V2 = 0)
    
    result <- optim(
      par = 100,
      fn = function(dose) {
        sim <- model |> 
          rxSolve(
            params, 
            init, 
            events = et(amt = dose, dur = infusion_time) |> 
              add.sampling(seq(0, interval*2, by = 0.1)
                           )
          )
        
        
        if (nrow(sim) == 0) return(Inf)
        trough <- tail(sim$central, 1)
        return((trough - target_trough)^2)
      },
      method="L-BFGS-B"
    )
    
    if (result$convergence == 0) {
      recommended_dose(round(result$par, 2)) # Update reactive value
    } else {
      recommended_dose(NULL) # Reset if optimization fails
    }
  })
  
  output$recommended_dose <- renderText({
    if (!is.null(recommended_dose())) {
      paste("Recommended Dose:", recommended_dose(), "mg")
    } else {
      "Optimization failed. Please check inputs."
    }
  })


  output$population <- renderPlot({
    dose <- recommended_dose()
    
    if (!is.null(dose)) {
      target_trough <- as.numeric(input$targettrough2)
      infusion_time <- as.numeric(input$infusiontime2)
      interval <- as.numeric(input$interval2)
      
      init <- c(central = 0, peri = 0)
      params <- c(CL = 2.82, V1 = 31.8, Q = 11.7, V2 = 75.4, e.CL = 0, e.V2 = 0)
      
      sim <- model |> 
        rxSolve(
          params, 
          init, 
          events = et(amt = dose, dur = infusion_time) |> 
            add.sampling(seq(0, interval*2, by = 0.1))
        )
    
      sampling_data <- sampling_history() 
      
    
      ggplot(sim, aes(x = time, y = central)) +
        geom_line(color = "black", size = 1) + 
        geom_point(data = sampling_data, aes(x = Time, y = Concentration), color = "blue", size = 2) +
        geom_hline(yintercept = target_trough, linetype = "dashed", color = "red", size = 0.8) +
        theme_bw() +
        labs(title = "Population Concentration vs. Time with Sampling Data",x = "Time (hours)",y = "Concentration (mg/L)")
      
    } else {
      NULL
    }
  })
  
  
  
  sampling_history <- reactiveVal(data.frame(
    Time = numeric(),
    Concentration = numeric(),
    stringsAsFactors = FALSE
  ))
  
  output$sampling2 <- renderDT({
    datatable(
      sampling_history(),
      editable = TRUE, 
      selection = "none", 
      options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 15),
        ordering = FALSE,
        searching = FALSE
      )
    )
  })
  

  observeEvent(input$sampling2_cell_edit, {
    info <- input$sampling2_cell_edit 
    updated_table <- sampling_history() 
    updated_table[info$row, info$col] <- as.numeric(info$value)  
    sampling_history(updated_table)  
  })
  

  observeEvent(input$addRow2, {
    new_row <- data.frame(
      Time = NA,  
      Concentration = NA,
      stringsAsFactors = FALSE
    )
    updated_table <- rbind(sampling_history(), new_row)
    sampling_history(updated_table)
  })
  

  observeEvent(input$deleteRow22, {
    selected <- input$sampling2_rows_selected  
    if (!is.null(selected) && length(selected) > 0) {
      updated_table <- sampling_history()[-selected, ]
      sampling_history(updated_table)
    }
  })
  
}

shinyApp(ui, server)