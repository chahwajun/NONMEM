library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(rxode2)
library(DT)
library(rmarkdown)

source("model.R")
source("function3.R")

side <- card(
  downloadBttn(
    "report1",
    label = "Export Patients Report",
    style = "bordered"
  ),
  uiOutput("dfa")
)
panel1 <- list(
  conditionalPanel(
    condition = "input.drug == 'Vancomycin'",  
    pickerInput("route2", label = "Route", choices = c("Intravenous Infusion"),
                options = list(`live-search`= TRUE, title = "Select Drug Route")
    ),
    pickerInput("target", label = "Target", choices = c("Trough"),
                options = list(`live-search`= TRUE, title = "Select Drug Route")
    ),
  ),
  conditionalPanel(
    condition = "input.drug == 'Cyclosporin'",  
    pickerInput("route3", label = "Route", choices = c("Intravenous Bolus", "Oral"),
                options = list(`live-search`= TRUE, title = "Select Drug Route")
    ),
    pickerInput("target", label = "Target", choices = c("Trough"),
                options = list(`live-search`= TRUE, title = "Select Drug Route")
    )
  )
)
panel2 <- list(
  conditionalPanel(
    condition = "input.drug == 'Vancomycin'",  
    textInput("targettrough2",label = "Dose (mg)"),
    textInput("time2",label = "Time (hr)"),
    textInput("infusiontime2", label = "Infusion Time")
  ),
  conditionalPanel(
    condition = "input.drug == 'Cyclosporin'",  
    textInput("targettrough2",label = "Dose (mg)"),
    textInput("time2",label = "Time (hr)")
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
          h3("First"),textInput("first", label = NULL),
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
        
        conditionalPanel(
          "input.drug == `Cyclosporin`",
          layout_columns(
            col_widths = c(2,4),
            h3("POD weeks"), textInput("POD",label = NULL)
          )
        )
        ,
        hr(),
        layout_columns(
          col_widths = c(2,4),
          "Drug", pickerInput("drug", label = NULL, choices = c("Vancomycin", "Cyclosporin"),
                              options = list(`live-search`= TRUE, title = "Select Drug")
          )
        ),
        
        conditionalPanel(
          condition = "input.drug =='Vancomycin'",  
          layout_columns(
            col_widths = c(2,4),
            "Route", pickerInput("route4", label = NULL, choices = c("Intravenous Infusion"),
                                 options = list(`live-search`= TRUE, title = "Select Drug Route") 
            )
          )
        ),
        conditionalPanel(
          condition = "input.drug == 'Cyclosporin'",  
          layout_columns(
            col_widths = c(2,4),
            "Route", pickerInput("route5", label = NULL, choices = c("Intravenous Bolus", "Oral"),
                                 options = list(`live-search`= TRUE, title = "Select Drug Route") 
            )
          )
        )
      ),
      list(
        card(
          h4("Lab Results"),
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
            ),
          ),
          conditionalPanel(
            "input.drug == `Cyclosporin`",
            layout_columns(
              col_widths = c(4,4,4),
              h3("Total Bilirubin"), textInput("bilirubin", label=NULL, width = "80%"), h3("mg/dl")
            )
          )
        ),
        card(
          h4("Model"),
          conditionalPanel(
            "input.drug == `Vancomycin`",
            layout_columns(
              col_widths = c(4,4),
              h3("Model"), pickerInput("model", label = NULL, choices = c("Bae 2019"))
            ),
          ),
          conditionalPanel(
            "input.drug == `Cyclosporin`",
            layout_columns(
              col_widths = c(4,4),
              h3("Model"), pickerInput("model", label = NULL, choices = c("Han 2019"))
            )
          )
        )
      ),
      card(
        h4("Population PK model"),
        verbatimTextOutput("model")
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
      col_widths = c(2, 10),
      side,
      list(
        layout_columns(
          col_widths = c(3, 9),
          card(
            h5("Therapeutic Target"),
            layout_columns(
              col_widths = c(4,4),
              list(
                h6("Input"),
                panel1,
                actionButton("report2", label = "Add Dose")
              ),
              list(
                panel2
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
    title = "Steady State",
    tableOutput("asd"),
    tableOutput("asf")
  )
)

server <- function(input, output, session) {
  
  model <- reactive({
    if (input$drug == "Vancomycin") {
      vancomycin
    } else if (input$drug == "Cyclosporin") {
      cyclosporin
    } else {
      stop("Select the drug")  
    }
  })
  
  observe({
    req(input$drug == "Cyclosporin")  
    
    updatePickerInput(session, "route5",
                      choices = c("Intravenous Bolus", "Oral"))
    updatePickerInput(session, "route3",
                      choices = c("Intravenous Bolus", "Oral"))
  })
  
  
  observeEvent(input$route5, {
    req(input$drug == "Cyclosporin")  
    
    updatePickerInput(session, "route3",
                      selected = input$route5,
                      choices = c("Intravenous Bolus", "Oral"))
  }, ignoreInit = FALSE)
  
  
  observe({
    req(input$drug == "Vancomycin")  
    
    updatePickerInput(session, "route4", 
                      choices = c("Intravenous Infusion"),
                      selected = "Intravenous Infusion")
    updatePickerInput(session, "route2", 
                      choices = c("Intravenous Infusion"),
                      selected = "Intravenous Infusion")
  })
  
  
  patient_info <- reactive({
    list(
      FirstName = input$first,
      LastName = input$last,
      Birthdate = input$date1,
      Age = current_age(),
      Weight = input$weight,
      Height = input$height,
      Sex = input$sex,
      PatientID = input$patient
    )
  })
  
  # 2. RMarkdown 파일 생성 및 렌더링
  output$dfa <- renderUI({
    req(patient_info())  # patient_info가 있어야 실행
    info <- patient_info()
    current_dir <- getwd()
    
    # RMarkdown 파일 내용 생성
    rmd_content <- c(
      "---",
      "title: 'Patient Report'",
      "output: html_fragment",
      "---",
      "",
      "# Patient Information",
      paste0("- **First Name**: ", info$FirstName),
      paste0("- **Last Name**: ", info$LastName),
      paste0("- **Birthdate**: ", info$Birthdate),
      paste0("- **Age**: ", info$Age),
      paste0("- **Weight**: ", info$Weight, " kg"),
      paste0("- **Height**: ", info$Height, " cm"),
      paste0("- **Sex**: ", info$Sex),
      paste0("- **Patient ID**: ", info$PatientID)
    )
    
    # 임시 .Rmd 파일 생성
    temp_file <- file.path(tempdir(), "Temporary.Rmd")
    writeLines(rmd_content, con = temp_file)
    
    # .Rmd 파일 렌더링
    tryCatch({
      # HTML로 렌더링
      rmarkdown::render(temp_file, output_format = "html_fragment", quiet = TRUE)
      
      # 렌더링된 HTML 파일 읽기
      html_file <- paste0(tools::file_path_sans_ext(temp_file), ".html")
      table_html <- readLines(html_file, warn = FALSE)
      
      # HTML 출력
      HTML(table_html)
    }, error = function(e) {
      # 에러 메시지를 HTML로 출력
      HTML(paste("Error occurred while rendering Markdown:", e$message))
    })
  })
  
  # 3. Markdown 파일 다운로드
  output$report1 <- downloadHandler(
    filename = function() {
      paste0("Patient_Report_", Sys.Date(), ".md")
    },
    content = function(file) {
      info <- patient_info()
      report_content <- paste(
        "# Patient Information\n",
        "- **First Name**: ", info$FirstName, "\n",
        "- **Last Name**: ", info$LastName, "\n",
        "- **Birthdate**: ", info$Birthdate, "\n",
        "- **Age**: ", info$Age, "\n",
        "- **Weight**: ", info$Weight, " kg\n",
        "- **Height**: ", info$Height, " cm\n",
        "- **Sex**: ", info$Sex, "\n",
        "- **Patient ID**: ", info$PatientID, "\n"
      )
      writeLines(report_content, file)
    }
  )
  
  
  output$model <- renderPrint({
    
    model()
  })
  
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
  
  dosage_history <- reactiveVal(
    tibble(
      Dose = numeric(),
      Time = numeric(),
      Infusion_Time= numeric(),
      Route = character()
    )
  )
  
  
  
  observeEvent(input$report2, {

    if(input$drug == "Vancomycin") {
        
        new_dose <- tibble(
          Dose = as.numeric(input$targettrough2),
          Time = as.numeric(input$time2),
          Infusion_Time = as.numeric(input$infusiontime2),
          Route = input$route2
        )
      
    } else if(input$drug == "Cyclosporin") {
        new_dose <- tibble(
          Dose = as.numeric(input$targettrough2),
          Time = as.numeric(input$time2),
          Infusion_Time = NA,
          Route = input$route3
        )
    }
    
    # Get current history and append new dose
    if(exists("new_dose")) {
      current_history <- dosage_history()
      updated_history <- rbind(current_history, new_dose)
      dosage_history(updated_history)
    }
  }, ignoreInit = TRUE)
  
  # Add input validation and reset
  observeEvent(input$report2, {
    # Clear or reset input fields after successful addition
    updateTextInput(session, "targettrough2", value = "")
    updateTextInput(session, "infusiontime2", value = "")
    updateTextInput(session, "interval2", value = "")
  }, ignoreInit = TRUE)
  
  observeEvent(input$deleteRow2, {
    selected <- input$dosageTable_rows_selected
    if (!is.null(selected)) {
      current_history <- dosage_history()
      updated_history <- current_history[-selected, ]
      dosage_history(updated_history)
    }
  })
  
  output$dosageTable <- renderDT({
    
    history <- dosage_history()
    if (nrow(history) == 0) {
      return(datatable(
        data.frame(
          Dose = numeric(),
          Infusion_Time = numeric(),
          Route = character(),
          Interval = numeric(),
          stringsAsFactors = FALSE
        ),
        editable = TRUE,
        selection = "single",
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15),
          ordering = TRUE,
          searching = TRUE
        )
      ))
    }
    
    if(input$drug == "Cyclosporin") {
      history$Infusion_Time <- NULL
    }
    
    datatable(
      history,
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
  
  
  
  
  
  sampling_history <- reactiveVal(
    tibble(
    Time = numeric(),
    Concentration = numeric()
  ))
  
  output$sampling2 <- renderDT({
    datatable(
      sampling_history(),
      editable = TRUE, 
      selection = "single", 
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
  
  sim <- reactiveVal(NULL)
  
  dosing <- reactive({    
    history <- dosage_history()
    
    dose <- et(amountUnits = "mg",timeUnits = "hours")
    
    if(nrow(history) > 0) {
      for(i in 1:nrow(history)) {
        dose <- dose |> 
          et(dose = history$Dose[i],
             time = history$Time[i],
             dur = ifelse(!is.na(history$Infusion_Time[i]), 
                           history$Infusion_Time[i], 
                           NULL)
          )
      }
      dose <- dose |> 
        add.sampling(seq(0, max(24, max(history$Time) * 2), by = 0.1))
    }
    return(dose)  
  })
  sim_optim <- reactiveVal(NULL) 
  
  observeEvent(input$report2, {
    params <- c(CL = 2.82, V1 = 31.8, Q = 11.7, V2 = 75.4, e.CL = 0, e.V2 = 0)
    
    current_model <- model()
    sim_result <- current_model |> 
      rxSolve(
        params, 
         dosing()
      )
    sim(sim_result)
  })

  
  sim_optim <- reactiveVal(NULL)
  
  sim_optim <- reactiveVal(NULL)
  
  observeEvent(sampling_history(), {
    if (nrow(sampling_history()) > 0 && 
        !any(is.na(sampling_history()$Time)) && 
        !any(is.na(sampling_history()$Concentration))) {
      
      # observation 데이터 준비
      y <- sampling_history()$Concentration/1000  # mg/L로 변환
      pointtime <- sampling_history()$Time
      
      mapb2 <- function(eta) {
        out <- model() |> 
          rxSolve(
            c(t.CL = 2.82,
              t.V1 = 31.8,
              t.Q = 11.7,
              t.V2 = 75.4,
              e.CL = eta[1],
              e.V2 = eta[2],
              Additive = 1e-06,
              Proportional = 0.284),
            dosing()
          ) |>
          as_tibble() |>
          mutate(DV = central/V1)
        
        eta <- c(eta[1], eta[2])
        eta_m <- unlist(matrix(eta, nrow = 2))
        sig2 <- 0.253^2
        
        # 각 관측시점마다 residual 계산
        residuals <- numeric(length(pointtime))
        for(i in 1:length(pointtime)) {
          sig2j <- (subset(out$DV, out$time == pointtime[i]))^2 * sig2
          residuals[i] <- log(sig2j) + (1/sig2j) * (y[i] - subset(out$DV, out$time == pointtime[i]))^2
        }
        sqwres <- sum(residuals)
        
        omega <- matrix(c(0.685, 0, 0, 0.216), 2, 2)
        omega.inv <- solve(omega)
        nOn <- diag(t(eta_m) %*% omega.inv %*% eta_m)
        
        return(sqwres + nOn)
      }
      
      # 참고코드와 같은 초기값 사용
      ini <- c(0.1, 0.1)
      FIT <- optim(ini, mapb2, method = "L-BFGS-B", control = list(trace=1, REPORT=1))
      
      params_optim <- c(
        t.CL = 2.82,
        t.V1 = 31.8,
        t.Q = 11.7,
        t.V2 = 75.4,
        e.CL = FIT$par[1],
        e.V2 = FIT$par[2],
        Additive = 1e-06,
        Proportional = 0.284
      )
      
      sim_result <- model() |> 
        rxSolve(params_optim, dosing())
      sim_optim(sim_result)
    }
  })
  
  output$population <- renderPlot({
    dosage <- dosage_history()
    sampling <- sampling_history() 
    sim_data <- sim()
    sim_optim_data <- sim_optim()
    req(sim_data)
    
    df <- as_tibble(sim_data)
    p <- ggplot() +  
      geom_line(data = df, aes(x = time, y = central/V1), color = "blue") +
      theme_bw() +
      labs(x = "Time (hours)", 
           y = "Vancomycin Concentration (mg/L)") + 
      geom_hline(yintercept = 15, color = "darkorange3") + 
      geom_hline(yintercept = 40, color = "darkorange2") + 
      scale_x_continuous(breaks = seq(0, max(24,dosage$Time*2), 12))
    
    if (!is.null(sim_optim_data)) {
      df_optim <- as_tibble(sim_optim_data)
      p <- p + geom_line(data = df_optim, aes(x = time, y = central/V1), color = "red")
    }
    
    if (nrow(sampling) > 0) {
      p <- p + geom_point(data = sampling, 
                          aes(x = as.numeric(Time), y = as.numeric(Concentration)),
                          color = "black", 
                          size = 3)
    }
    
    p
  })
  
  output$asd <- renderTable({
    sim_optim()
  })
  output$asf <- renderTable({
    sim()
  })
}

shinyApp(ui, server)