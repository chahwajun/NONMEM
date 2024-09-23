library(shiny)
library(bslib)
library(rxode2)
library(tidyverse)
library(ggplot2)
library(NonCompart)
library(plotly)
library(markdown)
library(bsicons)
library(psych)
library(nonmem2rx)



zolpidem_rxode2 <- nonmem2rx(
  file = "ZPD_101101E01_MFDS_1.OUT"
)




dosing_info <-  card(
  card_title("Dosage Regimens"),
  value_box(
    "Total Dosage of Zolpidem(mg)",
    value = textOutput("Total_dose"),
    max_height = "150px",
    fill = FALSE,
    showcase = icon("capsules", style = "font-size: 50px;")
    
    
  ),
  
  tags$hr(),
  card(
    radioButtons("Drug_name", "Strength of Tablet",
                 choices = list( "5mg" = 1, "10mg" = 2 )
    ),
    sliderInput("Dose1", "Number of Tablet", min = 0, max = 200, value = 10, step = 1, width = "100%"),
    fill = FALSE
  ),
  
  sliderInput("Simulation", "Number of Simulation", min = 100, max = 1000, value = 0, step = 50, width = "100%"),
  actionButton("Button", "Simulate")
)






ui <- page_navbar(
  title = tags$span("Zolpidem Simulator", style = "color: white;"), 
  bg = "#337AB7",
  nav_panel(
    title = tags$span("Dosage Regimen", style = "color: white;"),
    icon = bs_icon("clipboard2-data", style = "color: white;"),
    h2(" Zolpidem Simulation",align='left',style="color:#dbdbdb; font-weight:bold"),
    tags$hr(),
    
    layout_columns(
      col_widths = c(2,8,2,12),
      dosing_info,
      card(
        card_title("Simulation Plot"),
        plotlyOutput("current")
      ),
      card(
        card_title("Summary of Zolpidem Simulation"),
        div(
          card(
            "Predicted Parameter",
            tableOutput("frontparameter1"),
            tableOutput("frontparameter2")
          ),
          
          value_box(
            "Current Zolpidem Concentration(μg/L)",
            value = textOutput("current_concentration"),
            showcase = icon("chart-line", style = "font-size: 50px;")
          ),
          card(
            card_title("Duration of Toxic Concentraion"),
            tableOutput("duration_front")
          )
        )
      )
    )
    
  ),
  
  nav_panel(
    title = tags$span("Simulation Results", style = "color: white;"),
    icon = icon("table-list", style = "color: white;"),
    h2("Simulation Results",align='left',style="color:#dbdbdb; font-weight:bold"),
    tags$hr(),
    
    navset_card_underline(
      nav_panel(
        "Simulation Results",
        card(
          
          card_body(
            card_title("Descriptive Statistics of PK Parameters"),
            layout_columns(
              col_widths = c(7,5,12),
              card(
                tableOutput("parameter"),
                tableOutput("nca_parameter")
                
              ),
              includeMarkdown("Parameters_info.md")
            )
          ),
          card_body(
            card_title("Duration of Toxic Concentration(h)"),
            layout_columns(
              col_widths = c(7,5,12),
              card(
                tableOutput("duration1"),
                tableOutput("duration2")
                
              )
            )
            
          )
        )
        
      ),
      nav_panel(
        "Individual Simulation Results",
        card(
          
          card_body(
            card_title("Individual PK Parmeters"),
            card(
              dataTableOutput("iparametertable"),
              dataTableOutput("nca_table")
            )
          ),
          
          card_body(
            card_title("Individual PK Profile"),
            card(
              plotOutput("individual_graph"),
              full_screen = TRUE
            )
          )
        )
      )
    )
    
  ),
  
  nav_panel(
    title = tags$span("About", style = "color: white;"),
    icon = icon("circle-info", style = "color: white;"),
    
    h2("About",align='left',style="color:#dbdbdb; font-weight:bold"),
    tags$hr(),
    includeMarkdown("About.md"),
    
    h2("User Instructions",align='left',style="color:#dbdbdb; font-weight:bold"),
    tags$hr(),
    includeMarkdown("Instructions.md"),
    
    h2("Authors",align='left',style="color:#dbdbdb; font-weight:bold"),
    tags$hr(),
    includeMarkdown("Authors.md"),
    
    h2("Reference",align='left',style="color:#dbdbdb; font-weight:bold"),
    tags$hr(),
    includeMarkdown("Reference_about.md"),
    
  )
)



server <- function(input, output, session){
  
  
  zolpidem_dosing <- eventReactive(input$Button, {
    
    zolpidem_dosing <- eventTable(
      amount.units = "mg",
      time.units = "hr"
    ) |>
      add.dosing(
        case_when(
          input$Drug_name == 1       ~ (dose = as.double(input$Dose1) * 5),
          input$Drug_name == 2       ~ (dose = as.double(input$Dose1) * 10)
        )
      )|> 
      add.sampling(c(seq(0,3 , by = 1/12), seq(3, 24, by = 0.5)))
    
    
  })
  
  
  
  zolpidem_rxsolve <- eventReactive(input$Button,{
    
    
    rxSolve(zolpidem_rxode2, zolpidem_dosing(), nSub = as.double(input$Simulation)
    )
  })
  
  zolpidem_parameter <- eventReactive(input$Button,{
    
    zolpidem_rxsolve() |> 
      select(sim.id, ka, vc, cl,  k, time, ipred) |> 
      rename(Ka = ka, `Vc/F` = vc, `CL/F` = cl, Ke = k)
    
    
    
  })
  
  current <- eventReactive(input$Button, {
    
    zolpidem_min_max <- zolpidem_rxsolve() |> 
      group_by(time) |> 
      mutate(ipred_max = quantile(ipred, 0.975),
             ipred_min = quantile(ipred, 0.025)
      )
    
    zolpidem_median <- zolpidem_rxsolve() |> 
      group_by(time) |> 
      mutate(ipred = median(ipred))
    
    zolpidem_graphs <- ggplot() + 
      geom_hline(aes(yintercept = 4000, color = "Fatal Concentration: 4000ng/mL")) +
      geom_hline(aes(yintercept = 1500, color = "Comatose Concentration: 1500ng/mL")) +
      geom_hline(aes(yintercept = 500, color = "Toxic Concentration: 500ng/mL")) +
      geom_hline(aes(yintercept = 200, color = "Therapeutic Concentration: 200ng/mL")) +
      geom_line(data = zolpidem_median, aes(x = time, y = ipred, color = "Median Concentration(ng/mL)")) + 
      geom_ribbon(data = zolpidem_min_max, aes(x = time, ymin = ipred_min, ymax = ipred_max), alpha = 0.4, fill = "#89CFF0") + 
      labs(x = "Time (hours)", y = "Plasma concentration(ng/mL)") + 
      scale_color_manual(values = c("Fatal Concentration: 4000ng/mL" = "red",
                                    "Comatose Concentration: 1500ng/mL" = "orange", 
                                    "Toxic Concentration: 500ng/mL" = "gold", 
                                    "Therapeutic Concentration: 200ng/mL" = "grey50", 
                                    "Median Concentration(ng/mL)" = "#337AB7"),
                         guide = guide_legend("Reference Range")
      ) +
      
      theme_minimal(base_size = 14) +
      theme(legend.position = "right",
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12)) + 
      scale_x_continuous(breaks = seq(0, 24, by = 2))
    
  })
  nca <- eventReactive(input$Button,{
    
    
    if (input$Drug_name == 1) {
      zolpidem_rxsolve() |> 
        select(sim.id, time, ipred) |> 
        tblNCA(key = "sim.id", colTime = "time", colConc = "ipred", dose = (5*input$Dose1), adm = "Extravascular", doseUnit = "mg",
               timeUnit = "h", concUnit = "ug/L", R2ADJ = -1
        ) |> round(digits=2) 
    }
    
    else if (input$Drug_name == 2) {
      zolpidem_rxsolve() |> 
        select(sim.id, time, ipred) |> 
        tblNCA(key = "sim.id", colTime = "time", colConc = "ipred", dose = (10*input$Dose1), adm = "Extravascular", doseUnit = "mg",
               timeUnit = "h", concUnit = "ug/L", R2ADJ = -1
        )|> round(digits=2) 
    }
  })
  
  duration <- eventReactive(input$Button, {
    
    zolpidem_rxsolve() |> 
      select(sim.id, time, ipred) |> 
      mutate(ipred_500 = case_when(
        ipred<500            ~ 0,
        ipred>=500 & time<=3 ~ 1/12,
        ipred>=500 & time>3  ~ 0.5
        
      ),
      
      
      ipred_1500 = case_when(
        ipred<1500            ~ 0,
        ipred>=1500 & time<=3 ~ 1/12,
        ipred>=1500 & time>3  ~ 0.5
      ),
      
      
      ipred_4000 = case_when(
        ipred<4000            ~ 0,
        ipred>=4000 & time<=3 ~ 1/12,
        ipred>=4000 & time>3  ~ 0.5
      )
      )|>
      group_by(sim.id) |> 
      summarize(toxic = sum(ipred_500), 
                comatose = sum(ipred_1500),
                fatal = sum(ipred_4000),
      ) 
  })
  
  
  
  output$current <- renderPlotly({
    current()
  })
  
  
  output$frontparameter1 <- renderTable({
    
    zolpidem_parameter() |> 
      distinct(sim.id, .keep_all = TRUE) |> 
      select(-sim.id, -time, -ipred) |> 
      summarise_all(median) 
    
    
  })
  
  output$duration1 <- renderTable({
    
    duration() |> 
      describe() |> 
      select(median, min,max, sd) |> 
      slice(-1) |> 
      mutate(`Reference Range`= c('Toxic Concentration (500ng/mL)','Comatose Concentration (1500ng/mL)', 'Fatal Concentration (4000ng/mL)'), .before = 1)
    
  })
  
  output$duration2 <- renderTable({
    duration_table <- duration()
    
    tibble(
      "Duration of Concentration" = c("Not Reached","0h to 1h(%)","1h to 2h(%)","2h to 3h(%)","More than 3h(%)"),
      "Toxic Concentration (500ng/mL)" =c(
        sum(duration_table$toxic == 0) / nrow(duration_table) * 100,  
        sum(duration_table$toxic  < 1 & duration_table$toxic > 0) / nrow(duration_table) * 100,  
        sum(duration_table$toxic >= 1 & duration_table$toxic < 2) / nrow(duration_table) * 100,  
        sum(duration_table$toxic >= 2 & duration_table$toxic < 3) / nrow(duration_table) * 100,  
        sum(duration_table$toxic >= 3) / nrow(duration_table) * 100  
      ),
      'Comatose Concentration (1500ng/mL)' =c(
        sum(duration_table$comatose == 0) / nrow(duration_table) * 100, 
        sum(duration_table$comatose < 1  & duration_table$comatose > 0) / nrow(duration_table) * 100,  
        sum(duration_table$comatose >= 1 & duration_table$comatose < 2) / nrow(duration_table) * 100,  
        sum(duration_table$comatose >= 2 & duration_table$comatose < 3) / nrow(duration_table) * 100,  
        sum(duration_table$comatose >= 3) / nrow(duration_table) * 100  
      ),
      'Fatal Concentration (4000ng/mL)'=c(
        sum(duration_table$fatal == 0) / nrow(duration_table) * 100, 
        sum(duration_table$fatal < 1& duration_table$fatal > 0) / nrow(duration_table) * 100,  
        sum(duration_table$fatal >= 1 & duration_table$fatal < 2) / nrow(duration_table) * 100,  
        sum(duration_table$fatal >= 2 & duration_table$fatal < 3) / nrow(duration_table) * 100,  
        sum(duration_table$fatal >= 3) / nrow(duration_table) * 100  
      )
    )
    
  })
  
  
  output$duration_front <- renderTable({
    
    duration()|> 
      describe() |> 
      select(median, min,max, sd) |> 
      slice(-1) |> 
      select(1) |> 
      mutate(`Reference Range`= c('Toxic Concentration(500ng/mL)','Comatose Concentration(1500ng/mL)', 'Fatal Concentration(4000ng/mL)'), .before = 1) |> 
      rename(`Median Time (h)`= median)
    
  })
  
  
  output$frontparameter2 <- renderTable({
    
    nca() |> 
      select(CMAX, TMAX, LAMZHL, AUCLST)|> 
      summarise_all(median) 
    
    
    
    
  })
  
  
  
  output$individual_graph <- renderPlot({
    zolpidem_rxsolve() |> 
      ggplot() + geom_line(aes(x = time, y = ipred, group = sim.id), alpha = 0.1) +geom_hline(yintercept = 500, color ="yellow") +
      geom_hline(yintercept = 4000, color = "red") +
      geom_hline(yintercept = 200, color = "black") +
      geom_hline(yintercept = 1500 , color = "darkorange") +
      labs(x = "Time (h)",y = "Plasma concentration of zolpidem (ng/mL)",color = NULL) +
      theme_minimal() + scale_x_continuous(breaks = seq(0,24, by = 4))+ theme(axis.title = element_text(size = 20), axis.text = element_text(size = 20))
  })
  
  output$iparametertable <-  renderDataTable({
    
    
    zolpidem_parameter() |> 
      distinct(sim.id, .keep_all = TRUE) |> 
      select(-time, -ipred) |> 
      round(digits = 3)
    
  }, options = list(pageLength = 10))
  
  
  output$parameter <- renderTable({
    
    table <- zolpidem_parameter() |> 
      select(-time, -ipred) |> 
      distinct(sim.id, .keep_all = TRUE) |> 
      select(-sim.id) |> 
      describe() |> 
      select(median, min, max, mean, sd)
    
    df <- tibble(
      "Parameters" = c("Ka", "Vc/F", "CL/F", "Ke")
    )
    print(bind_cols(df, table))
    
  })
  
  output$nca_parameter <- renderTable({
    nca_parameter <- nca() |> 
      select(CMAX, TMAX, LAMZHL, AUCLST) |>
      rename(Cmax = CMAX, Tmax = TMAX, `Half life`= LAMZHL, AUC = AUCLST) |> 
      describe() |> 
      select(median, min, max, mean, sd)
    
    
    df2 <- tibble(
      "Parameters" = c("Cmax", "Tmax", "LAMZHL", "AUC")
    )
    print(bind_cols(df2, nca_parameter))
  })
  
  
  output$nca_table <- renderDataTable({
    
    print(nca())
    
  },options = list(pageLength = 10))
  
  
  output$Total_dose <- renderText({
    
    if (input$Drug_name == 1 )  {
      total_doses <- input$Dose1  * 5
    } 
    
    else if (input$Drug_name == 2) {
      total_doses <- input$Dose1  * 10
    } 
    
    paste(total_doses, "mg")
    
    
  })  
  
  
  
  
  
  output$current_concentration <- renderText({
    event_register(current(), 'plotly_click')  
    event_data <- event_data("plotly_click", source = "A")  
    if (is.null(event_data) || nrow(event_data) == 0) {
      return(NULL)
    }
    x_value <- event_data$x
    y_value <- event_data$y  
    text <- paste( round(y_value, digits = 2), "μg/L")
    return(text)
  })
  
  
  
  
}

shinyApp(ui,server)