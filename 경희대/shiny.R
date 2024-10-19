library(shiny)
library(bslib)
library(tidyverse)
library(NonCompart)
library(nonmem2rx)
library(rxode2)
library(markdown)

model1 <- nonmem2rx("final_backward5.lst")
model2 <- nonmem2rx("FINAL.out")

sidebar1 <- card(
  sliderInput("dose1", "Dose (mg)",min = 0, max = 100, step = 1, value = 80, width = "100%"),
  hr(),
  h4("Covariates"),
  textInput("ptn1","Total Protein (g/dL)", width = "100%", value = 6.5),
  textInput("weight1", " Weight (kg)", width = "100%", value = 63.3),
  actionButton("sim1","Simulate")
)
simulation1 <- card(
  h3("Simulation Plot", style = "font-size: 20px;"),
  plotOutput("plot1")
)
result1 <- card(
  h3("Result", style = "font-size: 20px;"),
  tableOutput("table1"),
  hr(),
  h3("XPOVIO® FDA Label Section 12.3 Pharmacokinetics", style = "font-size: 16px; font-weight: bold;"),
  includeMarkdown("Selinexor.md"),
  tableOutput("selinexor")
)
sidebar2 <- card(
  sliderInput("dose2", "Dose (mg)", min = 0, max = 25, step = 0.5, value = 5, width = "100%"),
  sliderInput("num2", "Number of Dose", min = 0, max = 5, step = 1, value = 3, width = "100%"),
  hr(),
  h4("Covariates"),
  selectInput("sex2", "Sex", choices = list("Male"=1, "Female"=2), width = "100%"),
  textInput("height2", "Height (cm)", width = "100%", value = 162),
  textInput("weight2", "Weight (kg)", width = "100%", value = 62.6),
  sliderInput("age2","Age (years)", min = 20, max = 100, value = 50, width = "100%"),
  textInput("scr2","Serum Creatinine (mg/dL)", width = "100%", value = 1.77),
  textInput("alb2", "Albumin Concentration(g/dL)", width = "100%", value = 4.2),
  actionButton("sim2","Simulate")
)

simulation2 <- card(
  h3("Simulation Plot", style = "font-size: 20px;"),
  plotOutput("plot2")
)
result2 <- card(
  h3("Calculated Covariates", style = "font-size: 20px;"),
  tableOutput("table2"),
  hr(),
  h3("Result", style = "font-size: 20px;"),
  tableOutput("table22"),
)


ui <- page_fillable(
  h2("Personalized Dose Regimen for Selinexor and Lenalidomide"),
  hr(),
  navset_card_underline(
    nav_panel(
      title = h3("Selinexor",style = "font-size: 20px;"),
      layout_columns(
        col_widths = c(3,6,3),
        sidebar1,simulation1, result1
      )

    ),
    nav_panel(
      title = h3("Lenalidomide",style = "font-size: 20px;"),
      layout_columns(
        col_widths = c(3,6,3),
        sidebar2,simulation2, result2
      )
    )
  ),
  hr()
)

server <- function(input, output, session) {
  
  dosing1 <- eventReactive(input$sim1,{
    eventTable(amount.units = "mg", time.units = "h") |> 
      add.dosing(dose = as.double(input$dose1)) |> 
      add.sampling(seq(0,24, 0.1))
  })
  
  rxsolve1 <- eventReactive(input$sim1,{
    rxSolve(model1, nsim= 500, dosing1(), c(WT = as.double(input$weight1), PTN = as.double(input$ptn1)))
  })
  
  sim1 <- reactive({
    simresult1 <- rxsolve1()
    minmax1 <- simresult1 |> 
      group_by(time) |> 
      mutate(min = quantile(ipred, 0.05),
             max = quantile(ipred, 0.95)
             )
    median1 <- simresult1 |> 
      group_by(time) |> 
      summarize(ipred = median(ipred))
    list(minmax1 = minmax1, median1 = median1)
  })
  
  output$plot1 <- renderPlot({
    data1 <- sim1()
    
    ggplot() + geom_ribbon(data = data1$minmax1,aes(ymin = min, ymax = max, x = time, fill = "Simulated Percentiles(5-95%)")) + geom_line(data = data1$median1, aes(x =time, y = ipred, color = "Median Concentration")) + theme_bw() +
      scale_fill_manual(values = "#9ed5f0") + scale_color_manual(values = "#192931") + scale_x_continuous(breaks = seq(0,24,4))+
      labs(x = "Time (hr)", y ="Selinexor Concentration (ng/mL)", fill = NULL, color = NULL)+
      theme(axis.text = element_text(size = 12),
            axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 17, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.text = element_text(size = 17),
            legend.position = "top"
            )
  })
  output$table1 <- renderTable({
    data2 <- sim1()
    NCA <- data2$median1 |> 
      mutate(ID=1) |> 
      tblNCA( key = "ID",colTime = "time", colConc = "ipred",dose = as.double(input$dose1),adm = "Extravascular",doseUnit = "mg",timeUnit = "h",concUnit = "ng/mL",R2ADJ = -1) |> 
      select(CMAX,AUCIFO) |> 
      mutate(Dose = paste(as.double(input$dose1), "mg")) |> 
      select(3,1,2)
  })
  
  output$selinexor <- renderTable({
    table2 <- tibble(
      "XPOVIO Dose" = c("60 mg", "80 mg", "100 mg"),
      "Cmax (ng/mL)" = c("442 (188)", "680 (124)", "693 (201)"),
      "AUC0-INF (ng·h/mL)" = c("4,096 (1,185)", "5,386 (1,116)", "6,998 (818)")
    )
  })
  
  dosing2 <- eventReactive(input$sim2,{
    eventTable(amount.units = "mg", time.units = "hr") |> 
      add.dosing(dose = as.double(input$dose2), nbr.doses = as.double(input$num2),dosing.interval = 24) |> 
      add.sampling(seq(0, max(24, 24*as.double(input$num2)), 0.3))
  })
  
  rxsolve2 <- eventReactive(input$sim2,{
    if (input$sex2 == 1) {
      gfr = 175*as.double(input$scr2)**(-1.154)*as.double(input$age2)**(-0.203)
      rxSolve(model2, nsim= 500, dosing2(), c(BSA = 0.007184*as.double(input$height2)**0.725*as.double(input$weight2)**0.425, 
                                              GFR = gfr, ALB = as.double(input$alb2))
      )
    }
    else if (input$sex2 == 2) {
      gfr = 175*as.double(input$scr2)**(-1.154)*as.double(input$age2)**(-0.203)*0.742 
      rxSolve(model2, nsim= 500, dosing2(), c(BSA = 0.007184*as.double(input$height2)**0.725*as.double(input$weight2)**0.425, 
                                              GFR = gfr, ALB = as.double(input$alb2))
      )
    }

  })
  
  sim2 <- reactive({
    simresult2 <- rxsolve2()
    minmax2 <- simresult2 |> 
      group_by(time) |> 
      mutate(min = quantile(ipred, 0.05),
             max = quantile(ipred, 0.95)
      )
    median2 <- simresult2 |> 
      group_by(time) |> 
      summarize(ipred = median(ipred))
    list(minmax2 = minmax2, median2 = median2)
  })
  
  output$plot2 <- renderPlot({
    data2 <- sim2()
    
    ggplot() + geom_ribbon(data = data2$minmax2,aes(ymin = min, ymax = max, x = time, fill = "Simulated Percentiles(5-95%)")) + geom_line(data = data2$median2, aes(x =time, y = ipred, color = "Median Concentration")) + theme_bw() +
      scale_fill_manual(values = "#9ed5f0") + scale_color_manual(values = "#192931") + 
      labs(x = "Time (hr)", y ="Lenalidomide Concentration (ng/mL)", fill = NULL, color = NULL)+     scale_x_continuous(breaks = seq(0, 120, 4), limits = c(0, 24 * as.double(input$num2)))+
      theme(axis.text = element_text(size = 12),
            axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = 17, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.text = element_text(size = 17),
            legend.position = "top"
      )
  })
  
  output$table2 <- renderTable({
    if (input$sex2 == 1) {
      table2 <- tibble(
        BSA = 0.007184*as.double(input$height2)**0.725*as.double(input$weight2)**0.425,
        GFR = 175*as.double(input$scr2)**(-1.154)*as.double(input$age2)**(-0.203),
        ALB = as.double(input$alb2)
      )
    }
    else if (input$sex2 == 2) {
      table2 <- tibble(
        BSA = 0.007184*as.double(input$height2)**0.725*as.double(input$weight2)**0.425,
        GFR = 175*as.double(input$scr2)**(-1.154)*as.double(input$age2)**(-0.203)*0.742,
        ALB = as.double(input$alb2)
      )
    }
    
  })
  
  output$table22 <- renderTable({
    

    max_time <- 24 * as.double(input$num2)
    

    iAUC <- data.frame(
      Name = paste0("AUC[", seq(0, max_time - 24, by = 24), "-", seq(24, max_time, by = 24), "h]"),
      Start = seq(0, max_time - 24, by = 24),
      End = seq(24, max_time, by = 24)
    )
    
    data2 <- sim2()
    
    NCA <- data2$median2 |> 
      mutate(ID = 1) |> 
      tblNCA(key = "ID", 
             colTime = "time", 
             colConc = "ipred", 
             dose = as.double(input$dose2), 
             adm = "Extravascular", 
             doseUnit = "mg", 
             timeUnit = "h", 
             concUnit = "ng/mL", 
             iAUC = iAUC,
             R2ADJ = -1) |> 
      select(matches("^AUC\\["))
  })
  
}

shinyApp(ui, server)