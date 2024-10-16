library(shiny)
library(bslib)
library(tidyverse)
library(NonCompart)


sidebar1 <- card(
)
simulation1 <- card(
)
result1 <- card(
  plotOutput("plot1")
)
sidebar2 <- card(
)
simulation2 <- card(
  plotOutput("plot2")
)
result2 <- card(
)


ui <- page_fillable(
  h2("Personalized Dose Regimen for Selinexor and Lenalidomide"),
  navset_card_underline(
    nav_panel(
      title = "Selinexor",
      hr(),
      layout_columns(
        col_widths = c(3,6,3),
        sidebar1,simulation1, result1
      ),
      hr()
    ),
    nav_panel(
      title = "Lenalidomide",
      hr(),
      layout_columns(
        col_widths = c(3,6,3),
        sidebar2,simulation2, result2
      ),
      hr()
    )
  )

)







server <- function(input, output, session) {
  
  
}




shinyApp(ui, server)