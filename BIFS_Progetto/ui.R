#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

shinyUI <- fluidPage(
  tags$h1("Progetto BISF:"),
  useShinyjs(),# Include shinyjs
  navbarPage(
    "Selezione task:",
    tabPanel(
      "Descriptive Analysis",
        sidebarLayout(
          sidebarPanel(
            selectInput("input_stock", 
                        label = "Selezione stock", 
                        selectize = TRUE,
                        choices = c("NVDA", "AMD", "BPSO", "ISP", "PFE", "BMY")),
                        radioButtons("plots", "Selezionare il tipo di plot:",
                                   c(
                                     "CCReturns" = "rtns",
                                     "Boxplot" = "box",
                                     "QQPlot" = "qq",
                                     "Histogram" = "histog")),
            conditionalPanel(condition = "output.hist", id="cond_hist",
                             strong(tags$u("Condizioni per istogramma")),
                             radioButtons("density",
                                          label = "Mostrare linea densita'?",
                                          choices = c("Si", "No"),
                                          selected = "No"),
                             sliderInput(inputId = "breakCount",
                                         label = "Selezionare il numero di breaks:",
                                         min = 1,
                                         max = 50,
                                         value = 10))
                      ),
              mainPanel(
                dygraphOutput("lineplot"),
                plotOutput("hist"),
                plotOutput("boxplot"),
                plotOutput("qqplot"),
              )
    
        )
      ),
    
    tabPanel(
      "Predictive Analysis",
      sidebarLayout(
        sidebarPanel(
          h3("Forecasting ARIMA model"),
          radioButtons("asset_forecast", "Asset forecast(10 year)",
                       c("NVDA" ="NVDA", "AMD"="AMD", "BPSO"="BPSO", "ISP"="ISP", "PFE"="PFE", "BMY"="BMY")),
          "List of params used: ",
          "NVDA(1,0,5) AMD(2,0,2) BPSO(4,0,3) ISP(5,0,2) PFE(3,0,4) BMY(1,0,4)",
        ),
        mainPanel(
          plotOutput("forecast_plot", height = "500px")
        )
      )
    )
  )
)