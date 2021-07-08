#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

shinyServer <- function(input, output, session) {
    input_data <- reactive({
        if(input$input_stock == "NVDA"){
            NVDA_cc_return
          }else if(input$input_stock == "AMD"){
            AMD_cc_return
          }else if(input$input_stock == "PFE"){
            PFE_cc_return
          }else if(input$input_stock == "BPSO"){
            BPSO_cc_return
          }else if(input$input_stock == "ISP"){
            ISP_cc_return
          }else if(input$input_stock == "BYM"){
            BYM_cc_return
          }
    })
    
    input_dataset <- reactive({stocks_cc_returns[,input$input_stock]})
    plots_list_names <- c("hist", "lineplot","boxplot", "qqplot")
    
    hide_plots_func <- function(plot_to_hide){
      hide(plot_to_hide)
    }
    
    descriptive <- function() {
      v <-  observeEvent(input$plots,{ 
        
          if(input$plots=="histog"){
            hide_plots_func("lineplot")
            hide_plots_func("boxplot")
            hide_plots_func("qqplot")
            show("hist")
            output$hist <- renderPlot({
                hist(input_data(), freq = F, breaks = input$breakCount, 
                     main = paste("Istogramma returns CC mensili",input$input_stock), xlab = "return", 
                     col = "orange")
                  if(input$density == "Si"){
                     points(density(input_data()), type = "l", col = "black")
                  }})}
        
        
          if(input$plots=="rtns"){
            hide_plots_func("hist")
            shinyjs::hideElement(id= "cond_hist")
            hide_plots_func("boxplot")
            hide_plots_func("qqplot") 
            show("lineplot")
            output$lineplot <- renderDygraph({
              dygraph(input_dataset(), main=paste("Returns CC mensili", input$input_stock)) %>% 
                dySeries(input$input_stock, label=input$input_stock, color = "blue") %>%
                dyAxis("y", label="price") %>% 
                dyAxis("x", label="time") %>% 
                dyLegend(show = "onmouseover", hideOnMouseOut = T) %>%
                dyOptions(fillGraph = T) %>% 
                dyHighlight(highlightCircleSize = 5,
                            highlightSeriesBackgroundAlpha = 0.2,
                            highlightSeriesOpts = list(strokeWidth = 3),
                            hideOnMouseOut = T) %>%
                dyRangeSelector(height = 50)
             })}
        
          
          if(input$plots=="box"){
            hide_plots_func("hist")
            hide_plots_func("lineplot")
            hide_plots_func("qqplot")
            show("boxplot")
            output$boxplot <- renderPlot({
              chart.Boxplot(coredata(input_data()), 
                            outlier.symbol = "O", 
                            main=paste("Boxplot CC returns", input$input_stock), 
                            xlab="return")
             })}
          
          if(input$plots=="qq"){
            hide_plots_func("hist")
            hide_plots_func("boxplot")
            hide_plots_func("lineplot")
            show("qqplot")
            output$qqplot <- renderPlot({
              qqnorm(input_data(), main=paste("QQPlot CC returns", input$input_stock), col="black", pch=20)
              qqline(input_data(), col="orange")
           })}
          
          })
    }
    
    predictive <- function() {
      output$forecast_plot <- renderPlot({
        ticker <- input$asset_forecast
        arimaplotGen(get(paste(ticker, "_fc_ccr", sep = "")))
      })
    }
    
    descriptive()
    predictive()
    
}
