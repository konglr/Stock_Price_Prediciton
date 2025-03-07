# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyWidgets)
library(tidyquant)
library(ggplot2)
library(quantmod)
#library(tensorflow)
#library(keras)
library(TTR)
library(htmltools)

# Define UI
ui <- fluidPage(
  HTML('
        <head>
         <script async src="https://www.googletagmanager.com/gtag/js?id=G-8LL329L0WC"></script>
         <script>
           window.dataLayer = window.dataLayer || [];
           function gtag(){dataLayer.push(arguments);}
           gtag("js", new Date());
           gtag("config", "G-8LL329L0WC");
         </script>
        </head>
      '),
  
  titlePanel("Stock Price Prediction-AI股票价格预测"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 pickerInput(
                   inputId = "ticker",
                   label = "Ticker Symbol",
                   choices = c("AAPL", "AMZN", "GOOGL", "MSFT", "TSLA", "NVDA", "000001.SZ"),
                   selected = "AAPL"
                 ),
                 
                 radioButtons(
                   inputId = "period",
                   label = "Time Period",
                   choices = c("20 days" = 20, "1 month" = 30, "3 months" = 90, 
                               "6 months" = 180, "1 year" = 365),
                   selected = 30
                 ),
                 selectInput(
                   inputId = "plot_type",
                   label = "Plot Type",
                   choices = c("Line" = "line", "Bars" = "bars", "Candlesticks" = "candlesticks", "Matchsticks" = "matchsticks"),
                   selected = "candlesticks"
                 ),
                 
                 radioButtons("radio", label = "Prediction Models",
                              choices = list("Keras-5 days back" = 1, "Karas-SMA" = 2, "Keras-RSI" = 3), 
                              selected = 1),
                 
                 numericInput("table_rows", "显示表格行数:", 5),
                 checkboxInput("show_prediction", "显示预测数据", TRUE)
    ),
    mainPanel(width = 9,
              plotOutput(outputId = "plot"),
              tableOutput(outputId = "data"), # 使用 tableOutput
              conditionalPanel(condition = "input.show_prediction == true", tableOutput(outputId = "prediction_result"))
    )
  )
)
# Define server
server <- function(input, output) {
  library(tidyverse)
  library(plotly)
  
  # Get data
  ticker_data <- reactive({
    getSymbols(input$ticker,
               from = Sys.Date() - 380,
               to = Sys.Date(), auto.assign = FALSE)
  })
  
  output$plot <- renderPlot({
    plot_type <- switch(input$plot_type,
                        "line" = "line",
                        "bars" = "bars",
                        "candlesticks" = "candlesticks",
                        "matchsticks" = "matchsticks")
    
    chartSeries(ticker_data(), type = plot_type, name = input$ticker, theme = "white",
                subset = paste("last", input$period, "day", sep = " "),
                TA = c(addSMA(n = 5, on = 1, col = "brown"),
                       addSMA(n = 10, on = 1, col = "purple"),
                       addSMA(n = 20, on = 1, col = "orange"))
    )
  })
  
  # Create a table of the stock data
  output$data <- renderTable({
    data <- tail(ticker_data(), input$table_rows + 1) # 获取多一天的数据
    close_prices <- Cl(data)
    change_percentage <- (close_prices - lag(close_prices, 1)) / lag(close_prices, 1) * 100
    
    # 删除第一行的NA，只显示需要的数据
    change_percentage <- tail(change_percentage, input$table_rows)
    data <- tail(data, input$table_rows)
    close_prices <- tail(close_prices, input$table_rows)
    
    change_percentage_formatted <- paste0(round(change_percentage, 2), "%")
    table_data <- data.frame(data[, 1:5],  "Change" = change_percentage_formatted)
    table_data
  }, spacing = "xs", rownames = TRUE, striped = TRUE, hover = TRUE)
  
  
  output$prediction_result <- renderTable({
    data.frame(tail(ticker_data(), 5))
  })
}

# Run app
shinyApp(ui = ui, server = server)