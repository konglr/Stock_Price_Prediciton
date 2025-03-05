
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
        <!-- Google tag (gtag.js) -->
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
        choices = c("10 day" = 10, "20 days" = 20, "1 month" = 30, "3 months" = 90, "1 year" = 365),
        selected = 20
      ),
      selectInput(
        inputId = "plot_type",
        label = "Plot Type",
        choices = c("Line" = "line", "Bars" = "bars", "Candlesticks" = "candlesticks", "Matchsticks"="matchsticks"),
        selected = "candlesticks"
      ),
      
      radioButtons("radio", label = "Prediction Models",
                   choices = list("Keras-5 days back" = 1, "Karas-SMA" = 2, "Keras-RSI" = 3), 
                   selected = 1)
    ),
    mainPanel(width = 9,
      plotOutput(outputId = "plot"),
      tableOutput(outputId = "data"),
      tableOutput(outputId = "prediction_result")
    )
  )
)
# Define server
server <- function(input, output) {
  library(tidyverse)
  library(plotly)
  
  # Get data
  ticker_data <<-  reactive({
    getSymbols(input$ticker,
               #from = Sys.Date() - days(input$period),
               from = Sys.Date() - 400,
               to = Sys.Date(), auto.assign = FALSE)
  })
  # Load hdf5 Model 
  model_5_days <- keras::load_model_hdf5("model_PAYH_5_days.h5")
  
  predict_result <- reactive({
    last_five_rows <- tail(ticker_data(), 5) 
    last_five_data <- last_five_rows[, 1:4]
    test_5_days_data <- array(0, dim = c(1,5,4))
    test_5_days_data[1,,] <-test_5_days_data
      
    prediction <- predict(model_5_days, test_5_days_data)
    
    # Return the prediction result
    return(prediction)
  })
  
  output$plot <- renderPlot({
  
    # Plot data
    plot_type <- switch(input$plot_type,
                        "line" = "line",
                        "bars" = "bars",
                        "candlesticks" = "candlesticks",
                        "matchsticks" = "matchsticks")
    
    chartSeries(ticker_data(), type = plot_type, name= input$ticker, theme = "white",
                subset = paste("last",input$period, "day",sep = " "),
                TA=c(    addSMA(n=5, on=1, col = "brown"),
                         addSMA(n=10, on=1, col = "purple"),
                         addSMA(n=20, on=1, col = "orange")
                         )
                )

 
    
   # ggplot(ticker_data, aes(x = date, y = close)) +
     # geom_(color = "black") +
    #  labs(title = input$ticker, x = "Date", y = "Price")
  })
 
  # Create a table of the stock data
  output$data <- renderTable(data.frame(ticker_data()[tail(1:nrow(ticker_data()), 5), 1:5]), spacing = "xs", rownames= TRUE,
                             striped=TRUE, hover=TRUE)
  
  output$prediction_result <- renderTable({
    data.frame(predict_result())
    #data.frame(tail(ticker_data(), 5))
  })
  
}

# Run app
shinyApp(ui = ui, server = server)