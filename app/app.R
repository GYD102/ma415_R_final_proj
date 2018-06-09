#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# source(paste(dirname(getwd()),"/api_pull.R",sep=""))
library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)

url <- "https://www.alphavantage.co"
path <- "/query"
apikey <- "NLS6EWI7EU6UMENW"

get_content <- function(q){
  data <- GET(url = url,
              path = path,
              query = q)
  # if (data$status_code != 200){
  #   return(FALSE)
  # }
  # View(data$content)
  raw <- rawToChar(data$content)
  # View(raw)
  # View(fromJSON(raw))
  return(fromJSON(raw))
}

content_to_tibble <- function(cont){
  cont_2 <- unlist(cont[[2]])
  return(
    tibble(
      symbol = cont[[1]][[2]],
      datetime = as.POSIXct(names(cont[[2]])),
      #time = format(as.POSIXct(names(cont[[2]])) ,format = "%H:%M:%S"),
      interval = cont[[1]][[4]],
      open = as.double(cont_2[grepl("open",names(cont_2))]),
      high = as.double(cont_2[grepl("high",names(cont_2))]),
      low = as.double(cont_2[grepl("low",names(cont_2))]),
      close = as.double(cont_2[grepl("[0-9]. close",names(cont_2))]),
      volume = as.double(cont_2[grepl("volume",names(cont_2))])
    )
  )
}

model <- function(pulled_data, input){
  active_data <- pulled_data %>%
    filter(datetime >= input$x_range[1] & datetime <= input$x_range[2]) %>%
    filter(close >= input$y_range[1] & close <= input$y_range[2])
  
  mod <- lm(close ~ ns(datetime, input$spline_count), data = active_data)
  
  grid <- active_data %>%
    data_grid(datetime) %>%
    add_predictions(mod)
  
  active_data <- active_data %>%
    add_residuals(mod)
  
  out <- list(active_data, grid)
  return(out)
}

visualize <- function(pulled_data, input){
  model_output <- model(pulled_data, input)
  
  active_data <- model_output[[1]]
  grid <- model_output[[2]]
  
  p1 <- ggplot(active_data,
               aes(datetime)) +
    geom_point(aes(y = close), color = "red") +
    scale_x_datetime() +
    geom_line(aes(y = close), color = "blue") +
    geom_line(aes(y = pred), data = grid,
              color = "green", size = 1) +
    labs(title = "Time Series and Model",
         x = "Datetime",
         y = "Stock Price")
  
  p2 <- ggplot(active_data, aes(datetime, resid)) +
    geom_ref_line(h = 0) +
    geom_point() +
    labs(title = "Deviation of Data-Points from Model",
         x = "Datetime",
         y = "Residual")
  
  p3 <- ggplot(active_data, aes(resid)) +
    geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) +
    geom_density(color = 'blue') +
    labs(title = "Residual Distribution",
         x = "Residual",
         y ="Density")
  
  grid.arrange(p1,p2,p3, ncol=1)
}


library(shiny)
library(modelr)
library(moments)
library(splines)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Data Collection and Visualization using Alphavantage API"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("funct", "Function:",
                     c("TIME_SERIES_INTRADAY",
                       "TIME_SERIES_DAILY",
                       "TIME_SERIES_DAILY_ADJUSTED",
                       "TIME_SERIES_WEEKLY",
                       "TIME_SERIES_WEEKLY_ADJUSTED",
                       "TIME_SERIES_MONTHLY",
                       "TIME_SERIES_MONTHLY_ADJUSTED" # ,
                       # "BATCH_STOCK_QUOTES",
                       # "CURRENCY_EXCHANGE_RATE",
                       # "DIGITAL_CURRENCY_INTRADAY",
                       # "DIGITAL_CURRENCY_DAILY",
                       # "DIGITAL_CURRENCY_WEEKLY",
                       # "DIGITAL_CURRENCY_MONTHLY",
                       # "SECTOR"
                       )),
         uiOutput("api_params_ui"),
         actionButton("go", "Get Data"),
         uiOutput("x_range"),
         uiOutput("y_range"),
         uiOutput("splines")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("ggp_ui", height = "900px")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$api_params_ui <- renderUI({
     output = tagList()
     if(is.element(input$funct,
                   c("TIME_SERIES_INTRADAY"))){
       output[[1]] = selectInput("interval", "Interval:", c("1min","5min",
                                              "15min","30min",
                                              "60min"))
       output[[2]] = checkboxInput("outputsize", "Full", FALSE)
       output[[3]] = textInput("symbol",
                               "Equity Name (ex. \"MSFT\", \"AAPL\", \"NVDA\"):")
     } else if(is.element(input$funct,
                          c("TIME_SERIES_DAILY",
                            "TIME_SERIES_DAILY_ADJUSTED"))){
       output[[1]] = checkboxInput("outputsize", "Full", FALSE)
       output[[2]] = textInput("symbol",
                               "Equity Name (ex. \"MSFT\", \"AAPL\", \"NVDA\"):")
     } else if(is.element(input$funct,
                          c("TIME_SERIES_WEEKLY",
                            "TIME_SERIES_WEEKLY_ADJUSTED",
                            "TIME_SERIES_MONTHLY",
                            "TIME_SERIES_MONTHLY_ADJUSTED"))){
       output[[1]] = textInput("symbol",
                               "Equity Name (ex. \"MSFT\", \"AAPL\", \"NVDA\"):")
      }# else if(is.element(input$funct,
     #                      c("DIGITAL_CURRENCY_INTRADAY",
     #                        "DIGITAL_CURRENCY_DAILY",
     #                        "DIGITAL_CURRENCY_WEEKLY",
     #                        "DIGITAL_CURRENCY_MONTHLY"))){
     #   output[[1]] = textInput("symbol", "Digital/Crypto Currency")
     #   output[[2]] = textInput("market", "Exchange Market")
     # } else if(is.element(input$funct,
     #                       c("CURRENCY_EXCHANGE_RATE"))){
     #   output[[1]] = textInput("from_currency", "Physical or Digital Currency; FROM")
     #   output[[2]] = textInput("to_currency", "Physical or Digital Currency; TO")
     # } else if(is.element(input$funct,
     #                      c("BATCH_STOCK_QUOTES"))){
     #   output[[1]] = textInput("symbol", "Equity Names (separation by commas, no spaces)")
     # }
     # else if(is.element(input$funct,
     #                    c("SECTOR"))){}
     output
   })
   observeEvent(input$go, {
     if(is.element(input$funct,
                   c("TIME_SERIES_INTRADAY"))){
       query <<- list('function' = input$funct,
                     symbol = input$symbol,
                     interval = input$interval,
                     # outputsize = c("compact","full")[as.integer(input$outputsize)+1],
                     apikey = apikey)
     } else if(is.element(input$funct,
                          c("TIME_SERIES_DAILY",
                            "TIME_SERIES_DAILY_ADJUSTED"))){
       query <<- list('function' = input$funct,
                     symbol = input$symbol,
                     # outputsize = c("compact","full")[as.integer(input$outputsize)+1],
                     apikey = apikey)
     } else if(is.element(input$funct,
                          c("TIME_SERIES_WEEKLY",
                            "TIME_SERIES_WEEKLY_ADJUSTED",
                            "TIME_SERIES_MONTHLY",
                            "TIME_SERIES_MONTHLY_ADJUSTED"))){
       query <<- list('function' = input$funct,
                     symbol = input$symbol,
                     apikey = apikey)
     } # else if(is.element(input$funct,
     #                      c("DIGITAL_CURRENCY_INTRADAY",
     #                        "DIGITAL_CURRENCY_DAILY",
     #                        "DIGITAL_CURRENCY_WEEKLY",
     #                        "DIGITAL_CURRENCY_MONTHLY"))){
     #   query <<- list('function' = input$funct,
     #                 symbol = input$symbol,
     #                 market = input$market,
     #                 apikey = apikey)
     # } else if(is.element(input$funct,
     #                      c("CURRENCY_EXCHANGE_RATE"))){
     #   query <<- list('function' = input$funct,
     #                 from_currency = input$from_currency,
     #                 to_currency = input$to_currency,
     #                 apikey = apikey)
     # } else if(is.element(input$funct,
     #                      c("BATCH_STOCK_QUOTES"))){
     #   query <<- list('function' = input$funct,
     #                 symbols = input$symbols,
     #                 apikey = apikey)
     # }
     # else if(is.element(input$funct,
     #                    c("SECTOR"))){
     #   query <<- list('function' = input$funct, apikey = apikey)
     # }
     # View(query)
     cont <- get_content(query)
     # View(cont)
     pulled_data <<- content_to_tibble(cont)
     # View(pulled_data)
     output$x_range <- renderUI({
       sliderInput("x_range", "Time Limits:",
                   min = min(pulled_data$datetime),
                   max = max(pulled_data$datetime),
                   value = c(min(pulled_data$datetime),
                             max(pulled_data$datetime)))
     })
     output$y_range <- renderUI({
       sliderInput("y_range", "Price Limits:",
                   min = min(pulled_data$close),
                   max = max(pulled_data$close),
                   value = c(min(pulled_data$close),
                             max(pulled_data$close)),
                   step = 0.005)
     }) 
     output$splines <- renderUI({
       sliderInput("spline_count", "Resolution of Regression:",
                   min = 1, max = 20, value = 1, step = 1)
     })
     
     output$ggp_ui <- renderPlot(
       visualize(pulled_data, input)
     )
     #View(pulled_data$volume)
     #View(sum(pulled_data$volume))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

