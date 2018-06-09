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