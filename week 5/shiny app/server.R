# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  cities_max_bike <- city_weather_bike_df %>%
    group_by(CITY_ASCII) %>%
    slice(which.max(BIKE_PREDICTION))
  
  # Observe drop-down event
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown == 'All') {
      #Render the city overview map
      output$city_bike_map <- renderLeaflet({
        #complete this function to render a leaflet map
        leaflet(cities_max_bike) %>% 
          addTiles() %>%
          setMaxBounds(lng1 = -171.522125, lat1 = 84.700978, lng2 = 210.469585, lat2 = -51.869708) %>%
          addCircleMarkers(data = cities_max_bike, 
                           lng= cities_max_bike$LNG,
                           lat = cities_max_bike$LAT, 
                           popup = cities_max_bike$LABEL,
                           radius = ~ifelse(cities_max_bike$BIKE_PREDICTION_LEVEL == 'small', 6, 12),
                           color = ~color_levels(cities_max_bike$BIKE_PREDICTION_LEVEL))
      })
    }
    else {
      #Render the specific city map
      filteredcity <- cities_max_bike %>% 
        filter(CITY_ASCII == input$city_dropdown)
      output$city_bike_map <- renderLeaflet({
        leaflet(filteredcity) %>%
          addTiles() %>%
          addPopups(data = filteredcity,
                    lng = filteredcity$LNG,
                    lat = filteredcity$LAT,
                    popup = filteredcity$DETAILED_LABEL,
                    options = popupOptions(noHide = T)) %>%
          addCircleMarkers(data = filteredcity,
                           lng = filteredcity$LNG,
                           lat = filteredcity$LAT, 
                           radius = ~ifelse(filteredcity$BIKE_PREDICTION_LEVEL == 'small', 6, 12),
                           color = ~color_levels(filteredcity$BIKE_PREDICTION_LEVEL))
        
      })
      #filter temp data for selected city
      sel_city_weather_bike_df <- city_weather_bike_df %>%
        filter(CITY_ASCII == input$city_dropdown)
      #temperature plot
      output$temp_line <- renderPlot ({
        temp_line_plot <- ggplot(sel_city_weather_bike_df, aes(x=1:length(TEMPERATURE), y=TEMPERATURE)) +
          geom_line(color = "yellow", size = 1) +
          labs(x = "Time (3 Hours Ahead)", y = "Temperature (C)") +
          geom_point() +
          geom_text(aes(label = paste(TEMPERATURE, "C")), hjust = 0, vjust = 0) +
          ggtitle("Temperature Trends")
        temp_line_plot
        
      })
      #bike prediction plot
      output$bike_line <- renderPlot ({
        bike_line_plot <- ggplot(sel_city_weather_bike_df, aes(x = as.POSIXct(FORECASTDATETIME, format = "%Y-%m-%d %H:%M:%S"), y=BIKE_PREDICTION)) +
          scale_x_datetime(date_labels = "%m-%d-%y") +
          geom_line(color = "darkcyan", size = 1, linetype = "dashed") +
          labs(x = "Time (3 hours ahead)", y= "Predicted Bike Count") +
          geom_point() +
          geom_text(aes(label = paste(BIKE_PREDICTION)), hjust = 0, vjust = 0) +
          ggtitle("Bike Prediction Trends")
        bike_line_plot
      })
      #text output of bike count point 
      output$bike_date_output <- renderText({
        paste("Time:", as.POSIXct(as.integer(input$plot_click$x), origin = "1970-01-01"),
              "\nBike Count Prediction: ", as.integer(input$plot_click$y))
      })
      #humidity prediction plot
      output$humidity_pred_chart <- renderPlot ({
        humid_line_plot <- ggplot(sel_city_weather_bike_df, aes(x = HUMIDITY, y = BIKE_PREDICTION)) +
          labs(x = "Humidity", y = "Predicted Bike Count") +
          geom_point() +
          geom_smooth(method = lm, formula = y ~ poly(x,4), color = "red") +
          ggtitle("Bike Prediction Based on Humidity")
        humid_line_plot
      })
    } 
  })
})