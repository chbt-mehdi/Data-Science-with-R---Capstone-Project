# Load required libraries
require(leaflet)


# Create a RShiny UI
shinyUI(
  fluidPage(padding=5,
            titlePanel("Bike-sharing demand prediction app"), 
            # Create a side-bar layout
            sidebarLayout(
              # Create a main panel to show cities on a leaflet map
              mainPanel(
                leafletOutput('city_bike_map', height = 1000, width = 1300)
                
              ),
              # Create a side bar to show detailed plots for a city
              sidebarPanel(
                # select drop down list to select city
                selectInput(inputId = "city_dropdown", "Select City: ",
                            c("All", "Seoul", "Suzhou", "London", "New York", "Paris")),
                plotOutput('temp_line', height = 300, width = 440),
                plotOutput('bike_line', height = 300, width = 440, click ="plot_click"),
                verbatimTextOutput("bike_date_output"),
                plotOutput('humidity_pred_chart', height = 300, width = 440)
              ))
  ))