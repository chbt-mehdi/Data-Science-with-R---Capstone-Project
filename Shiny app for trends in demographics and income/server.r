# Load libraries
library(shiny)
library(tidyverse)

# Read in data
adult <- read_csv("adult.csv")
# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

# Define server logic
shinyServer(function(input, output) {
  
  df_country <- reactive({
    adult %>% filter(native_country == input$country)
  })
  
  # TASK 5: Create logic to plot histogram or boxplot
  output$p1 <- renderPlot({
    if (input$graph_type == "histogram") {
      
      # Histogram
      ggplot(df_country(), aes_string(x = input$continuous_variable)) +
        geom_histogram(binwidth = 5) + 
        ggtitle(paste("Histogram of", input$continuous_variable)) +
        xlab(input$continuous_variable) +
        ylab("Number of People") +
        facet_wrap(c("prediction"))
    } else {
      
      # Boxplot
      ggplot(df_country(), aes_string(y = input$continuous_variable)) +
        geom_boxplot() +
        coord_flip() +
        ggtitle(paste("Boxplot of", input$continuous_variable)) +
        ylab(input$continuous_variable) +
        facet_wrap(c("prediction"))
    }
  })
  
  # TASK 6: Create logic to plot faceted bar chart or stacked bar chart
  output$p2 <- renderPlot({
    # Bar chart
    p <- ggplot(df_country(), aes_string(x = input$categorical_variable)) +
      ylab("Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom") +
      ylab("Number of People")
    
    
    if(input$is_stacked) {
      p + geom_bar(aes_string(fill = "prediction"), position = "stack") +
        ggtitle(paste("Traind of", input$categorical_variable))
      
    } else {
      p + geom_bar(aes_string(fill = input$categorical_variable)) +
        facet_wrap(~prediction) +
        ggtitle(paste("Traind of", input$categorical_variable))
    }
  })
})
