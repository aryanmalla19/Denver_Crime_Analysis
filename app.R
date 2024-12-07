
#https://yg979g-aryan0malla.shinyapps.io/CrimeShiny/

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)  # For time-based manipulation

# Load the dataset
crime_data <- read.csv("crime.csv")

selected_crime <- c("public-disorder", "drug-alcohol", "murder", "larceny")
selected_neighborhood <- c("lincoln-park", "clayton", "country-club", "mar-lee", "cbd", "barnum", "regis")

# Convert date columns to date-time format
crime_data$first_occurrence_date <- as.POSIXct(crime_data$first_occurrence_date, format="%m/%d/%Y %I:%M:%S %p", tz="UTC")
crime_data$reported_date <- as.POSIXct(crime_data$reported_date, format="%m/%d/%Y %I:%M:%S %p", tz="UTC")

# Extract additional time-related fields for analysis
crime_data$hour <- hour(crime_data$first_occurrence_date)
crime_data$day_of_week <- wday(crime_data$first_occurrence_date, label = TRUE)
crime_data$month <- month(crime_data$first_occurrence_date, label = TRUE)

# Define UI
ui <- navbarPage("Crime Data Visualization",
                 
                 # Tab 1: Crime by Category
                 tabPanel("Crime by Category",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("category", "Select Crime Category:", 
                                          choices = unique(crime_data$offense_category_id),
                                          selected = selected_crime, multiple = TRUE)
                            ),
                            mainPanel(
                              plotOutput("crimeCategoryPlot")
                            )
                          )),
                 
                 # Tab 2: Crime by Time of Day
                 tabPanel("Crime by Time of Day",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("hourRange", "Select Hour Range:", 
                                          min = 0, max = 23, 
                                          value = c(0, 23), step = 1)
                            ),
                            mainPanel(
                              plotOutput("crimeTimePlot")
                            )
                          )),
                 
                 # Tab 3: Crime by Day of the Week
                 tabPanel("Crime by Day of the Week",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("dayOfWeek", "Select Days:", 
                                                 choices = levels(crime_data$day_of_week),
                                                 selected = levels(crime_data$day_of_week))
                            ),
                            mainPanel(
                              plotOutput("crimeDayOfWeekPlot")
                            )
                          )),
                 
                 # Tab 4: Crime Heatmap by Month
                 tabPanel("Crime Heatmap by Month",
                          mainPanel(
                            plotOutput("crimeHeatmapPlot")
                          )),
                 
                 # Tab 5: Crime by Neighborhood
                 tabPanel("Crime by Neighborhood",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("neighborhood", "Select Neighborhood:", 
                                          choices = unique(crime_data$neighborhood_id), 
                                          selected = selected_neighborhood, multiple = TRUE)
                            ),
                            mainPanel(
                              plotOutput("crimeNeighborhoodPlot")
                            )
                          ))
)

# Define server logic
server <- function(input, output) {
  
  # Tab 1: Crime by Category
  output$crimeCategoryPlot <- renderPlot({
    filtered_data <- crime_data %>%
      filter(offense_category_id %in% input$category)
    
    ggplot(filtered_data, aes(x = offense_category_id)) +
      geom_bar() +
      labs(title = "Crime Distribution by Category", x = "Crime Category", y = "Count") +
      theme_minimal()
  })
  
  # Tab 2: Crime by Time of Day
  output$crimeTimePlot <- renderPlot({
    filtered_data <- crime_data %>%
      filter(hour >= input$hourRange[1] & hour <= input$hourRange[2])
    
    ggplot(filtered_data, aes(x = hour)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
      labs(title = "Crime Distribution by Time of Day", x = "Hour", y = "Crime Count") +
      theme_minimal()
  })
  
  # Tab 3: Crime by Day of the Week
  output$crimeDayOfWeekPlot <- renderPlot({
    filtered_data <- crime_data %>%
      filter(day_of_week %in% input$dayOfWeek)
    
    ggplot(filtered_data, aes(x = day_of_week)) +
      geom_bar(fill = "tomato", color = "white") +
      labs(title = "Crime Distribution by Day of the Week", x = "Day of Week", y = "Crime Count") +
      theme_minimal()
  })
  
  # Tab 4: Crime Heatmap by Month
  output$crimeHeatmapPlot <- renderPlot({
    monthly_crime <- crime_data %>%
      group_by(month) %>%
      summarise(crime_count = n())
    
    ggplot(monthly_crime, aes(x = month, y = crime_count)) +
      geom_tile(aes(fill = crime_count), color = "white") +
      scale_fill_gradient(low = "lightyellow", high = "red") +
      labs(title = "Crime Heatmap by Month", x = "Month", y = "Crime Count") +
      theme_minimal()
  })
  
  # Tab 5: Crime by Neighborhood
  output$crimeNeighborhoodPlot <- renderPlot({
    filtered_data <- crime_data %>%
      filter(neighborhood_id %in% input$neighborhood)
    
    ggplot(filtered_data, aes(x = neighborhood_id)) +
      geom_bar() +
      labs(title = "Crime Distribution by Neighborhood", x = "Neighborhood", y = "Count") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
