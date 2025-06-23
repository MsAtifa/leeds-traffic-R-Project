library(tidyr)
library(plotly)
library(hms)
library(shiny)
library(dplyr)
library(lubridate)
library(sf)
library(leaflet)
traffic_data <- read.csv("Leeds_Traffic_Accidents_Final_Cleaned.csv", check.names = FALSE)

#Dashboard Code
ui <- fluidPage(
  tags$style(
    HTML("
      body { background-color: #0047AB; }  /* Set cobalt background */
      .title { font-size: 36px; color: #FFFFFF; font-weight: bold; text-align: center;font-family: 'Open Sans', sans-serif;  }
      .plot-title { font-size: 24px; color: #FFFFFF; font-weight: bold;font-family: 'Open Sans', sans-serif;  }
      .stat-box { background-color: #1F2E2C; color: #fff; padding: 20px; text-align: center; margin: 10px; border-radius: 8px; }
      .stat-value { font-size: 30px; }
      .stat-icon { font-size: 50px; margin-bottom: 5px; }
      .plot-area { background-color: #151E3D; padding: 15px;font-size: 24px;text-align: center; border-radius: 8px; margin: 10px;font-family: 'Open Sans', sans-serif;  }
      .header-box { background-color: #151E3D; padding: 20px; color: #FFFFFF;font-size: 18px; border-radius: 8px; margin: 10px;font-family: 'Open Sans', sans-serif;  }
      .casualty-box { background-color: #151E3D; padding: 20px;font-size: 24px; border-radius: 8px; text-align: center; color: #FFFFFF;font-family: 'Open Sans', sans-serif;  }
      .casualty-icon { font-size: 70px; color: #2980b9; }
      .casualty-text { font-size: 24px; font-weight: bold; color: #FFFFFF; margin-top: 10px;font-family: 'Open Sans', sans-serif;  }
      .casualty-count { font-size: 30px; font-weight: bold; color: #FFFFFF; margin-top: 5px;font-family: 'Open Sans', sans-serif;  }
    ")
  ),
  
  # Title
  titlePanel(div(class = "title", "Leeds Traffic Accidents Dashboard")),
  
  #Header box elements
  fluidRow(
    column(width = 10, offset = 1,
           div(class = "header-box",
               fluidRow(
                 column(width = 5, offset = 1,
                        selectInput("severity", "Select Casualty Severity:", choices = c("Slight", "Serious", "Fatal"))
                 ),
                 column(width = 5,
                        uiOutput("averageAgeText")
                 )
               )
           )
    )
  ),
  
  # Casualties By Type section with counts
  fluidRow(
    column(
      width = 4,  # Occupying more width
      div(class = "casualty-box",
          h4("Casualties By Type", style = "color: #FFFFFF; font-family: 'Open Sans', sans-serif; font-size: 24px; font-weight: bold;"),
          div(class = "casualty-type",
              div(class = "casualty-icon", icon("user", lib = "font-awesome")),
              div(class = "casualty-text", "Driver"),
              div(class = "casualty-count", textOutput("driverCount")),  # Driver count
              br(),
              div(class = "casualty-icon", icon("user-friends", lib = "font-awesome")),
              div(class = "casualty-text", "Passenger"),
              div(class = "casualty-count", textOutput("passengerCount")),  # Passenger count
              br(),
              div(class = "casualty-icon", icon("walking", lib = "font-awesome")),
              div(class = "casualty-text", "Pedestrian"),
              div(class = "casualty-count", textOutput("pedestrianCount"))  # Pedestrian count
          )
      )
    ),
    
    column(
      width = 8,  
      fluidRow(
        column(width = 12,h5(class = "plot-title", "Monthly Accident Trend"),plotlyOutput("monthlyTrendPlot", height = "300px"))  # Line graph
      ),
      fluidRow(
        column(width = 12,h5(class = "plot-title", "Accidents by Vehicle Type"), plotlyOutput("vehicleTypePlot", height = "300px"))  # Vehicle type bar chart
      )
    )
  ),
  
  # Map Section for Accident Location by Severity
  fluidRow(
    column(
      width = 12, 
      div(class = "plot-area", 
          h5(class = "plot-title", "Accident Locations Map by Severity"),
          leafletOutput("severityMap", height = "500px"))
    )
  ),
  
  # Other rows with plots and content
  fluidRow(
    column(width = 6, div(class = "plot-area",h5(class = "plot-title", "Gender Distribution of Casualties"),plotlyOutput("genderPieChart"))),
    column(width = 6, div(class = "plot-area", h5(class = "plot-title", "Accidents by Weather Conditions"), plotlyOutput("weatherBarChart")))
  ),
  
  fluidRow(
    column(width = 6, div(class = "plot-area",  h5(class = "plot-title", "Road Surface vs. Lighting Conditions"),plotOutput("bubblePlot"))),
    column(width = 6, div(class = "plot-area",h5(class = "plot-title", "Accidents by Hour"), plotOutput("hourlyAccidentAreaPlot")))
  ),
  
  fluidRow(
    column(width = 6, div(class = "plot-area",h5(class = "plot-title", "Accidents by Road Type"),plotlyOutput("roadClassBarChart"))),
    column(width = 6, div(class = "plot-area",h5(class = "plot-title", "Number of Vehicles in Accidents"), plotlyOutput("vehiclepieChart")))
  )
)
  
server <- function(input, output) {
  # Reactive expression to filter data based on severity selection
  filtered_data_with_coords <- reactive({
    # Filter data by the selected severity
    filtered_data <- traffic_data %>% 
      filter(`Casualty Severity` == input$severity)
    
    # Process coordinates
    easting_northing_with_coords <- filtered_data %>%
      select(`Grid Ref: Easting`, `Grid Ref: Northing`) %>%
      st_as_sf(coords = c("Grid Ref: Easting", "Grid Ref: Northing"), crs = 27700) %>%
      st_transform(crs = 4326) %>%
      mutate(
        Latitude = st_coordinates(.)[, 2],
        Longitude = st_coordinates(.)[, 1]
      ) %>%
      as.data.frame()
    
    # Return the filtered data with coordinates
    cbind(filtered_data, easting_northing_with_coords)
  })
  
  # Q1: What is the distribution of casualties by gender and their average age for each severity?
  output$genderPieChart <- renderPlotly({
    # Summarize data for male and female counts
    data <- filtered_data_with_coords() %>%
      summarise(
        Male = sum(as.numeric(`Number of Males`), na.rm = TRUE),
        Female = sum(as.numeric(`Number of Females`), na.rm = TRUE)
      ) %>%
      tidyr::gather(key = "Gender", value = "Count")
    
    # Create the pie chart with Plotly
    fig <- plot_ly(
      data,
      labels = ~Gender,
      values = ~Count,
      type = 'pie',
      textinfo = 'none',  
      hoverinfo = 'label+value',  
      marker = list(colors = c("#78A0F7", "#F8656E"))
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(x = 1, y = 0.5)
      )
    
    fig
  })
  
  #Average Age
  output$averageAgeText <- renderUI({
    # Calculate the average age for males and females separately
    avg_age_gender <- filtered_data_with_coords() %>%
      summarise(
        Average_Age_Male = mean(`Average Age`[as.numeric(`Number of Males`) > 0], na.rm = TRUE),
        Average_Age_Female = mean(`Average Age`[as.numeric(`Number of Females`) > 0], na.rm = TRUE)
      )
    
    # Create HTML output with line breaks for better readability
    HTML(paste(
      "<strong>Average age of casualties:</strong><br>",
      "Male: ", round(avg_age_gender$Average_Age_Male), "<br>",  
      "Female: ", round(avg_age_gender$Average_Age_Female)  
    ))
  })
  
  #Q2: How does the location of accidents influence the severity of traffic accidents?
  # Render the Leaflet map
  output$severityMap <- renderLeaflet({
    leaflet(data = filtered_data_with_coords()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 5,
        color = ~ifelse(`Casualty Severity` == "Fatal", "red",
                        ifelse(`Casualty Severity` == "Serious", "orange", "green")), # Different colors for severities
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste("Severity:", `Casualty Severity`, "<br>",
                       "Latitude:", round(Latitude, 4), "<br>",
                       "Longitude:", round(Longitude, 4))
      )
  })
  
  #Q3:How does the distribution of casualties among drivers, passengers, and pedestrians vary across different accident severities?
  output$driverCount <- renderText({
    filtered_data_with_coords() %>%
      filter(`Casualty Class` == "Driver/Rider") %>%
      nrow()
  })
  
  output$passengerCount <- renderText({
    filtered_data_with_coords() %>%
      filter(`Casualty Class` == "Vehicle Passenger") %>%
      nrow()
  })
  
  output$pedestrianCount <- renderText({
    filtered_data_with_coords() %>%
      filter(`Casualty Class` == "Pedestrian") %>%
      nrow()
  })
  
  # Q4: Which vehicle types are most commonly involved in traffic accidents, and how does their involvement impact casualty severity?
  output$vehicleTypePlot <- renderPlotly({
    vehicledata <- filtered_data_with_coords() %>%
      group_by(`Type of Vehicle`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    # Create ggplot with tooltips for plotly
    p <- ggplot(vehicledata, aes(y = reorder(`Type of Vehicle`, Count), x = Count, text = paste("Count:", Count))) +
      geom_bar(stat = "identity", fill = "#0047AB", width = 0.5) +
      labs(title = "Accidents by Vehicle Type", x = "Count", y = "Vehicle Type") +
      theme_minimal()
    
    # Convert ggplot to plotly for interactive tooltips
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(r = 100))  # Adjust margins if needed
  })
  
  #Q5: How do monthly variations in accident counts influence the severity of casualties?
  output$monthlyTrendPlot <- renderPlotly({
    # Ensure 'Accident Date' is in Date format in the filtered data
    filtered_data <- filtered_data_with_coords() %>%
      mutate(`Accident Date` = dmy(`Accident Date`))  # Using dmy() from lubridate
    
    # Group by year and month, then count accidents per month
    monthly_accidents <- filtered_data %>%
      mutate(YearMonth = floor_date(`Accident Date`, 'month')) %>%
      group_by(YearMonth) %>%
      summarise(Accident_Count = n(), .groups = 'drop')
    
    # Create a full sequence of months
    all_months <- data.frame(
      YearMonth = seq(min(monthly_accidents$YearMonth), max(monthly_accidents$YearMonth), by = "month")
    )
    
    
    # Merge with actual accident data, filling missing months with 0
    monthly_accidents_complete <- all_months %>%
      left_join(monthly_accidents, by = "YearMonth") %>%
      replace_na(list(Accident_Count = 0))
    
    
    # Create an interactive line chart using Plotly
    plot_ly(monthly_accidents_complete, x = ~YearMonth, y = ~Accident_Count, type = 'scatter', mode = 'lines+markers', 
            line = list(color = '#0047AB'), 
            marker = list(size = 4)) %>%
      layout(
             xaxis = list(title = "Month", tickformat = "%b", dtick = "M1"),
             yaxis = list(title = "Number of Accidents"),
             hovermode = "closest") 
  })
  
  # Q6: How do road surface types and lighting conditions affect accident frequency
  output$bubblePlot <- renderPlot({
    data <- filtered_data_with_coords() %>%
      group_by(`Road Surface`, `Lighting Conditions`) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(desc(Count))
    
    ggplot(data, aes(x = `Road Surface`, y = `Lighting Conditions`, size = Count, color = Count)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(3, 15)) +  # Adjust size range for bubbles
      scale_color_gradient(low = "#78A0F7", high = "#2862DC") +
      labs(
        x = "Road Surface Type", y = "Lighting Condition", size = "Accident Count", color = "Accident Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
 
  #Q7:  How do weather conditions influence the severity of traffic accidents?
  output$weatherBarChart <- renderPlotly({
    weather_data <- filtered_data_with_coords() %>%
      group_by(`Weather Conditions`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    # Create the ggplot bar chart
    weather_chart <- ggplot(weather_data, aes(x = reorder(`Weather Conditions`, Count), y = Count, fill = `Weather Conditions`)) +
      geom_bar(stat = "identity", width = 0.7) +
      labs(x = "Weather Conditions", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),  
            legend.position = "none")
    
    
    # Convert ggplot to plotly for interactivity with only count labels
    ggplotly(weather_chart, tooltip = "y")
  })
  
  
  
  # Q8: What are the peak times for the accidents to occur?
  output$hourlyAccidentAreaPlot <- renderPlot({
    # Convert 'Time (24hr)' to character, then extract hour
    hourly_data <- filtered_data_with_coords() %>%
      mutate(`Time (24hr)` = as.character(`Time (24hr)`),
             Hour = as.numeric(substr(`Time (24hr)`, 1, 2))) %>%
      group_by(Hour) %>%
      summarise(Accident_Count = n(), .groups = 'drop')
    
    # Create the area plot for accident count by hour
    ggplot(hourly_data, aes(x = Hour, y = Accident_Count)) +
      geom_area(fill = "#0047AB", alpha = 0.7) +  # Single color fill
      labs(
           x = "Hour of the Day",
           y = "Accident Count") +
      scale_x_continuous(breaks = 0:23) +  # Set x-axis ticks for each hour
      theme_minimal()
  })
  
  
  #Q9: In what type of roads, do the accidents occur?
  output$roadClassBarChart <- renderPlotly({
    # Summarize the count for each road type
    road_data <- filtered_data_with_coords() %>%
      group_by(`1st Road Class`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    # Create the ggplot bar chart
    road_chart <- ggplot(road_data, aes(x = reorder(`1st Road Class`, Count), y = Count, fill = `1st Road Class`)) +
      geom_bar(stat = "identity", width = 0.7) +
      labs(x = "Road Type", y = "Count") +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Convert ggplot to plotly for interactivity with only count labels
    ggplotly(road_chart, tooltip = "y")
  })
  
  
  
  #Q10: Number of vehicles involved in the accidents?
  output$vehiclepieChart <- renderPlotly({
    # Summarize data for each vehicle count
    vehicledata <- filtered_data_with_coords() %>%
      count(`Number of Vehicles`) %>%
      rename(Vehicle_Count = `Number of Vehicles`, Count = n)
    
    # Create the pie chart with Plotly
    vehifig <- plot_ly(
      vehicledata,
      labels = ~Vehicle_Count,
      values = ~Count,
      type = 'pie',
      textinfo = 'none',  
      hoverinfo = 'value',  
      marker = list(colors = c("#78A0F7", "#45E18D", "#FFC505", "#F8656E", "#B45082", "#F5DC71")) 
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(x = 1, y = 0.5)
      )
    
    vehifig
  })
}

#Run the app
shinyApp(ui = ui, server = server)

