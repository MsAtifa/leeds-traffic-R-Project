library(dplyr)
library(tidyr)
library(hms)
library(dplyr)
library(lubridate)

#Data Cleaning and Manipulation

#Step 1: Check the data
traffic_data <- read.csv("Traffic%20accidents_2019_Leeds.csv", header = TRUE, check.names = FALSE)

# Step 2: Check data structure and summary
str(traffic_data)
summary(traffic_data)
colSums(is.na(traffic_data))

# Step 3: Convert Time (24hr) to POSIXct format and other variables to factors
traffic_data$'Time (24hr)' <- format(as.POSIXct(sprintf("%04d", traffic_data$'Time (24hr)'), format = "%H%M"), "%H:%M")

# Step 4: Summarize data by Reference Number
traffic_data <- traffic_data %>%
  mutate(`Age of Casualty` = as.numeric(`Age of Casualty`))

# Summarize data
summary_data <- traffic_data %>%
  group_by(`Reference Number`) %>%
  #Summing number of males and females separately
  summarize(
    `Number of Males` = sum(`Sex of Casualty` == "1", na.rm = TRUE), 
    `Number of Females` = sum(`Sex of Casualty` == "2", na.rm = TRUE),
    `Average Age` = round(mean(`Age of Casualty`, na.rm = TRUE)) #round off the age values
  )


# Step 5: Filter out specific unwanted values
filtered_data <- traffic_data %>%
  filter(
    !('1st Road Class & No' == "U" &
        'Weather Conditions' == 9 &
        'Road Surface' == 9 &
        'Lighting Conditions'== 7&
        'Type of Vehicle' == 90)
  )

# Step 6: Remove Local Authority and Vehicle Number columns
cleaned_data <- filtered_data %>%
  select(-'Local Authority', -'Vehicle Number',-'1st Road Class & No')

# Step 7: View the final cleaned dataset
print(cleaned_data)
str(cleaned_data)
summary(cleaned_data)

#Step 8: Combine summary_data with cleaned_data based on Reference Number
final_data <- cleaned_data %>%
  left_join(summary_data, by = 'Reference Number') %>%
  select(-'Sex of Casualty', -'Age of Casualty')
str(final_data)

#Step 9: Recode the variable values to categorical values
final_data$'Road Surface' <- dplyr::recode(traffic_data$'Road Surface',
                                           '1' = "Dry",
                                           '2' = "Wet/Damp",
                                           '3' = "Snow",
                                           '4' = "Frost/Ice",
                                           '5' = "Flood",
                                           '9' = "Unknown")
final_data$'Lighting Conditions' <- dplyr::recode(traffic_data$'Lighting Conditions',
                                                  '1' = "Daylight",
                                                  '4' = "Dark (lit street)",
                                                  '5' = "Dark (unlit street)",
                                                  '6' = "Dark (unknown lighting)",
                                                  '7' = "Unknown")
final_data$'Weather Conditions' <- dplyr::recode(traffic_data$'Weather Conditions',
                                                 '1' = "Fine",
                                                 '2' = "Raining",
                                                 '3' = "Snowing",
                                                 '4' = "Fog/Mist",
                                                 '5' = "High Winds",
                                                 '6' = "Rain with High Winds",
                                                 '7' = "Snow with High Winds",
                                                 '8' = "Other",
                                                 '9' = "Unknown")
final_data$'Type of Vehicle'=dplyr::recode(traffic_data$'Type of Vehicle',
                                           '1'="Pedal Cycle",
                                           '2'="Motorcycle(50cc or less)",
                                           '3'="Motorcycle(50cc-125cc)",
                                           '4'="Motorcycle(125cc-500cc)",
                                           '5'="Motorcycle(500cc or more)",
                                           '8'="Taxi/Private car",
                                           '9'="Car",
                                           '10'="Minibus(8-16 passenger seats)",
                                           '11'="Bus(17 or more passenger seats)",
                                           '16'="Ridden horse",
                                           '17'="Agricultural Vehicle",
                                           '18'="Tram",
                                           '19'="Van/Goods Vehicle(3.5 tonnes mgw or less)",
                                           '20'="Goods Vehicle(3.5-7.5 tonnes mgw)",
                                           '21'="Goods Vehicle(7.5 tonnes mgw or more)",
                                           '22'="Mobility Scooter",
                                           '23'="Electric Motorcycle",
                                           '90'="Other vehicle",
                                           '97'="Motorcycle-unkown cc",
                                           '98'="Goods vehicle – unknown weight")
final_data$'Casualty Class'=dplyr::recode(traffic_data$'Casualty Class',
                                          '1'="Driver/Rider",
                                          '2'="Vehicle Passenger",
                                          '3'="Pedestrian")

final_data$'Casualty Severity'=dplyr::recode(traffic_data$'Casualty Severity',
                                             '1'="Fatal",
                                             '2'="Serious",
                                             '3'="Slight")

final_data$'1st Road Class'=dplyr::recode(traffic_data$'1st Road Class',
                                          '1'="Motorway",
                                          '2'="Primary Motorway",
                                          '3'="Non - Primary Road",
                                          '4'="Road",
                                          '5'="Road or unclassified",
                                          '6'="Unclassified")


# Step 10: Save the final cleaned data to a CSV file
write.csv(final_data, "Leeds_Traffic_Accidents_Final_Cleaned.csv", row.names = FALSE)



