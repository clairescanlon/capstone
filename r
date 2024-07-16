


###### PHASE 2 - PREPARE DATA ######

### Merging All Yearly Datasets Together & Checking Data

## Installing and Attaching R Packages ##

install.packages(c("tidyverse", "janitor", "data.validator", "lubridate", "data.table", "dplyr", "knitr","readr"))

library(tidyverse)
library(janitor)
library(data.validator)
library(ggplot2)
library(dplyr)
library(lubridate) 
library(data.table)
library(stringr)
library(knitr)
library(tidyr)
library(readr)

# Set your working directory to where the CSV files are located
setwd("C:/Users/USERNAME/LOCATION/FOLDERWHEREDATASETSARE/")


# Get a list of all CSV files in the directory which are 2020,2021, 2022 and 2022 datasets
csv_files <- list.files(pattern = ".csv")
view(csv_files)

# Read and combine all CSV files using dplyr::bind_rows()
cyclisticdata <- lapply(csv_files, function(x) read.csv(x, stringsAsFactors = FALSE, 
                                                                colClasses = c("start_station_id" = "character", "end_station_id" = "character"))) %>% 
  bind_rows()

# View the first few rows of the data
head(cyclisticdata)

# View summary statistics for the data
summary(cyclisticdata)

# View the last few rows of the data
tail(cyclisticdata)

# Save merged data as CSV
write.csv(cyclisticdata, file = "cyclisticdata.csv", row.names = FALSE)


# Install required packages
install.packages(c("summarytools", "DataExplorer", "dplyr", "tidyr", "janitor", "skimr", "ggplot2", "readr", "knitr", "cluster", "lubridate", "data.validator", "tidyverse","geosphere"))

# Load required packages
library(summarytools)
library(DataExplorer)
library(dplyr)
library(tidyr)
library(janitor)
library(skimr)
library(ggplot2)
library(readr)
library(knitr)
library(cluster)
library(lubridate)
library(data.validator)
library(tidyverse)
library(geosphere)

# Set your working directory to where the CSV files are located
setwd("C:/Users/USERNAME/LOCATION/FOLDERWHEREDATASETSARE/")

# Save cyclisticdata as an Rda file
save(cyclisticdata, file = "cyclisticdata.Rda")

# Load cyclisticdata from a CSV file
cyclisticdata <- read_csv("C:/Users/USERNAME/LOCATION/FOLDERWHEREDATASETSARE/cyclisticdata.csv")
View(cyclisticdata)

###### Exploring the Data ######

# Introduce the data
introduce(cyclisticdata)

# Display the structure of the data
str(cyclisticdata)

# Display summary statistics of data
summary(cyclisticdata)

# Display the column names of the data
names(cyclisticdata)

###### Handling Missing Values ######

# Find missing values in the data
is.na(cyclisticdata)

# Count the number of missing values in the data frame
sum(is.na(cyclisticdata))

# Count missing values in each column
colSums(is.na(cyclisticdata))

# Count rows with missing values
sum(rowSums(is.na(cyclisticdata)) > 0)

# Remove rows with missing values
cleanedcyclisticdata <- na.omit(cyclisticdata)

# Save cleanedcyclisticdata as an Rda file
save(cleanedcyclisticdata, file = "cleanedcyclisticdata.Rda")

# Write the cleaned data frame to a CSV file
write.csv(cleanedcyclisticdata, file = "cleanedcyclisticdata.csv", row.names = FALSE)

# Introduce the cleaned data
introduce(cleanedcyclisticdata)

# Display the structure of the cleaned data
str(cleanedcyclisticdata)

# Display summary statistics of cleaned data
summary(cleanedcyclisticdata)

# Display the column names of the cleaned data
names(cleanedcyclisticdata)

# Identify complete cases
complete.cases(cleanedcyclisticdata)

###### Handling Duplicate Values ######

# Group data by ride_id and count the number of occurrences
cleanedcyclisticdata %>% 
  group_by(ride_id) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# Delete duplicate data
cleanedcyclisticdata2 <- cleanedcyclisticdata %>% 
  distinct(ride_id, .keep_all = TRUE)

# Save cleanedcyclisticdata2 as an Rda file
save(cleanedcyclisticdata2, file = "cleanedcyclisticdata2.Rda")

# Display the structure of cleanedcyclisticdata2
str(cleanedcyclisticdata2)

# Display summary statistics of cleanedcyclisticdata2
summary(cleanedcyclisticdata2)

# Update the cleaned CSV file to reflect the changes
write_csv(cleanedcyclisticdata2, "cleanedcyclisticdata2.csv")

##### Creating New Columns #######

# Load required libraries
library(dplyr)
library(lubridate)

# Load the cleanedcyclisticdata2 dataset
load("cleanedcyclisticdata2.rda")

# Create Ride_duration column by subtracting the start time from the end time and converting the difference to minutes
cleanedcyclisticdata3 <- cleanedcyclisticdata2 %>% 
  mutate(ride_duration = as.numeric(difftime(ended_at, started_at, units = "mins")))

# Create Day_of_week column by extracting the day of the week from the start time
cleanedcyclisticdata3 <- cleanedcyclisticdata3 %>% 
  mutate(day_of_week = wday(started_at, label = TRUE, abbr = FALSE))

# Create Time_of_day column by formatting the start time to show only the hour, minute, and second
cleanedcyclisticdata3 <- cleanedcyclisticdata3 %>% 
  mutate(time_of_day = format(started_at, format = "%H:%M:%S"))

# Create Season column by categorizing the start time by month as winter, spring, summer, fall, or unknown
cleanedcyclisticdata3 <- cleanedcyclisticdata3 %>% 
  mutate(season = case_when(month(started_at) %in% c(12, 1, 2) ~ "winter",
                            month(started_at) %in% c(3, 4, 5) ~ "spring",
                            month(started_at) %in% c(6, 7, 8) ~ "summer",
                            month(started_at) %in% c(9, 10, 11) ~ "fall",
                            TRUE ~ "unknown"))

# Save the new dataset as cleanedcyclisticdata3.rda
save(cleanedcyclisticdata3, file = "cleanedcyclisticdata3.rda")

# Save the new dataset as a CSV file
write.csv(cleanedcyclisticdata3, "cleanedcyclisticdata3.csv", row.names = FALSE)


# Set your working directory to where the CSV files are located
setwd("C:/Users/USERNAME/LOCATION/FOLDERWHEREDATASETSARE/")

# Load the cleanedcyclisticdata3 dataset
load("cleanedcyclisticdata3.rda")

# Print information about the cleanedcyclisticdata3 dataset
introduce(cleanedcyclisticdata3)

# Display the structure of the cleanedcyclisticdata3 dataset
str(cleanedcyclisticdata3)

# Display summary statistics of the cleanedcyclisticdata3 dataset
summary(cleanedcyclisticdata3)

# Display the variable names of the cleanedcyclisticdata3 dataset
names(cleanedcyclisticdata3)

###### PHASE 3 - PROCESS DATA ######
# Install required packages
install.packages(c("summarytools", "DataExplorer", "dplyr", "tidyr", "janitor", "skimr", "ggplot2", "readr", "knitr", "cluster", "lubridate", "data.validator", "tidyverse","geosphere"))

# Load required packages
library(summarytools)
library(DataExplorer)
library(dplyr)
library(tidyr)
library(janitor)
library(skimr)
library(ggplot2)
library(readr)
library(knitr)
library(cluster)
library(lubridate)
library(data.validator)
library(tidyverse)
library(geosphere)


# Set your working directory to where the CSV files are located
setwd("C:/Users/Name/Location")


# load the cleanedcyclisticdata3 dataset
load("cleanedcyclisticdata3.rda")

### Changing Data Types ###

# Convert character variables to appropriate data types
cleanedcyclisticdata3$ride_id <- as.character(cleanedcyclisticdata3$ride_id)
cleanedcyclisticdata3$rideable_type <- as.factor(cleanedcyclisticdata3$rideable_type)
cleanedcyclisticdata3$start_station_name <- as.character(cleanedcyclisticdata3$start_station_name)
cleanedcyclisticdata3$start_station_id <- as.character(cleanedcyclisticdata3$start_station_id)
cleanedcyclisticdata3$end_station_name <- as.character(cleanedcyclisticdata3$end_station_name)
cleanedcyclisticdata3$end_station_id <- as.character(cleanedcyclisticdata3$end_station_id)
cleanedcyclisticdata3$member_casual <- as.factor(cleanedcyclisticdata3$member_casual)
cleanedcyclisticdata3$day_of_week <- as.factor(cleanedcyclisticdata3$day_of_week)
cleanedcyclisticdata3$time_of_day <- as.POSIXct(cleanedcyclisticdata3$time_of_day, format = "%H:%M:%S")
cleanedcyclisticdata3$season <- as.factor(cleanedcyclisticdata3$season)

# Convert geographic coordinates to numeric
cleanedcyclisticdata3$start_lat <- as.numeric(cleanedcyclisticdata3$start_lat)
cleanedcyclisticdata3$start_lng <- as.numeric(cleanedcyclisticdata3$start_lng)
cleanedcyclisticdata3$end_lat <- as.numeric(cleanedcyclisticdata3$end_lat)
cleanedcyclisticdata3$end_lng <- as.numeric(cleanedcyclisticdata3$end_lng)

# Save the cleanedcyclisticdata3 object to a file
save(cleanedcyclisticdata3, file = "cleanedcyclisticdata4.rda")

# save the new dataset as a CSV file
write.csv(cleanedcyclisticdata4, "cleanedcyclisticdata4.csv", row.names = FALSE)
  
### Checking for Outliers ###

# Load required libraries
library(dplyr)
library(caret)

# Set your working directory to where the CSV files are located
setwd("C:/Users/USERNAME/LOCATION/FOLDERWHEREDATASETSARE/")

load("C:/Users/USERNAME/LOCATION/FOLDERWHEREDATASETSARE/cleanedcyclisticdata3.rda")

# Clear the workspace
rm(list = ls())

# Load the cleanedcyclisticdata3 dataset
load("cleanedcyclisticdata3.rda")

# Check for outliers
outliers <- boxplot.stats(cleanedcyclisticdata3$ride_duration)$out

# Inspect outliers
print(outliers)

# Remove outliers
cleanedcyclisticdata3 <- cleanedcyclisticdata3 %>%
  filter(!ride_duration %in% outliers)


# Summary statistics of cleaned data
summary(cleanedcyclisticdata3)

str(cleanedcyclisticdata3)

# Split the dataset into member and casual datasets
member_data <- cleanedcyclisticdata3 %>%
  filter(member_casual == "member")

# Save member_data as Rda file
save(member_data, file = "member_data.Rda")

# Save member_data as CSV file
write.csv(member_data, file = "member_data.csv", row.names = FALSE)

casual_data <- cleanedcyclisticdata3 %>%
  filter(member_casual == "casual")

# Save casual_data as Rda file
save(casual_data, file = "casual_data.Rda")

# Save casual_data as CSV file
write.csv(casual_data, file = "casual_data.csv", row.names = FALSE)


###### PHASE 4 - ANALYZE DATA ######

### KPI for average ride duration per user type (based on weighted average to reduce bias as the sizes of the two groups are not the same)

# Load member_data and casual_data
load("member_data.Rda")
load("casual_data.Rda")

# Calculate the total ride duration and number of rides for each user type
member_summary <- member_data %>%
  summarise(total_ride_duration = sum(ride_duration),
            total_rides = n())

casual_summary <- casual_data %>%
  summarise(total_ride_duration = sum(ride_duration),
            total_rides = n())

# Calculate weights based on the number of rides
member_weight <- member_summary$total_rides / (member_summary$total_rides + casual_summary$total_rides)
casual_weight <- casual_summary$total_rides / (member_summary$total_rides + casual_summary$total_rides)

# Calculate the weighted average ride duration for each user type
weighted_average_duration <- (member_summary$total_ride_duration * member_weight +
                                casual_summary$total_ride_duration * casual_weight) /
  (member_weight + casual_weight)

# Print the results
cat("Weighted Average Ride Duration (Member):", weighted_average_duration[1], "minutes\n")
cat("Weighted Average Ride Duration (Casual):", weighted_average_duration[1], "minutes\n")

# Create a data frame with weighted average ride duration for each customer group
weighted_average_df <- data.frame(Customer_Group = c("Member", "Casual"),
                                  Weighted_Average_Ride_Duration = weighted_average_duration)

# Set your working directory to the location where you want to save the CSV file
setwd("C:/Users/USERNAME/LOCATION/FOLDERWHERECSVFILEWILLBESAVED/")

# Save the data frame as a CSV file
write.csv(weighted_average_df, "weighted_average_ride_duration.csv", row.names = FALSE)


### Analyzing Ride Duration by User Type and Day of the Week ###

# Aggregate ride duration by user type and day of the week
ride_duration_agg <- cleanedcyclisticdata3 %>%
  group_by(member_casual, day_of_week) %>%
  summarize(avg_ride_duration = mean(ride_duration),
            total_rides = n())

# Print aggregated results
print(ride_duration_agg)

# Visualize results
ggplot(ride_duration_agg, aes(x = day_of_week, y = avg_ride_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Ride Duration by User Type and Day of the Week",
       x = "Day of the Week",
       y = "Average Ride Duration (minutes)") +
  scale_fill_manual(values = c("member" = "blue", "casual" = "orange")) +
  theme_minimal()

# Save the plot
ggsave("avg_ride_duration_plot.png")



### Analyzing Ride Count by User Type and Time of Day ###

# Create time_of_day bins
cleanedcyclisticdata3$time_of_day_bin <- cut(as.numeric(cleanedcyclisticdata3$time_of_day), breaks = 4, labels = c("Early Morning", "Morning", "Afternoon", "Evening"))

# Aggregate ride counts by user type and time of day bin
ride_count_agg <- cleanedcyclisticdata3 %>%
  group_by(member_casual, time_of_day_bin) %>%
  summarize(total_rides = n())

# Print aggregated results
print(ride_count_agg)

# Visualize results
ggplot(ride_count_agg, aes(x = time_of_day_bin, y = total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by User Type and Time of Day",
       x = "Time of Day",
       y = "Number of Rides") +
  scale_fill_manual(values = c("member" = "blue", "casual" = "orange")) +
  theme_minimal()

# Save the plot
ggsave("ride_count_plot.png")


### Analyzing Ride Count by User Type and Season ###

# Aggregate ride counts by user type and season
ride_count_season_agg <- cleanedcyclisticdata3 %>%
  group_by(member_casual, season) %>%
  summarize(total_rides = n())

# Print aggregated results
print(ride_count_season_agg)

# Visualize results
ggplot(ride_count_season_agg, aes(x = season, y = total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by User Type and Season",
       x = "Season",
       y = "Number of Rides") +
  scale_fill_manual(values = c("member" = "blue", "casual" = "orange")) +
  theme_minimal()

# Save the plot
ggsave("ride_count_season_plot.png")


### Top 10 Most Popular Start Stations by User Type ###

# Aggregate data for members
top_10_member_start_stations <- cleanedcyclisticdata3 %>%
  filter(member_casual == "member") %>%
  group_by(start_station_name) %>%
  summarize(total_rides = n()) %>%
  top_n(10, total_rides) %>%
  arrange(desc(total_rides))


# Aggregate data for casual riders
top_10_casual_start_stations <- cleanedcyclisticdata3 %>%
  filter(member_casual == "casual") %>%
  group_by(start_station_name) %>%
  summarize(total_rides = n()) %>%
  top_n(10, total_rides) %>%
  arrange(desc(total_rides))

# Print results
print(top_10_member_start_stations)
print(top_10_casual_start_stations)
