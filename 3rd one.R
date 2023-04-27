library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmnet)
library(pROC)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
library(shiny)
library(leaflet)

rm(list =ls())

setwd("~/Documents/Data 332")


df <-read.csv("~/Documents/Data 332/Uber Data/uber-raw-data-apr14.csv")
df_1 <-read.csv("~/Documents/Data 332/Uber Data/uber-raw-data-aug14.csv")
df_2 <-read.csv("~/Documents/Data 332/Uber Data/uber-raw-data-jul14.csv")
df_3 <-read.csv("~/Documents/Data 332/Uber Data/uber-raw-data-sep14.csv")
df_4 <-read.csv("~/Documents/Data 332/Uber Data/uber-raw-data-jun14.csv")
df_5 <-read.csv("~/Documents/Data 332/Uber Data/uber-raw-data-may14.csv")

combined <- rbind(df, df_1, df_2, df_3, df_4, df_5)
write.csv(combined, "combined_data.csv", row.names = FALSE) 

# Convert the "date_column" to date format with the desired format
# Assuming 'combined' is the name of your data frame
cleaned <- combined %>%
  mutate(Date.Time = strptime(Date.Time, format = "%m/%d/%Y %H:%M:%S"))

# Convert Date/Time column to POSIXct format
combined$Date.Time <- as.POSIXct(combined$Date.Time, format="%m/%d/%Y %H:%M:%S")

# Extract hour and month from Date/Time column
combined$Hour <- hour(combined$Date.Time)
combined$Month <- month(combined$Date.Time)

# Group by Hour and Month and count trips
result <- combined %>%
  group_by(Hour,Month) %>%
  dplyr::summarize(Total = n()) 
datatable(result) 

ggplot(result, aes(Hour, Total, fill =factor(Month))) +
  geom_bar(stat = "identity", color = "red") +
  scale_fill_manual(values = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")) + # Set custom colors for each month
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


ggplot(result, aes(Hour, Total, color = factor(Month))) +
  geom_line(size = 1) +
  labs(x = "Hour", y = "Trips", color = "Month") +
  ggtitle("Trips by Hour and Month") +
  theme_minimal() +
  scale_color_manual(values = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")) + # Set custom colors for each month
  scale_y_continuous(labels = comma)

combined$Date <- as.Date(combined$Date)

# Aggregate data by day of the month
result <- combined %>%
  mutate(Day = day(Date)) %>%
  group_by(Day,Month,Base,Hour,dayofweek,month) %>%
  summarize(Total = n())

# Create a line plot
ggplot(result, aes(Day, Total)) +
  geom_line(size = 1) +
  labs(x = "Day of Month", y = "Trips") +
  ggtitle("Trips by Day of Month") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:31) # Set x-axis breaks to show all days of the month
library(shiny)
library(ggplot2)

# Create sample data
result <- data.frame(
  Day = 1:31,
  Total = sample(1:100, size = 31, replace = TRUE)
)

# UI
ui <- fluidPage(
  titlePanel("Trips by Day of Month"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("day", "Select Day", min = 1, max = 31, value = 1, step = 1)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Render Plot
  output$plot <- renderPlot({
    p <- ggplot(result, aes(Day, Total)) +
      geom_line(size = 1) +
      labs(x = "Day of Month", y = "Trips") +
      ggtitle("Trips by Day of Month") +
      theme_minimal() +
      scale_x_continuous(breaks = 1:31) +
      geom_point(aes(x = input$day, y = result$Total[result$Day == input$day]), color = "red", size = 3) # Highlight selected day with a red point
    print(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)