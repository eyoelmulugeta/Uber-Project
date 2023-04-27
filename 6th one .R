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
library(data.table)
library(DT)
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
=
  
  day_group <- result %>%
  group_by(Day) %>%
  summarize(Total = sum(Total))

datatable(day_group)

day_month_group <- result %>%
  group_by(Month, Day) %>%
  dplyr::summarize(Total = n())

ggplot(result, aes(Day, Total)) +
  geom_bar(stat = "identity", width = 0.7, fill = "steelblue") +
  labs(x = "Day of Month", y = "Trips") +
  ggtitle("Trips by Day of Month") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:31)

day_month_group <- result %>%
  group_by(Month, Day) %>%
  dplyr::summarize(Total = n())
day_month_group$Day <- factor(day_month_group$Day, levels = 1:31)
day_month_group$Month <- factor(day_month_group$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))


# Convert Month to factor data type
result$Month <- as.factor(result$Month)

# Create the chart using ggplot
ggplot(result, aes(Base, Total, fill = Month)) +
  geom_col(position = "dodge", width = 0.8) +
  ggtitle("Trips by Bases and Month") +
  xlab("Bases") +
  ylab("Trips") +
  scale_fill_discrete(name = "Month") +
  theme_minimal()



hours <- 0:23
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
data <- expand.grid(Hour = hours, Day = days)
data$Value <- runif(nrow(data), 1, 100)

ggplot(result, aes(x = Hour, y = Day, fill = Total)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Heatmap by Hour and Day",
       x = "Hour",
       y = "Day") +
  theme_minimal()

library(shiny)
library(ggplot2)

# Create sample data
result <- data.frame(
  Hour = rep(1:24, each = 31),
  Day = rep(1:31, times = 24),
  Total = sample(1:100, size = 24 * 31, replace = TRUE)
)

# UI
ui <- fluidPage(
  titlePanel("Heatmap by Hour and Day"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("hour_select", "Select Hour", min = 1, max = 24, value = c(1, 24), step = 1),
      sliderInput("day_select", "Select Day", min = 1, max = 31, value = c(1, 31), step = 1)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Filter data based on selected hour and day
  filtered_result <- reactive({
    result[result$Hour >= input$hour_select[1] & result$Hour <= input$hour_select[2] &
             result$Day >= input$day_select[1] & result$Day <= input$day_select[2], ]
  })
  
  # Render Plot
  output$plot <- renderPlot({
    p <- ggplot(filtered_result(), aes(x = Hour, y = Day, fill = Total)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Heatmap by Hour and Day",
           x = "Hour",
           y = "Day") +
      theme_minimal()
    print(p)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)