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
library(sclaes)

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



# UI
ui <- fluidPage(
  titlePanel("Chart that shows Trips by Hour and Month"),
  fluidRow(
    column(width = 6,
           selectInput("month", "Select Month", choices = unique(result$Month), multiple = TRUE),
           plotOutput("plot1")),
    column(width = 6,
           sliderInput("hour", "Select Hour", min = 0, max = 23, value = 0, step = 1),
           plotOutput("plot2"))
  )
)

# Server
server <- function(input, output) {
  
  # Filter data based on selected month
  filtered_result <- reactive({
    if (is.null(input$month)) {
      return(NULL)
    } else {
      result[result$Month %in% input$month, ]
    }
  })
  
  # Render Plot 1
  output$plot1 <- renderPlot({
    if (is.null(filtered_result())) {
      return()
    }
    p <- ggplot(filtered_result(), aes(Hour, Total, fill = factor(Month))) +
      geom_bar(stat = "identity", color = "red") +
      scale_fill_manual(values = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")) +
      ggtitle("Trips Every Hour") +
      theme(legend.position = "none") +
      scale_y_continuous(labels = comma)
    print(p)
  })
  
  # Render Plot 2
  output$plot2 <- renderPlot({
    if (is.null(filtered_result())) {
      return()
    }
    filtered_result_hour <- filtered_result()[filtered_result()$Hour == input$hour, ]
    p <- ggplot(filtered_result_hour, aes(Hour, Total, color = factor(Month))) +
      geom_line(size = 1) +
      labs(x = "Hour", y = "Trips", color = "Month") +
      ggtitle("Trips by Hour and Month") +
      theme_minimal() +
      scale_color_manual(values = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")) +
      scale_y_continuous(labels = comma)
    print(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
