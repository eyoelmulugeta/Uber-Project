# Uber-Project
## Done By: Eyoel Mulugeta 

## Intorduction 

Examination of the dataset for Uber pickups in New York City.

        hour_month <- combined %>%
        group_by(Hour,Month) %>%
        dplyr::summarize(Total = n()) 
      write.csv(hour_month, "result_hour_month.csv")
      datatable(hour_month)

## Chart that shows Trips by Hour and Month

              combined$Date <- as.Date(combined$Date)
            ggplot(hour_month, aes(Hour, Total, fill =factor(Month))) +
              geom_bar(stat = "identity", color = "red") +
              scale_fill_manual(values = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")) + # Set custom                 colors for each month
              ggtitle("Trips Every Hour") +
              theme(legend.position = "none") +
              scale_y_continuous(labels = comma)

<img width="1440" alt="Screen Shot 2023-04-25 at 3 17 38 PM" src="https://user-images.githubusercontent.com/112992643/234393631-caad9546-919e-4e61-860f-29db7005cf86.png">

The line chart displays the total number of trips by hour and month. It consists of multiple lines, each representing a different month, and shows how the number of trips changes over the hours of the day for each month.


The x-axis represents the hour of the day, ranging from 0 (midnight) to 23 (11 PM). The y-axis represents the total number of trips. Each line on the chart represents a different month, with a unique color to differentiate between them.

By examining the lines, you can observe how the number of trips varies by hour of the day for each month. For example, you may notice that there are higher trip counts during certain hours of the day in specific months, while other hours have lower trip counts. This information can help identify patterns or trends in the data, such as peak hours of demand or changes in travel behavior by month.


## Chart that displays Trips Every Hour.

        ggplot(hour_month, aes(Hour, Total, color = factor(Month))) +
          geom_line(size = 1) +
          labs(x = "Hour", y = "Trips", color = "Month") +
          ggtitle("Trips by Hour and Month") +
          theme_minimal() +
          scale_color_manual(values = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")) + # Set custom colors for each month
          scale_y_continuous(labels = comma)

        combined$Date <- as.Date(combined$Date)

<img width="1440" alt="Screen Shot 2023-04-25 at 3 18 12 PM" src="https://user-images.githubusercontent.com/112992643/234394127-81c78313-9ac2-4343-8c64-68ea181181bd.png">


The chart is a bar chart that shows the total number of trips for each hour of the day. It consists of vertical bars, with each bar representing a different hour of the day. The height of each bar represents the total number of trips that occurred during that specific hour.

The x-axis represents the hours of the day, ranging from 0 (midnight) to 23 (11 PM). The y-axis represents the total number of trips. Each bar on the chart represents a different hour of the day, and the height of the bar indicates the total number of trips that occurred during that hour.

By examining the heights of the bars, you can see the distribution of trips throughout the day. This can help identify patterns, trends, or peaks in trip counts during certain hours, which can be useful for understanding travel patterns and demand for transportation services during different times of the day.


## Plot data by trips taken during every day of the month.

        day_group <- combined %>%
          mutate(Day = day(Date)) %>%
          group_by(Hour,Month,Base,Day) %>%
          dplyr::summarize(Total = n()) 
        write.csv(day_group, "result_hour_month.csv")

        # Create a line plot
        ggplot(day_group, aes(Day, Total)) +
          geom_line(size = 1) +
          labs(x = "Day of Month", y = "Trips") +
          ggtitle("Trips by Day of Month") +
          theme_minimal() +
          scale_x_continuous(breaks = 1:31) # Set x-axis breaks to show all days of the month

<img width="1440" alt="Screen Shot 2023-04-25 at 3 18 28 PM" src="https://user-images.githubusercontent.com/112992643/234394590-aa805634-e8d3-4a87-8baf-97a891506e3f.png">


The chart is a line chart that shows the total number of trips for each day of the month. It consists of a line that connects data points, with each data point representing a different day of the month. The position of each data point on the chart represents the day of the month along the x-axis, and the total number of trips on that day along the y-axis.

The x-axis represents the days of the month, ranging from the 1st day to the last day of the month. The y-axis represents the total number of trips. The line on the chart connects the data points, showing the trend of trips taken over the course of the month.

## table that shows Trips Every Day

<img width="1440" alt="Screen Shot 2023-04-25 at 3 18 43 PM" src="https://user-images.githubusercontent.com/112992643/234395246-bee197a2-6a00-4baa-a704-37ed5fab64ca.png">


A table that shows trips every day presents data in a tabular format with rows for each day and columns for attributes such as date and total trips. It allows for easy comparison and analysis of daily trip counts.

By examining the shape of the line and the position of the data points, you can see the overall trend of trips taken during the month. This can help identify patterns, trends, or peaks in trip counts during certain days of the month, which can be useful for understanding travel patterns and demand for transportation services throughout the month.

## Chart by Trips by Day and Month 

        day_group <- combined %>%
          mutate(Day = day(Date)) %>%
          group_by(Hour,Month,Base,Day) %>%
          dplyr::summarize(Total = n()) 
        write.csv(day_group, "result_hour_month.csv")

        ggplot(day_group, aes(Day, Total)) +
                  geom_bar(stat = "identity", width = 0.7, fill = "steelblue") +
                  labs(x = "Day of Month", y = "Trips") +
                  ggtitle("Trips by Day of month") +
                  theme_minimal() +
                  scale_x_continuous(breaks = 1:31)

<img width="1440" alt="Screen Shot 2023-05-02 at 7 17 46 PM" src="https://user-images.githubusercontent.com/112992643/235811632-3b879524-0f15-4856-a968-a4c9f44a38ea.png">

This plot shows the total number of trips taken each day of the month. The x-axis represents the day of the month, while the y-axis shows the total number of trips taken. The line connecting the data points represents the trend of the data throughout the month.

Overall, this plot provides an overview of the variations in the number of trips taken each day of the month, and can be used to identify any patterns or trends in the data.

## Chart Trips by Bases and Month

        base_month_group <- combined %>%
          mutate(Day = day(Date)) %>%
          group_by(Day,Month,Base,Hour) %>%
          summarize(Total = n())

        write.csv(base_month_group, "base_month_group.csv")

        # Convert Month to factor data type
        base_month_group $Month <- as.factor(base_month_group $Month)

        # Create the chart using ggplot
        ggplot(base_month_group , aes(Base, Total, fill = Month)) +
          geom_col(position = "dodge", width = 0.8) +
          ggtitle("Trips by Bases and Month") +
          xlab("Bases") +
          ylab("Trips") +
          scale_fill_discrete(name = "Month") +
          theme_minimal()
          
<img width="1440" alt="Screen Shot 2023-05-02 at 7 23 03 PM" src="https://user-images.githubusercontent.com/112992643/235812270-49002a3c-27cc-4353-b31d-65db80c2965e.png">


The above chart has column chart showing the total number of trips per base for each month. The x-axis shows the bases, and the y-axis shows the number of trips. Each column is split by month, with each color representing a different month. The chart allows for easy comparison of the number of trips per base between different months.

## Heat map that displays by hour and day

        Heatmap_by_Hour_and_Day<- combined %>%
          mutate(Day = day(Date)) %>%
          group_by(Day,Hour) %>%
          summarize(Total = n())
        write.csv(Heatmap_by_Hour_and_Day, "Heatmap_by_Hour_and_Day.csv")

        hours <- 0:23
        days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
        data <- expand.grid(Hour = hours, Day = days)
        data$Value <- runif(nrow(data), 1, 100)


        ggplot(Heatmap_by_Hour_and_Day, aes(x = Hour, y = Day, fill = Total)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "steelblue") +
          labs(title = "Heatmap by Hour and Day",
               x = "Hour",
               y = "Day") +
          theme_minimal()

<img width="1440" alt="Screen Shot 2023-05-02 at 7 30 26 PM" src="https://user-images.githubusercontent.com/112992643/235812853-793e5100-f12c-4ffa-b87c-040a3e2f8410.png">


This plot shows a heatmap of the number of trips taken at different hours of the day and days of the month. The plot is divided into a grid with the x-axis representing the hours of the day and the y-axis representing the days of the month. The color of each tile in the plot represents the number of trips taken during that hour and day, with darker shades of blue indicating higher numbers of trips.
       
## Heat map by month and day

        Heatmap_by_Month_and_Day<- combined %>%
          mutate(Day = day(Date)) %>%
          group_by(Day,Hour,Month) %>%
          summarize(Total = n())
        write.csv(Heatmap_by_Month_and_Day, "Heatmap_by_Month_and_Day.csv")

        ggplot(Heatmap_by_Month_and_Day, aes(Day, Month, fill = Total)) +
          geom_tile(color = "white") +
          ggtitle("Heat Map by Month and Day")
        
<img width="1440" alt="Screen Shot 2023-05-04 at 4 57 14 PM" src="https://user-images.githubusercontent.com/112992643/236339050-4d932ced-346e-4b27-b60c-d0eb9c438dca.png">

The heatmap displays the total number of a given event that occurred on each day of the week and month of the year. The color of the tiles in the heatmap represents the number of events that occurred on a given day and month. The darker the color of the tile, the higher the number of events that occurred. The resulting heatmap provides an easy-to-read visual representation of the data, allowing patterns and trends to be easily identified.

## Heat map by month and week

dayofweek = rep(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), times = 12)

        Heatmap_by_Month_and_Week<- combined %>%
          mutate(Day = day(Date)) %>%
          group_by(dayofweek,Hour,Month) %>%
          summarize(Total = n())
        write.csv(Heatmap_by_Month_and_Week, "Heatmap_by_Month_and_Week.csv")

        ggplot(Heatmap_by_Month_and_Week, aes(dayofweek, Month, fill = Total)) +
          geom_tile(color = "white") +
          ggtitle("Heat Map by Month and Day of Week")

<img width="1440" alt="Screen Shot 2023-05-04 at 5 02 09 PM" src="https://user-images.githubusercontent.com/112992643/236339344-af207aa9-4759-4aef-94d9-102a2b686dcd.png">

The heatmap displays the total number of a given event that occurred on each day of the week and month of the year. The color of the tiles in the heatmap represents the number of events that occurred on a given day and month. we can see the heat map getting lighter as we go from left to right. 

## Heat map Bases and Day of Week

        Heat_Map_by_Bases_and_Day_of_Week<- combined %>%
          mutate(Day = day(Date)) %>%
          group_by(Base,dayofweek) %>%
          summarize(Total = n())
        write.csv(Heat_Map_by_Bases_and_Day_of_Week, "Heat_Map_by_Bases_and_Day_of_Week.csv")

        ggplot(Heat_Map_by_Bases_and_Day_of_Week, aes(Base, dayofweek, fill = Total)) +
          geom_tile(color = "white") +
          ggtitle("Heat Map by Bases and Day of Week")
          
 <img width="1440" alt="Screen Shot 2023-05-04 at 5 05 11 PM" src="https://user-images.githubusercontent.com/112992643/236339809-d036c10a-c6e9-459d-b95b-43e87e5ac8fd.png">
 
This heatmap is a combination of base and day of the week. The x-axis represents the different bases (first, second, and third), while the y-axis represents the days of the week (Monday through Sunday). The heatmap color scale ranges from lighter shades for lower counts to darker shades for higher counts. 

## Geo spatial Map 

This map scatterplot of the latitude and longitude coordinates of Uber rides in New York City during 2014 (April-September). The x-axis represents the longitude values and the y-axis represents the latitude values. The scatterplot shows each Uber ride as a blue point 
   
