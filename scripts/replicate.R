rm(list=ls());                         # clear Environment tab
options(show.error.locations = TRUE);  # show line numbers on error
library(package=ggplot2);              # get the GGPlot package

# read in CSV file and save the content to weatherData
weatherData = read.csv(file="data/Lansing2016NOAA.csv", 
                       stringsAsFactors = FALSE)

windDir2 <- factor(weatherData$windDir,
                    levels = c("North", "East", "South", "West"))

plot1 <- ggplot( data=weatherData ) +
  geom_point( mapping=aes(x=relHum, y=avgTemp, color=windSpeed, shape=windDir2),
              size = 3,
              alpha = .7) +
  theme_minimal() +
  labs(title = "Humidity vs Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Humidity (%)",
       y = "Temp (F)") +
  scale_shape_manual(values=c("North" = "~",
                              "East" = "%", 
                              "South" = "@", 
                              "West" = "*")) +
  scale_color_gradientn(colors = c("green", "yellow", "purple"),
                        values = c(0, .25, 1)) +
  scale_x_continuous(breaks = c(50, 60, 70, 80, 90)) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 100))
plot(plot1)

plot2 <- plot1 +
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12, angle = 0, vjust = 0.5),
        plot.title=element_text(size=18, face="bold",
                                color ="darkgreen", hjust = 0.5),
        plot.subtitle=element_text(size=10, face="italic",
                                   color ="black", hjust = 0.5),
        panel.grid.minor = element_line(color="grey25", linetype=4, size = 1),
        panel.grid.major = element_line(color="grey25", size = 1),
        panel.background = element_rect(fill="black"),
        axis.text.x = element_text(angle = 270, color = "red", face = "bold"),
        legend.position = "bottom",
        legend.key = element_rect(color = "blue", fill = "lightblue"),
        legend.title = element_text(color = "darkred", face = "bold"),
        legend.background = element_rect(fill = "grey95", color = "green", 
                                         linetype = 2, size = 1),
        plot.background = element_rect(fill = "lightyellow", color = "black"),
        axis.ticks.y = element_line(color = "purple", size = 5),
        axis.line = element_line(color = "blue", size = 2)
        )
  
plot(plot2)

