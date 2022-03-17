rm(list=ls());                         # clear Environment tab
options(show.error.locations = TRUE);  # show line numbers on error
library(package=ggplot2);              # get the GGPlot package

# read in CSV file and save the content to weatherData
weatherData = read.csv(file="data/Lansing2016NOAA.csv", 
                       stringsAsFactors = FALSE);  # for people still using R v3

# 1.
plot1 = ggplot( data=weatherData ) +
  geom_point( mapping=aes(x=avgTemp, y=relHum, shape=season, fill = season)) +
  scale_shape_manual(values = c(21, 22, 23, 24)) +
  theme_bw() +
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Temperature (\u00B0F)",  
       y = "Humidity (\u0025)")
plot(plot1)

plot2 <- plot1 +
  geom_smooth( mapping = aes(x=avgTemp, y=relHum, color = season), method = "lm",
               se=FALSE) #remove confidence intervals
plot(plot2)

#2.
windSpeedOrder <- factor(weatherData$windSpeedLevel, 
                         levels = c("Low", "Medium", "High"))

plot3 <- ggplot(data = weatherData) +
  geom_histogram(mapping = aes(x = relHum, color = windSpeedOrder))
plot(plot3)

plot4 <- plot3 +
  scale_color_manual(values = c("green", "yellow", "red"))
plot(plot4)

plot5 <- plot4 +
  theme(panel.background = element_rect(fill = "black"))
plot(plot5)

#3.
plot6 <- ggplot(data = weatherData) +
  geom_point(mapping = aes(x = avgTemp, y = relHum, color = stnPressure),
             size = 2,
             alpha = .5) +
  scale_color_gradientn(colors = c("red", "yellow", "blue"),
                        values = c(0, 0.33, 1))

plot(plot6)


### Questions
#1. 65%
#2. to go through the lesson and application ~1.5-2 hr
#3. distinguishing what is mapped and what is a new subcomponent; especially for 
   # colors/fill
#4.scale ColorBrewer






