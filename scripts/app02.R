# Lesson 2 Homework
# Kandace Griffin

rm(list=ls());                         # clear Environment tab
options(show.error.locations = TRUE);  # show line numbers on error
library(package=ggplot2);              # get the GGPlot package

#### Questions
# 1. geom_text
# 2. scale_x_date


#### Create new scatterplot

# read in CSV file and save the content to weatherData
weatherData = read.csv(file="data/Lansing2016NOAA.csv");

range(weatherData$stnPressure)
range(weatherData$windSpeed)

plot6 <- ggplot( data=weatherData ) +
  geom_point( mapping=aes(x=stnPressure, y=windSpeed) ) +
  labs( title="Pressure vs Wind Speed",
        subtitle="Lansing, MI -- 2016",
        x = "Pressure",
        y = "Average Wind Speed") +
  scale_x_continuous( limits = c(28.6, 29.6),
                      breaks = seq(from=28.5, to=29.7, by=.1) ) +
  scale_y_continuous( limits = c(2, 21),
                      breaks = c(3, 12, 21)) +
  theme_classic() +
  theme( axis.text.x = element_text(angle=45, vjust=0.5) )
  
plot(plot6)

#### Questions Part 2
# 1. I feel 85% confident in lesson.
# 2. Still pissed about complete theme_xxx and theme() throwing everything off
# 3. changing colors; building in stats
