rm(list=ls());                         # clear Environment tab
options(show.error.locations = TRUE);  # show line numbers on error
library(package=ggplot2);              # get the GGPlot package
library(package=ggforce);              # for geom_circle, geom_ellipse

# read in CSV file and save the content to weatherData
weatherData = read.csv(file="data/Lansing2016NOAA.csv", 
                       stringsAsFactors = FALSE);  # for people still using R v3

# 1.
### Re-order the directions on the x-axis using factor(s)
windDirOrdered = factor(weatherData$windDir,
                        levels=c("North", "East", "South", "West"));

xVector <- c(2, 2, 2)
yVector <- c(-30, 20, 25)

plot1 = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDirOrdered, y=changeMaxTemp),
               na.rm = TRUE) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Change in Temperature (\u00B0F)") +
  annotate(geom="point",
           x = xVector,
           y = yVector,
           size = 3,
           color = "black",
           fill = "purple",
           shape = 25) +
  annotate(geom = "rect",
           xmin = c(2.1, 2.1, 2.1),
           xmax = c(2.3, 2.3, 2.3),
           ymin = c(-31.5, 18.5, 23.5),
           ymax = c(-28.5, 21.5, 26.5),
           color = "black") +
  annotate(geom = "text",
           x = c(2.2, 2.2, 2.2),
           y = c(-30, 20, 25),
           label = c(-30, 20, 25),
           color = "yellow") 

plot(plot1)

#2. 

#subset to first 100 data points
first100 <- weatherData[1:100, ]

# Get the mean average temperature and mean relative humidity
meanTemp = mean(first100$avgTemp);
meanHum = mean(first100$relHum);
  
plot2 = ggplot() + 
  theme_bw() +
  annotate(geom="point",
           x = first100$avgTemp,
           y = first100$relHum, 
           size = 3,
           color = "blue",
           fill = "red",
           shape = 21) + 
  labs(title="Scatterplot using annotate",
       x = "Average Temperature",
       y = "Relative Humidity") +
  annotate(geom = "polygon",
           x = c(21, 21, 4, 4, 15) ,
           y = c(60, 85, 74, 63, 60),
           color = "purple",
           fill = NA,
           size = 1.5) +
  geom_vline(mapping=aes(xintercept = meanTemp),
             color = "orange",
             size= 2) +
  geom_hline(mapping=aes(yintercept = meanHum),
             color="green",
             size=2) +
  geom_ellipse(mapping=aes(x0 = 45, y0 = 88, a = 15, b=7.5, angle=100),
               alpha=0.2,   
               color = "grey",
               fill = NA,
               size=1.5)
plot(plot2)


#3.

xValues <- seq(from = 1, to = 1000, by = 1)
yValues <- sqrt(xValues)

plot3 = ggplot() + 
  theme_bw() +
  annotate(geom="line",
           x = xValues,
           y = yValues,
           size = 2,
           arrow = arrow())
plot(plot3)           


#Questions
#1. 70%
#2. 2 hr
#3. circles and ellipse - especially the angle; referencing / extracting the outlier
    # from list
#4. nothing comes to mind
