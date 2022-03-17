rm(list=ls());                         # clear Environment tab
options(show.error.locations = TRUE);  # show line numbers on error
library(package=ggplot2);              # get the GGPlot package
library(dplyr)
library(readr)

# read in CSV file and save the content to weatherData
weatherData = read.csv(file="data/Lansing2016NOAA.csv", 
                       stringsAsFactors = FALSE)

positions <- read_csv( file = "data/Fish_Positions.csv")

positions$tag_id <- as.character(positions$tag_id)

#filter only pressure tags (for depth plots)
press <- positions %>%
  filter(!is.na(fish_height_uncalibrated))

#calculate water depth from water elevation and bathymetry elevation
press <- press %>%
  mutate(water_depth = water_elevation_navd88 - bath_elevation_navd88)



#plot 1:
# 1 modification to plot point with 3 unique properties
# 3 element_text() subcomponenets
# 3 uses of unicode
# 4 uses of rbg colors
# 1 use of alpha

plot1 <- ggplot(data = press) +
  geom_point(mapping = aes(x = water_depth, y = fish_height_calibrated),
             shape = "\u002A",
             size = 3.5,
             alpha = .5,
             color = rgb(red=0, green=0, blue=0.3)) +
  labs(title = "Fish Height vs. Water Depth",
       subtitle = "White River, Michigan: 2021",
       x = "Water Depth (\u006D)",
       y = "Fish Height from Bottom (\u006D)") +
  theme(axis.title.x=element_text(size=14, color= rgb(red = 1, green = 0, blue = 0)),
        axis.title.y=element_text(size=14, color= rgb(red = 0, green = 1, blue = 0)), 
        plot.title=element_text(size=18, face="bold", 
                                color =rgb(red = .5, green = 0, blue = 0.5)))
  
plot(plot1)

#plot 2:
# 1 modification to plot points with 3 unique properties
# 2 element_text() subcomponents
# 2 element_line() subcomponents
# 2 element_rect() subcomponents
# 2 unicode use
# 2 greyscale use

plot2 <- ggplot(data = filter(press,hpe_s < 15)) +
  geom_point(mapping = aes(x = water_depth, y = fish_height_calibrated, 
                           color = hpe_s),
             shape = 18,
             size = .7,
             alpha = .75) + 
  labs(title = "\u0046ish Height vs. Water Depth",
         subtitle = "White River, Michigan: 2021",
         x = "Water Depth (m)",
         y = "Fish Height from Bottom (m)",
         color = "\u0048PE") +
  theme_minimal() +
  theme(panel.grid.minor = element_line(color="grey80", linetype = 2),
        panel.grid.major = element_line(color="grey50"),
        legend.background = element_rect(color = "blue"),
        legend.title = element_text(face = "bold"),
        plot.title=element_text(size=20, face="bold"),
        panel.background = element_rect(color="red")) 
plot(plot2)   

#plot 3:
# 1 modification to plot points with 3 unique properties
# 3 element_text() subcomponents
# 2 element_line() subcomponents
# 1 element_rect() subcomponents
# 1 greyscale use

plot3 <- ggplot(data = filter(press,hpe_s < 15)) +
  geom_point(mapping = aes(x = fish_height_uncalibrated, y = fish_height_calibrated,
                           color = hpe_s),
             shape = 16,
             alpha = .25,
             size = .75) +
  theme_bw() + 
  labs(title = "Calibrated vs. Uncalibrated Fish Height",
       subtitle = "White River, Michigan: 2021",
       x = "Uncalibrated(m)",
       y = "Calibrated (m)",
       color = "HPE") +
  theme(plot.background = element_rect(fill = "grey75"),
        legend.title = element_text(face = "italic"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(face = "bold"),
        axis.ticks.x = element_line(color = "purple", size = 2),
        axis.ticks.y = element_line(color = "blue", size = 2))
plot(plot3)

#plot 4

# 1 modification to plot points with 3 unique properties
# 1 modification to plot lines with 3 unique properties
# 1 element_rect()
# 1 alpha use

plot4 <- ggplot(data = filter(press,hpe_s < 15)) +
  geom_point(mapping = aes(x = water_depth, y = fish_height_uncalibrated, 
                           color = sex),
             shape = 16,
             size = 2,
             alpha = .75) + 
  geom_smooth(mapping=aes(x= water_depth, y=fish_height_uncalibrated),
              method="lm",
              color="black",  
              linetype=1,
              size=.75) +
  labs(title = "Fish Height vs. Water Depth by Sex",
       subtitle = "White River, Michigan: 2021",
       x = "Water Depth (m)",
       y = "Fish Height from Bottom (m)",
       color = "Sex") +
  theme_minimal() +
  theme(legend.background = element_rect(color = "blue"))
         
plot(plot4)

#plot 5
# 1 modification to plot points w/ 3 unique properties
# 1 modification to plot lines w/ 3 unique properties

plot5 <- ggplot(data = positions) +
  geom_point(mapping = aes(x = weight, y = length),
             size = 2,
             shape = 15,
             color = "purple") +
  labs(title = "Fish Length vs. Weight",
       subtitle = "White River, Michigan: 2021",
       x = "Weight (g)",
       y = "Length (cm)") +
  geom_smooth(mapping=aes(x = weight, y = length),
              method="lm",
              color="black",  
              linetype=2,
              size=.75) +
  theme_bw()
plot(plot5)

#plot 6
# 1 modification to plot points w/ 3 unique properties
# 1 element_rect()

plot6 <- ggplot(data = positions) +
  geom_point(mapping = aes(x = weight, y = length, color = sex),
             size = 2,
             shape = 4,
             alpha = 1) +
  labs(title = "Fish Length vs. Weight",
       subtitle = "White River, Michigan: 2021",
       x = "Weight (g)",
       y = "Length (cm)",
       color = "Sex") +
  theme_bw() +
  theme(legend.background = element_rect(color = "black"))
plot(plot6)

# plot 7

# 2 modification to plot_lines with 3 unique properties

plot7 <- ggplot(data = weatherData) +
  geom_line(mapping=aes(x=1:nrow(weatherData), y=sunset),
            color="violetred1",
            linetype = 2,
            size = 1) +
  geom_line(mapping=aes(x=1:nrow(weatherData), y=sunrise), 
            color="orange",
            linetype = 3,
            size = 1) +
  labs(title = "Sunrise and Sunset Times Throughout the Year",
       subtitle = "Lansing, Michigan: 2016",
       x = "Day",
       y = "Time (24 hr)")
plot(plot7)

# plot 8

# 2 modification to plot_lines with 3 unique properties

plot8 <- ggplot(data = weatherData) +
  geom_line(mapping=aes(x=1:nrow(weatherData), y=windSpeed),
            color="blue",
            linetype = 1,
            size = .5) +
  geom_line(mapping=aes(x=1:nrow(weatherData), y=windPeakSpeed), 
            color="red",
            linetype = 1,
            size = .5) +
  labs(title = "Average and Peak Wind Speed Throughout the Year",
       subtitle = "Lansing, Michigan: 2016",
       x = "Day",
       y = "Wind Speed")
plot(plot8)
  

## Questions
#1. What was your level of comfort with the lesson/application?
  #70%
#2. What areas of the lesson/application confused or still confuses you?
  #rbg colors from 0 to 1 scale
#3. What is a way you can apply the material in this lesson towards your research or area of study?
  #yep, see above - lots of diagnostic plots - check for outliers and 
#4. What are some things you would like to learn related to, but not covered in, this lesson?
  #facets - separating graphs by variable - especially when axes don't exactly line up
  #(eg. I don't have the same number of positions or date for each tag_id, but I would like
  # to look at those plots together).
