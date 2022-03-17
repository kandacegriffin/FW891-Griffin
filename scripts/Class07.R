
rm(list=ls());                         # clear Environment tab
options(show.error.locations = TRUE);  # show line numbers on error
library(package=ggplot2);              # get the GGPlot package
library(package=ggforce);              # for geom_circle, geom_ellipse

# read in CSV file and save the content to weatherData
weatherData = read.csv(file="data/Lansing2016NOAA.csv");

### Re-order the directions on the x-axis using factor(s)
windDirOrdered = factor(weatherData$windDir,
                        levels=c("North", "East", "South", "West"));

######  Group 1: ##############################################
# - Redo HW 7 #1 using the 4 different methods of using data
# - save as Class7.R
# - Come up with questions
###############################################################


# Method 1: data global, mapping local

xVector <- c(2, 2, 2)
yVector <- c(-30, 20, 25)

plot1a = ggplot(data=weatherData) +
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

plot(plot1a)

# Method 2: data and mapping global

plot1b = ggplot(data=weatherData, 
               mapping=aes(x=windDirOrdered, y=changeMaxTemp),
               na.rm = TRUE) +
  geom_boxplot() +
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

plot(plot1b)

# Method 3:data and mapping in plot components

plot1c = ggplot() +
  geom_boxplot(data=weatherData, 
               mapping=aes(x=windDirOrdered, y=changeMaxTemp),
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

plot(plot1c)

# Method 4: explicit mapping in plot components

plot1d = ggplot() +
  geom_boxplot(mapping=aes(x=windDirOrdered, 
                           y=weatherData$changeMaxTemp),
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

plot(plot1d)


######  Group 2: ##############################################
# - Continue working in the script file Class7.R
# - create a scatterplot of pressure vs humidity
# - facet_wrap two other variables to create between 9 and 30 plots
#    - hint: https://ggplot2.tidyverse.org/reference/vars.html
#    - change either the number of rows or columns in the facet_wrap
#    - hint: https://ggplot2.tidyverse.org/reference/facet_wrap.html
#    - modify one other argument (aside from rows and columns) in facet_wrap 
#    - explain what the other argument does in comments
# - facet_grid the same two variable for the same scatterplot
###############################################################

plot2a <- ggplot(data = weatherData) +
  geom_point(mapping = aes(x = relHum, y = stnPressure)) +
  facet_wrap(facets = vars(season, windDir),
             nrow = 6,
             scales = "free") # allows scales to vary across each panel/facet
plot(plot2a)


plot2b <- ggplot(data = weatherData) +
  geom_point(mapping = aes(x = relHum, y = stnPressure)) +
  facet_grid(rows = vars(season), cols = vars(windDir)) 
plot(plot2b)
