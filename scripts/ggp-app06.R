rm(list=ls());                         # clear Environment tab
options(show.error.locations = TRUE);  # show line numbers on error
library(package=ggplot2);              # get the GGPlot package

### Use application from boxplots2 ####

### Computed variables -- do with geom_text (2 lessons?) ###

# read in CSV file and save the content to weatherData
weatherData = read.csv(file="data/Lansing2016NOAA.csv", 
                       stringsAsFactors = FALSE); # for people still using R v3

seasonOrdered = factor(weatherData$season,
                       levels=c("Spring", "Summer", "Fall", "Winter"))

weatherData$seasonOrdered = seasonOrdered

# 1.
windSpeedOrdered = factor(weatherData$windSpeedLevel,
                          levels=c("High", "Medium", "Low"))

windDirOrdered = factor(weatherData$windDir,
                        levels=c("North", "South", "East", "West"))

weatherData$windSpeedOrdered = windSpeedOrdered

weatherData$windDirOrdered = windDirOrdered

plot1 = ggplot( data=weatherData ) +
  geom_point( mapping=aes(x = relHum, y=avgTemp)) +  
  theme_bw() +
  facet_grid( rows = vars(windDirOrdered), 
              cols = vars(windSpeedOrdered) ) + 
  labs(title = "Temperature (\u00B0F) vs. Humidity",
       subtitle = "Lansing, Michigan: 2016",
       x = "Temperature (\u00B0F)",
       y = "Humidity");     
plot(plot1)

# -----------------------

#2.

plot2 = ggplot( data=weatherData ) +
  geom_histogram( mapping=aes(x=avgTemp, fill = season),
                  color="black",
                  bins = 15 ) +  
  theme_bw() +
  facet_grid( rows=vars(season) ) + 
  labs(title = "Temperature (\u00B0F)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Temperature (\u00B0F)") +
  theme( strip.background = element_rect(fill = "cyan", color = "magenta"),
         strip.text = element_text(size = 12,
                                   color =  "orangered",
                                   family = "serif")) +
  scale_fill_manual(values=c("Spring" = "green",
                              "Summer" = "red", 
                              "Fall" = "orange", 
                              "Winter" = "purple")) +
  scale_x_continuous(breaks = c(5, 15, 25, 35, 45, 55, 65, 76, 85),
                     limits = c(0, 90))
plot(plot2)

# took me a long time to figure out it needed to be scale_fill_manual - not
# something we went over


# ----------------------
#3.

windSpeedOrdered = factor(weatherData$windSpeedLevel,
                          levels=c("Low", "Medium", "High"))

windDirOrdered = factor(weatherData$windDir,
                        levels=c("North", "East", "South", "West"))

weatherData$windSpeedOrdered = windSpeedOrdered

weatherData$windDirOrdered = windDirOrdered

fillColors = c("lightyellow", NA, "lightyellow", NA,
               "lightyellow", NA, "lightyellow", NA,
               "lightyellow", NA, "lightyellow", NA)


boxColors = c(rep("black", 8), #first 8 black
              rep("blue", 3), #next 3 blue
              "black");#last one black

plot3 = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDirOrdered, y=changeMaxTemp),
               na.rm = TRUE,  
               color = boxColors, #this was here before
               fill = fillColors,
               outlier.size = 3,
               outlier.color = rgb(1, .5, 0),
               outlier.shape = "&") +
  theme_bw() +
  facet_grid( cols=vars(windSpeedOrdered)) + # I reordered them, but didn't use it in the plot - smh
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(plot3)


# ---------------

#4. 

#Removing outlier from boxplot

outlier.shape = NA
outlier.size = 0
outlier.color = NA
outlier.alpha = 0


#-----------------
## Questions ##

#1. 70% 
#2. 1.5hr
#3. still get confused when to use fill and color - likely just need practice; 
# when / why some variables can be in the environment and some need to be in data frame
#4. facet_wrap applications vs. facet_grid - when to use each and why; 
  #I've seen facet_grid(~VARIABLE) how is this different? why use one over other?
  #changing scales across each panel

