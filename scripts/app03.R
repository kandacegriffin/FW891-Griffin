# Lesson 3 Homework
# Kandace Griffin

options(show.error.locations = TRUE);  # show line numbers on error
library(package=ggplot2);              # get the GGPlot package
library(cowplot)

# read in CSV file and save the content to weatherData
weatherData = read.csv(file="data/Lansing2016NOAA.csv", 
                       stringsAsFactors = FALSE); # for people still using R v3

#### Part 10: Application ####

## A.
plot9 = ggplot( data=weatherData ) +
  geom_smooth( mapping=aes(x=avgTemp, y=relHum, color = season), 
               method="lm" ) +
  geom_point( mapping=aes(x=avgTemp, y=relHum, size=precip2, color=season) ) +
  labs( title="Humidity vs Temperature",
        subtitle="Lansing, MI -- 2016",
        x = "Average Temperatures (Fahrenheit)",
        y = "Relative Humidity",
        size = "Precipitation",
        color = "Seasons") +    # changes order
  scale_x_continuous( breaks = seq(from=10, to=80, by=10) ) +
  theme_bw() +
  theme( axis.text.x=element_text(angle=90, vjust=0.5) ,
         legend.position = "none");
plot(plot9)

# color=season creates separate linear models for each season - good for tracking
# trends over each season rather than over the entire year

## B.
plot10 <- ggplot( data = weatherData ) +
  geom_point(mapping = aes(x = windSpeed, y = tempDept, size = precip2, 
                           color = windDir)) +
  labs( title = "Temp Deviation From Average vs. Wind Speed",
        subtitle = "Lansing, MI -- 2016",
        x = "Wind Speed (mph)",
        y = "Temp Deviation From Average (Fahrenheit)",
        size = "Precipitation",
        color = "Wind Direction") +
  theme_cowplot() +
  theme(legend.position = c(.85,.25))
plot(plot10)  
  
# wide range of wind speed with western direction; most precipitation associated
# with 7 - 15 mph wind speed; typically more precip when temp higher than average

##C.
plot11 <- ggplot( data = weatherData ) +
  geom_point(mapping = aes(x = windSpeed, y = tempDept, size = precip2, 
                           color = windDir, shape = season)) +
  labs( title = "Wind Speed vs. Temp Deviation From Average",
        subtitle = "Lansing, MI -- 2016",
        x = "Wind Speed (mph)",
        y = "Temp Deviation From Average (Fahrenheit)",
        size = "Precipitation",
        color = "Wind Direction",
        shape = "Season") +
  theme_cowplot()
plot(plot11)

### Questions to answer
#1. 8/10
#2. ~1 hr - mostly messing with the mapping and axes
#3. before I've seen the mapping / aes called in the ggplot() line - how does it differ this way? is one preferred?
#4. adding line segments; stroke sub-component