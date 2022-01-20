# Class 2
# In class exercise 
# Kandace Griffin

library(package=ggplot2)
library(cowplot)
options(show.error.locations = TRUE)

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
  scale_x_continuous( n.breaks = 5,
                      minor_breaks = FALSE) +
  scale_y_continuous( limits = c(2, 21),
                      breaks = c(3, 12, 21)) +
  theme_cowplot() +
  theme( axis.text.x = element_text(angle=45, vjust=0.5, colour = "blue",
                                    size = 15, family = "mono") )

plot(plot6)

windowsFonts() # fonts loaded into the R package

