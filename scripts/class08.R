rm(list=ls());                         # clear Environment tab
options(show.error.locations = TRUE);  # show line numbers on error
library(package=ggplot2);              # get the GGPlot package
library(package=ggforce);              # for geom_circle, geom_ellipse

#### Two bugs in GGPlot that cause issues with including data and mapping:
#    1) faceting data must come from a declared (local or global)data frame
#    2) annotate (manually adding objects) does not work for all geoms_* 
#       -- hline/vline/circles/ellipses...

# read in CSV file and save the content to weatherData
weatherData = read.csv(file="data/Lansing2016NOAA.csv");
janTemps = read.csv(file="data/LansingJanTemps.csv");  # second data frame


#### Group 1: 
#    Using plotA1, create curve that matches the picture 
#      - Use annotate to create the brown curve
#      - Use geom_curve to create the green curve
#      - need angle, curvature, arrow (subcomponents of geom_curve)
#      -      x, y, xend, yend (aesthetics)
#    https://ggplot2.tidyverse.org/reference/geom_segment.html

#### Part C: Using annotate/geoms to manually add multiple curves #####
plotA1 = ggplot(data=weatherData) +  # generally what I recommend
  theme_bw() +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  geom_smooth(mapping=aes(x=avgTemp, y=relHum),
              method = "lm",
              fill = "red") +
  annotate(geom = "curve",
           x = 65,
           y = 88,
           xend = 82,
           yend = 82,
           color = "brown",
           angle = 90,
           curvature = -0.75,
           size = 1,
           arrow = arrow()) +
  geom_curve(mapping = (aes(x = 36, y = 51, xend = 20, yend = 49)),
             color = "darkgreen",
             angle = 90,
             curvature = -0.75,
             size = 1,
             arrow = arrow())
plot(plotA1);


#### Group 2:
## Using plotA1 (same as group 1)
##  - Create an X that connects to all four corners using geom_abline
##    - being exact is not necessary!
##    - change the color, linetype, size, and opacity of the X
##  - create text using one annotate component in the top-right and top-left corner
##    - make 3 property changes to the text
##  - create text using one geom_text component in the bottom-right and bottom-left corner
##    - make 3 property changes to the text   
##    - try to do this without a legend

plot2 = ggplot(data=weatherData) +  # generally what I recommend
  theme_bw() +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  geom_abline(slope = 0.75,
              intercept = 32) +
  geom_abline(slope = -0.75,
              intercept = 100) +
  geom_smooth(mapping=aes(x=avgTemp, y=relHum),
              method = "lm",
              fill = "red")
plot(plot2)
