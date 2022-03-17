# Lesson 8 Application
# Kandace Griffin
# 2022 March 11

rm(list=ls()); # clear Environment tab
options(show.error.locations = TRUE); # show line numbers on error
library(package=ggplot2); # get the GGPlot package
library(package=gridExtra); # for multipaneling
library(package=dplyr)

weatherData = read.csv(file="data/Lansing2016NOAA.csv",
                       stringsAsFactors = FALSE);

# subset to vector based on weather type
rainyDays = grep(weatherData$weatherType, pattern="RA");   # any day with rain
breezyDays = grep(weatherData$weatherType, pattern="BR"); # any breezy day
hazyDays = grep(weatherData$weatherType, pattern="HZ") # any hazy day
rainyAndBreezy = intersect(rainyDays, breezyDays);
rainyAndHazy = intersect(rainyDays, hazyDays);
rainyOrBreezy = union(rainyDays, breezyDays);
rainyOrHazy = union(rainyDays, hazyDays);
rainyNotBreezy = setdiff(rainyDays, breezyDays);
breezyNotRainy = setdiff(breezyDays, rainyDays);

#histogram of tempDept on rainy days
#base index way
plot1 <- ggplot(data = weatherData[rainyDays,]) +
              geom_histogram(mapping = aes(x=tempDept))
plot(plot1)

#dpylr way - makes more intuitive sense to me
plot1b <- ggplot(data = weatherData %>% filter(grepl(pattern = "RA", weatherType))) +
                   geom_histogram(mapping = aes(x=tempDept))
plot(plot1b)                  


#histogram of tempDept on breezy days
plot2 <- ggplot(data = weatherData[breezyDays,]) +
              geom_histogram(mapping = aes(x=tempDept))


#histogram of tempDept on hazy days
plot3 <- ggplot(data = weatherData[hazyDays,]) +
           geom_histogram(mapping = aes(x=tempDept))

#histogram of tempDept on rain AND hazy days
plot4 <- ggplot(data = weatherData[rainyAndHazy,]) +
            geom_histogram(mapping = aes(x=tempDept))

#histogram of tempDept on rain OR hazy days
plot5 <- ggplot(data = weatherData[rainyOrHazy,]) +
  geom_histogram(mapping = aes(x=tempDept))


## adding vertical line at avg tempDept and label with avg value

#get average tempDept for each histogram
rainyAvg <- round(mean(weatherData[rainyDays,]$tempDept), digits = 3)
breezyAvg <- round(mean(weatherData[breezyDays,]$tempDept), digits = 3)
hazyAvg <- round(mean(weatherData[hazyDays,]$tempDept), digits = 3)
rainyAndHazyAvg <- round(mean(weatherData[rainyAndHazy,]$tempDept), digits = 3)
rainyOrHazyAvg <- round(mean(weatherData[rainyOrHazy,]$tempDept), digits = 3)

# add line and label to each histogram
plot1a <- plot1 +
            geom_vline(xintercept = rainyAvg,
                       color = "red") +
            geom_text(mapping = aes(x = rainyAvg+1.5, y = 11,  
                                    label = rainyAvg),
                      color = "red")
plot(plot1a)

plot2a <- plot2 +
            geom_vline(xintercept = breezyAvg,
                       color = "red") +
            geom_text(mapping = aes(x = breezyAvg+1.5, y = 15,  
                                    label = breezyAvg),
                      color = "red")
plot(plot2a)

plot3a <- plot3 +
              geom_vline(xintercept = hazyAvg,
                         color = "red") +
              geom_text(mapping = aes(x = hazyAvg+1.5, y = 4.2,  
                                      label = hazyAvg),
                        color = "red")
plot(plot3a)

plot4a <- plot4 +
              geom_vline(xintercept = rainyAndHazyAvg,
                         color = "red") +
              geom_text(mapping = aes(x = rainyAndHazyAvg+1.5, y = 2,  
                                      label = rainyAndHazyAvg),
                        color = "red")
plot(plot4a)

plot5a <- plot5 +
              geom_vline(xintercept = rainyOrHazyAvg,
                         color = "red") +
              geom_text(mapping = aes(x = rainyOrHazyAvg+1.5, y = 14,  
                                      label = rainyOrHazyAvg),
                        color = "red")
plot(plot5a)

## place all 5 histograms on one canvas

multi1=arrangeGrob(plot1a, plot2a, plot3a, plot4a, plot5a,
                   nrow=3);
plot(multi1);

## place 3 histograms on 1 canvas and resize at least 2 of them

multi2 <- arrangeGrob(plot1a, plot2a, plot5a,
                      layout_matrix = rbind(c(1,1,2),
                                            c(NA,3,3)))
plot(multi2)


## Questions
# 1. 50%
# 2. 3 hrs
# 3. I'm not great at base r subsetting. it took me a while to figure out how to 
# code the avg tempDept for the restricted data. dplyr is more intuitive to me.
# Also took me a while to wrap my head around how the layout_matrix distributed
# the plots. I think at first I thought the first c(1, 1, 2) corresponded to the 
# first plot (i.e plot 1 was in row 1, column 1, 1 and 2?) NOT the first row 
# contains plots 1, 1, 2. I was very confused going through the lesson and think
# it could be spelled out a bit more clearly.
# 4. unsure right now