```{r}
library(dplyr)
library(grid)
library(lubridate)
library(reshape)
library(tidyr)

## load data file
# file name and set working directory
f <- "ebtrk_atlc_1988_2015.txt"
setwd("c:/users/conner/adv_programming_R")

# read table from text file and set column names
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)

ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

# read file
hrcns <- read.fwf(f, widths = ext_tracks_widths, col.names = ext_tracks_colnames)

## Tidy, clean, and subset
# reformat `storm_id` variable combining name and year
hrcns["storm_id"] = paste(sub("\\s+$", "", hrcns$storm_name), hrcns$year, sep="-")

# combine time and date variables
hrcns["date_time"] <- as_datetime(
  paste(
    paste(hrcns$year, hrcns$month, hrcns$day, sep="/"), 
    paste(hrcns$hour, c(rep("00", 2)), sep = ":")
    )
)

# subset 
subset_vars <- c("storm_id", "date_time", "latitude", "longitude", 
                 paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                 paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                 paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"))

hrcns <- hrcns[subset_vars]
                        
# long form
hrcns <- hrcns%>%
  gather(txt_rd, dist, radius_34_ne:radius_64_nw, na.rm=TRUE)


hrcns <- hrcns%>%
  separate(txt_rd, c("txt", "wind_speed", "wind_direction"), sep="_")

hrcns <- hrcns[, -5]

hrcns <- hrcns%>%
  spread(wind_direction, dist)%>%
  arrange(storm_id, date_time)

hrcns["wind_speed"] <- as.integer(hrcns$wind_speed)

hrcns["longitude"] <- hrcns["longitude"] * -1

head(hrcns)

ike_2008 <- hrcns[hrcns$storm_id=="IKE-2008",]
test <- ike_2008[ike_2008$latitude == 22.0,]

test
test <- melt(test[3:9], 
     id.vars = c('latitude', 'longitude', 'wind_speed'),
     measure.vars = c('ne', 'nw', 'sw', 'se')
     ) %>% arrange(wind_speed)


dists <- test[,6:9]
lat <- test[1, 3]
long <- test[1, 4]
lats <- c()
longs <- c()

for(grp in 1:3){
  temp_lats <- c()
  temp_longs <- c()
  for(quad in 0:3){
    for(angle in (0:90 +(90 * quad))){
      new_lat <- lat + (dists[grp, quad+1] * cos(angle)/111111)
      temp_lats <- c(temp_lats, new_lat)
      new_long <- long + (dists[grp, quad+1] * sin(angle)/cos(lat)/111111)
      temp_longs <- c(temp_longs, new_long)
    }
  }
  lats <- c(lats, temp_lats)
  longs <- c(longs, temp_longs)
}
lats
longs
test_poly <- polygonGrob(longs, lats, gp = gpar(lty=1, lwd=0.5))
grid.draw(test_poly)
windows()
grid.newpage()
dev.list()
my_circle <- circleGrob(x=0.5, y=0.5, r=0.5, gp=gpar(col="gray", lty=3))
grid.draw(my_circle)
library(ggplot2)
data <- data.frame(cbind(longs, lats))
data
ggplot(data=data)+
  geom_polygon(aes(x=longs, y=lats, group=1))
library(grid)
library(png)

img.path <- system.file("img", "Rlogo.png", package="png")
bg <- readPNG(img.path)
background <- rasterGrob(unclass(bg))

grid.draw(background)
```









