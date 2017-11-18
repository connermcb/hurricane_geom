
library(data.table)
library(dplyr)
library(geosphere)
library(ggmap)
library(ggplot2)
library(grid)
library(lubridate)
library(png)
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

test <- ike_2008[ike_2008$latitude == 30.3,]



test <- melt(test[3:9], 
     id.vars = c('latitude', 'longitude', 'wind_speed'),
     measure.vars = c('ne', 'nw', 'sw', 'se')
     ) %>% arrange(wind_speed)


test <- test %>%
  mutate(variable = ifelse(variable=="ne", 0, 
                           ifelse(variable=="nw", 1,
                                  ifelse(variable=="sw", 2, 3)
                                  )
                           )
         )

test


mtrs_per_mile <- 1609.344

data <- data.frame(matrix(nrow=0, ncol=3))
colnames(data) <- c("wind_speed", "longs", "lats")

data

get_coords <- function(d){
  for(i in 1:nrow(d)){
    c <- d[i, ]
    dist_mtrs <- d[i, "value"] * mtrs_per_mile
    for(angle in (0:90)){
      new_points <- destPoint(c(d[i, "longitude"], d[i, "latitude"]), 
                              angle + 90 * d[i, "variable"], dist_mtrs)
      lat <- new_points[1, "lat"][[1]]
      print(lat)
      print(class(lat))
      lon <- new_points[1, "lon"][[1]]
      data <<- rbind(data, setNames(as.list(c(d[i, "wind_speed"], lon, lat)), names(data)))
    }
  }
}
get_coords(test)

data

names(data)
l <- split(d, wind_speed)
coords <- unsplit(do.call(rbind, sapply(l, function(x){})))

test <- test[1:4,]
apply(test, 2, function(x){print(x[[1]])})


lats
longs


data <- data.frame(cbind(longs, lats))
data



get_map("Texas", zoom=6, maptype = "toner-background") %>%
  ggmap(extent="device") +
  geom_polygon(data=data, 
               aes(x=longs, y=lats, 
                   group=wind_speed, color=wind_speed, 
                   fill=wind_speed, alpha=0.5))









