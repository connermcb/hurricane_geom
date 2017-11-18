# import libraries
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




GeomHURRICANE <- ggproto("GeomHURRICANE", Geom,
                         required_aes = c("x", "y", "group"),
                         default_aes = aes(colour=NA, fill=NA, size=0.5, linetype=1, 
                                           alpha=NA),
                         draw_key = draw_key_polygon,
                         draw_panel = function (data, panel_scales, coord){
                           n <- nrow(data)
                           if (n == 1) 
                             return(zeroGrob())
                           
                           munched <- coord$transform(data, panel_scales)
                           print(str(munched))
                           munched <- munched[order(munched$group), ]
                           first_idx <- !duplicated(munched$group)
                           first_rows <- munched[first_idx, ]
                           ggplot2:::ggname("geom_hurricane", 
                                            polygonGrob(munched$x, munched$y, 
                                                        default.units = "native", 
                                                        id = munched$group, 
                                                        gp = gpar(col = first_rows$colour,  
                                                                  fill = alpha(first_rows$fill, 
                                                                               first_rows$alpha), 
                                                                  lwd = first_rows$size * .pt, 
                                                                  lty = first_rows$linetype)))
                           
                         }
)

geom_hurricane <- function (mapping = NULL, data = NULL, stat = "identity", 
                            position = "identity", ..., na.rm = FALSE, 
                            show.legend = NA, inherit.aes = TRUE){
  test <- melt(data[3:9], 
               id.vars = c('latitude', 'longitude', 'wind_speed'),
               measure.vars = c('ne', 'nw', 'sw', 'se')
  ) %>% arrange(wind_speed)
  test <- test %>%
    mutate(variable = ifelse(variable=="ne", 0, 
                             ifelse(variable=="nw", 1,
                                    ifelse(variable=="sw", 2, 3)
                             )))
  
  
  mtrs_per_mile <- 1609.344
  
  data2 <- data.frame(matrix(nrow=0, ncol=3))
  colnames(data2) <- c("wind_speed", "longitude", "latitude")
  get_coords <- function(d){
    for(i in 1:nrow(d)){
      c <- d[i, ]
      dist_mtrs <- d[i, "value"] * mtrs_per_mile
      for(angle in (0:89)){
        new_points <- destPoint(c(d[i, "longitude"], d[i, "latitude"]), 
                                angle + 90 * d[i, "variable"], dist_mtrs)
        lat <- new_points[1, "lat"][[1]]
        lon <- new_points[1, "lon"][[1]]
        data2 <<- rbind(data2, setNames(as.list(c(d[i, "wind_speed"], lon, lat)), names(data2)))
      }
    }
  }
  
  
  get_coords(test)
  
  ggplot2::layer(data = data2, mapping = mapping, stat = stat, geom = GeomHURRICANE, 
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                 params = list(na.rm = na.rm, ...))
}

get_map(location = c(lon=test[1,"longitude"], lat=test[1,"latitude"]),
        zoom=6, maptype = "toner-background") %>%
  ggmap(extent="device")+
  geom_hurricane(data=ike2008,
                 aes(x=longitude, y=latitude,
                     group=wind_speed, colour=factor(wind_speed),
                     fill=factor(wind_speed)), alpha=0.5)+
  scale_color_manual(name="Wind Speed (knts)",
                     values=c("red","orange", "yellow"))+
  scale_fill_manual(name="Wind Speed (knts)",
                    values=c("red", "orange", "yellow"))
