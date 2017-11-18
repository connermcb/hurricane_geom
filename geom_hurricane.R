



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



#' @title \code{GeomHURRICANE}
#' 
#' @description 
#' \code{ggproto} object based on \code{GeomPOLYGON} as plotting function for 
#' \code{geom_hurricane}
#' 
#' @import ggplot2
#' @import grid
#' 
#' 
GeomHURRICANE <- ggproto("GeomHURRICANE", Geom,
                         required_aes = c("x", "y", "group"),
                         default_aes = aes(colour=NA, fill=NA, size=0.5, linetype=1, 
                                           alpha=NA),
                         draw_key = ggplot2::draw_key_polygon,
                         draw_panel = function (data, panel_scales, coord){
                           n <- nrow(data)
                           if (n == 1) 
                             return(grid::zeroGrob())
                           
                           trnsfrmd <- coord$transform(data, panel_scales)
                           first_idx <- !duplicated(trnsfrmd$group)
                           first_rows <- trnsfrmd[first_idx, ]
                           ggplot2:::ggname("geom_hurricane", 
                                            grid::polygonGrob(trnsfrmd$x, trnsfrmd$y, 
                                                        default.units = "native", 
                                                        id = trnsfrmd$group, 
                                                        gp = gpar(col = first_rows$colour,  
                                                                  fill = alpha(first_rows$fill, 
                                                                               first_rows$alpha), 
                                                                  lwd = first_rows$size * .pt, 
                                                                  lty = first_rows$linetype)))
                           
                         }
)


#' @title \code{geom_hurricane}
#' 
#' @description
#' \code{geom_hurricane} is a ggplot2 plotting layer designed to plot hurrican wind 
#' speed distributions as radii corresponding to the geographic reach of a 
#' particular wind speed in the four cartesian quadrants. Hurricane wind 
#' radii report how far winds of a certain intensity (e.g., 34, 50, or 64 knots)
#' extended from a hurricane's center, with separate values reported for the 
#' northeast, northwest, southeast, and southwest quadrants of the storm. 
#' 
#' These wind radii are available for Atlantic basin tropical storms since 1988 
#' through the Extended Best Tract dataset, available here: 
#' \link{http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/}
#' 
#' @import dplyr
#' @import geosphere
#' @import ggmap
#' @import ggplot2
#' @import grid
#' @import lubridate
#' @import png
#' @import reshape
#' @import tidyr
#' 
#' @param mapping vector of aesthetic values - x and y plot coordinates, group
#' variable, colour, fill, linetype, size, alpha.
#' @param data dataframe preprocessed using \code{get_hrcn_data}.
#' @param stat transformation function
#' @param position 
#' @param na.rm boolean switch for removing NA values or not
#' @param show.legend
#' @param inherit.aes
#' @param scale_radii plot scaling factor (0, 1] 
#' 
#' @return ggplot object plot layer
#' 
#' @import tidyr
#' @import dplyr
#' @import geosphere
#' 
#' @export
geom_hurricane <- function (mapping = NULL, data = NULL, stat = "identity", 
                            position = "identity", ..., na.rm = FALSE, 
                            show.legend = NA, inherit.aes = TRUE, 
                            scale_radii=1){
  ## data to long form by quadrants
  long_data <- tidyr::melt(data[3:9], 
               id.vars = c('latitude', 'longitude', 'wind_speed'),
               measure.vars = c('ne', 'nw', 'sw', 'se')) %>% 
    ## organize by grouping factor
    dplyr::arrange(wind_speed)
  
  ## transform directions to numeric values to facilitate calculations
  long_data <- long_data %>%
    dplyr::mutate(variable = ifelse(variable=="ne", 0, 
                             ifelse(variable=="nw", 1,
                                    ifelse(variable=="sw", 2, 3)
                             )))
  
  ## variables for geographic distance calculations with `geosphere`.
  mtrs_per_mile <- 1609.344
  quad_dgrs <- 0:89
  
  ## empty df for collecting plot coordinates during loop
  df_coords <- data.frame(matrix(nrow=0, ncol=3))
  colnames(df_coords) <- c("wind_speed", "longitude", "latitude")
  
  ## main loop for calculating radii by wind_speed and quadrant
  get_coords <- function(d){
    for(i in 1:nrow(d)){
      c <- d[i, ]
      ## calculate distance and apply scale
      dist_mtrs <- d[i, "value"] * mtrs_per_mile * scale_radii
      for(angle in quad_dgrs){
        ## calculate distance from hurricane center
        new_points <- geosphere::destPoint(c(d[i, "longitude"], d[i, "latitude"]), 
                                angle + 90 * d[i, "variable"], dist_mtrs)
        lat <- new_points[1, "lat"][[1]]
        lon <- new_points[1, "lon"][[1]]
        ## add new coordinates to dataframe
        df_coords <<- rbind(df_coords, 
                            setNames(as.list(c(d[i, "wind_speed"], lon, lat)), 
                                     names(df_coords)
                                     )
                            )
      }
    }
  }
  ## apply loop function
  get_coords(test)
  
  ## create plot layer with processed data
  ggplot2::layer(data = df_coords, mapping = mapping, stat = stat, geom = GeomHURRICANE, 
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                 params = list(na.rm = na.rm, ...))
}


#' @title \code{Load Hurricane Data}
#' 
#' @description 
#' \code{get_hrcn_data} loads hurricane data .txt file from local directory and
#' completes preprocessing of data - read in data, selecting relevant variables,
#' format, then combine date and name variables, reshape data for use in 
#' \code{geom_hurricane}.
#' 
#' @param f file name of hurricane .txt file
#' @param wd local directory where file \code{f} is found
#' @param hrcn three letter all-caps character class obj indicating hurricane
#' of interest
#' @param yr numeric variable indicating year of hurricane. Some hurricane names
#' have been more than once.
#' 
#' @import tidyr
#' @import dplyr
#' 
#' @return dataframe class object containing subsetted and preprocessed 
#' hurricane data.

## load and preliminary formatting of data
get_hrcn_data <- function(f="ebtrk_atlc_1988_2015.txt", 
                          wd="c:/users/conner/adv_programming_R", hrcn, yr){
  ## load data file
  # file name and set working directory
  setwd(wd)
  
  # format hurricane, year variable as "[hurricane_name]-[year]"
  hy <- paste0(as.character(hrcn), "-", as.character(yr))
  
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
  
  # subset based on relevant variables
  subset_vars <- c("storm_id", "date_time", "latitude", "longitude", 
                   paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                   paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                   paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"))
  
  hrcns <- hrcns[subset_vars]
  
  # long form
  hrcns <- hrcns%>%
    tidyr::gather(txt_rd, dist, radius_34_ne:radius_64_nw, na.rm=TRUE)
  
  
  hrcns <- hrcns%>%
    tidyr::separate(txt_rd, c("txt", "wind_speed", "wind_direction"), sep="_")
  
  hrcns <- hrcns[, -5]
  
  hrcns <- hrcns%>%
    tidyr::spread(wind_direction, dist)%>%
    dplyr::arrange(storm_id, date_time)
  
  hrcns["wind_speed"] <- as.integer(hrcns$wind_speed)
  
  hrcns["longitude"] <- hrcns["longitude"] * -1
  
  hrcn_data <- hrcns[hrcns$storm_id==hy,]
  return(hrcn_data)
}



## test geom_hurricane
getwd()
setwd("./hurricane_geom/")

ike2008 <- get_hrcn_data(hrcn="IKE", yr=2008)

hrcn_data <- ike2008[ike2008$latitude==27.5,]

## get base map
get_map(location = c(lon=hrcn_data[1,"longitude"], lat=hrcn_data[1,"latitude"]),
        zoom=6, maptype = "toner-background") %>%
  ggmap(extent="device")+
  
  ## add geom_hurricane layer using IKE data from 2008
  geom_hurricane(data=ike2008,
                 aes(x=longitude, y=latitude,
                     group=wind_speed, colour=factor(wind_speed),
                     fill=factor(wind_speed)), alpha=0.5, scale_radii=1)+
  ## add color and legend
  scale_color_manual(name="Wind Speed (knts)",
                     values=c("red","orange", "yellow"))+
  scale_fill_manual(name="Wind Speed (knts)",
                    values=c("red", "orange", "yellow"))
