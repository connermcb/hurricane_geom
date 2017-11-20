## Suite of functions for geographically plotting hurricane windspeed and radii
## data.

#' @title Calculate Plot Points
#' 
#' @description 
#' \code{get_dests} takes a coordinate pair \code{pnt} corresponding to the 
#' center of hurricane, a vector of four distances \code{dists} in miles that
#' are the distances that the particular wind speed being processed extends in 
#' the four quadrants. The distances are respectively NE, NW, SW, SE. The
#' function calculates the 364 coordinate pairs necessary to plot a polygonGrob
#' object and scales them according to the scaling value \code{scl} passed from
#' \code{GeomHurricane}.
#' 
#' @param pnt vector of class numeric longitude and latitude values in degrees 
#' and in that order.
#' @param dists vector of class numeric, four radii distances in miles for 
#' cartisian quadrants.
#' @param scl float value (0,1] for scaling wind radii plot
#' 
#' @import geosphere
#' 
#' @return dataframe object with two columns corresponding respectively to 
#' longitude and latitude values of coordinate pairs, labeled as `x` and `y` to
#' facilitate reading in polygonGrob function.
#' 
#' @export
get_dests <- function(pnt, dists, scl){
  ## conversion value for miles to meters, list of angle per quadrant  
  mtrs_p_mile <- 1609.344
  quads <- list(0:90, 90:180, 180:270, 270:360)
  
  ## calculate complete set of points for one wind_speed group
  coords <- do.call(rbind, lapply(1:4, function(i){
    geosphere::destPoint(pnt, b=quads[[i]], d=dists[i]*mtrs_p_mile* scl)
    }))
  
  ## rename variables for reading in polygonGrob
  colnames(coords) <- c("x", "y")

  return(data.frame(coords))
}



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
Geom_Hurricane <- ggproto("GeomHurricane", ggplot2::Geom,
                         required_aes = c("x", "y",
                                          "r_ne", "r_nw", "r_sw", "r_se"),
                         default_aes = ggplot2::aes(colour=NA,
                                                    fill=NA,
                                                    size=0.5,
                                                    linetype=1,
                                                    alpha=0.7,
                                                    scale_radii=1),
                         draw_key = ggplot2::draw_key_polygon,
                         draw_group = function(data, panel_scales, coord){
                           
                           ## get hurricane center
                           center <- data[1, 3:4]
                           
                           ## calculate plot pairs
                           coords <- get_dests(center, 
                                               data[1, 5:8],
                                               data[1, "scale_radii"])
                           
                           ## scale to plot panel
                           coords <- coord$transform(coords, panel_scales)
                
                grid::polygonGrob(
                  x = coords$x,
                  y = coords$y,
                  gp = grid::gpar(col = data[1,]$colour, 
                                  fill = data[1,]$fill, 
                                  alpha = data[1,]$alpha))
                         }
)


#' @title Plot Hurricane Wind Speed/Radii Data
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
#' @return ggplot2 object plot layer
#' 
#' @import tidyr
#' @import dplyr
#' @import geosphere
#' 
#' @export
geom_hurricane <- function (mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            scale_radii=1,...){
  ## create plot layer with processed data
  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = Geom_Hurricane, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}


#' @title Load Hurricane Data
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



library(data.table)
library(dplyr)
library(geosphere)
library(ggmap)
library(lubridate)
library(readr)
library(tidyr)
library(grid)

## test geom_hurricane
getwd()
setwd("./hurricane_geom/")

ike2008 <- get_hrcn_data(hrcn="IKE", yr=2008)

unique(ike2008$latitude)

hrcn_data <- ike2008[ike2008$latitude==28.3,]
hrcn_data

## create base map
map_data <- get_map(c(hrcn_data[1,"longitude"], hrcn_data[1, "latitude"]),
        zoom=6, maptype = "toner-background")

## plot
ggmap(map_data, extent = "device")+
  ## add geom_hurricane layer using IKE data from 2008
  geom_hurricane(data=hrcn_data,
                 aes(x=longitude, y=latitude, 
                     r_ne=ne, r_nw=nw, r_sw=sw, r_se=se,
                     color=factor(wind_speed),
                     fill=factor(wind_speed)))+
  ## add color and legend
  scale_color_manual(name="Wind Speed (knts)",
                     values=c("red","orange", "yellow"))+
  scale_fill_manual(name="Wind Speed (knts)",
                    values=c("red", "orange", "yellow"))



