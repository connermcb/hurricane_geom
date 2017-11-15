# new class
GeomNEW <- ggproto("GeomNEW", Geom,
                   required_aes = c("lat", "long", 
                                    "r_nw", "r_ne", "r_se", "r_sw",
                                    "fill", "color"),
                   default_aes = aes(linetype=1, size=0.5),
                   draw_key = NA,
                   draw_panel = function(data, panel_scales){
                     n <- nrow(data)
                     if(n == 0) return(zeroGrob())
                     
                     mlt_data <- melt(test[3:9], 
                                      id.vars = c('latitude', 
                                                  'longitude', 
                                                  'wind_speed'),
                                      measure.vars = c('ne', 
                                                       'nw', 
                                                       'sw', 
                                                       'se')
                                      ) %>% 
                       arrange(wind_speed)
                     
                     dist_lst <- data[,6:9]
                     
                     get_pnts <- function(lat, long, dist_lst){
                       for(quad in 0:3){
                         dist <- dist_lst[[quad+1]]
                         for(angle in (0:90 +(90 * quad))){
                           dist * 
                           
                         } 
                       }
                     }
                     
                     ggname("geom_hurricane",
                            polygonGrob(x, y, default.units = "native"
                                        id = group,
                                        gp = gpar(
                                          col = color
                                          fill = fill
                                          lty = linetype
                                          lwd = size
                                          )
                                        )
                            )
                                         
                   })
f <- 1:10 + (10*2)
f

# construct geom
geom_hurricane <- function(mapping=NULL, 
                           data=NULL, 
                           stat='identity',
                           position='identity', 
                           na.rm=FALSE, 
                           show.legend=NA, 
                           inherit.aes=TRUE,...
                           )
  {
  ggplot::layer(
    geom = GeomNEW, 
    mapping=mapping, 
    data=data, 
    stat=stat, 
    position=position,
    show.legend=show.legend, 
    inherit.aes=inherit.aes,
    params = list(na.rm=na.rm)
  )
}
