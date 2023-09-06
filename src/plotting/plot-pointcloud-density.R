source("src/general-use-fcns/get-pointcloud-from-traj-list.R")
source("src/plotting/plot-traj.R")

library(plotly)
library(ggplot2)

plot_pointcloud_density <- function(btraj_obj_list, d_radius){
  points <- get_pointcloud_from_traj_list(btraj_obj_list)
  
  fig <- points 
  fig <- fig %>%
    plot_ly(
      type = 'densitymapbox',
      lat = ~Lat,
      lon = ~Long,
      coloraxis = 'coloraxis',
      radius = d_radius) 
  fig <- fig %>%
    layout(
      mapbox = list(
        style="open-street-map",
        center= list(lat=61.844, lon=-44.655)), coloraxis = list(colorscale = "Plasma"))
  
  return(fig)
}
