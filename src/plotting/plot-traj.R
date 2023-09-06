library(plotly)
library(ggplot2)
library(dplyr)


#' @description
#' Initializes a plotly-geo object for plotting trajectories and points in 
#' Greenland. It defines aesthetic options and an initial frame for the plot.
#' 
initialize_plot_GL <- function(localization="GL"){
  if(localization=="GL"){
    projection <- list(type = 'azimuthal equidistant', scale = 2, rotation = list(lon = -45.18687, lat = 66.09193))
    lonaxis <- list(range = c(-80, -10))
    lataxis <- list(range = c(55, 85))
    centroid <- find
  }
  if(localization=="ANT"){
    projection <- list(type = 'azimuthal equidistant', scale = 2, rotation = list(lon = 47.05302, lat = -77.75564))
    lonaxis <- list(range = c(-180, 180))
    lataxis <- list(range = c(-90, -60))
  }
  geo <- list(
    scope = 'country',
    resolution = 50,
    projection = projection,
    # projection = ifelse(localization == "GL",yes = list(type = 'azimuthal equidistant', scale = 1, rotation = list(lon = -45, lat = 70)),
    #                     no = list(type = 'azimuthal equidistant', scale = 1, rotation = list(lon = 0, lat = -90)) ),
    # projection = list(type = 'azimuthal equidistant', scale = 1, rotation = list(lon = -45, lat = 70)),
    # projection = list(type = 'azimuthal equidistant', scale = 1, rotation = list(lon = 0, lat = -90)),
    #projection = list(type = 'stereographic', scale = 1, rotation=list(lon=-90,lat=-90)),
    showland = TRUE,
    landcolor = toRGB("gray95"),
    countrycolor = toRGB("gray80"),
    showcountries = FALSE,
    showframe = FALSE,
    showcoastlines = TRUE,
    lonaxis = lonaxis,
    lataxis = lataxis
    # lonaxis = ifelse(localization=="GL",yes = list(range = c(-80, -10)), no = list(range = c(-180, 180))),
    # lataxis = ifelse(localization=="GL",yes = list(range = c(55, 85)), no = list(range = c(-90, -60)))
  )
  
  fig <- plot_geo(type = "scattergeo", mode = "markers")
  fig <- fig %>% layout(
    title = 'RTrajGL',
    geo = geo, legend = FALSE
  )
  fig
}

#' @description
#' Draws the lines from a single trajectory over a plotly-geo object.
#' @param fig the base plotly figure to paint over
#' @param traj_obj trajectory object
#' @returns the original plotly-geo object with the painted trajectory lines
#' 
paint_lines_traj <- function(fig, traj_obj,line_colors = NULL){
  traj <- traj_obj$Trajectory
  
  lat <- traj$Lat
  long <- traj$Long
  alt <- traj$Alt
  len <- length(lat)
  
  plottable_traj <- data.frame(matrix(NA,nrow = len-1,ncol = 0))
  
  plottable_traj$start_lat <- lat[-len]
  plottable_traj$end_lat <- lat[-1]
  plottable_traj$start_lon <- long[-len]
  plottable_traj$end_lon <- long[-1]

  # hovertext <- paste("alt: ",traj$Alt[-len],"m")
  
  if(is.null(line_colors)) {
    switch(traj_obj$Metadata$Reanalysis,
           "ERA5" = line_colors <- I("blue"),
           "GDAS" = line_colors <- I("orange"))
  }
  
  
  fig <- fig %>% add_segments(
    data = plottable_traj,
    x = ~start_lon, xend = ~end_lon,
    y = ~start_lat, yend = ~end_lat,
    alpha = 0.4, size = I(0.6),hovertext="", 
    hoverinfo = '', color=line_colors
  )
  
  fig
}
  
#' @description
#' Draws the lines from a single trajectory over a plotly-geo object. It has the
#' function paint_lines_traj as a direct dependency.
#' @param fig the base plotly figure to paint over
#' @param traj_obj_list Trajectory object list
#' @returns the original plotly-geo object with the painted trajectory list lines
paint_lines_traj_list <- function(fig,traj_obj_list,color=NULL){
  it = 0
  for (traj in traj_obj_list){
    it <- it+1
    fig <- fig %>% paint_lines_traj(traj,line_colors = color[it])
  }
  
  return(fig)
}

paint_lines_traj_list_alt_color <- function(fig, traj_obj_list){
  
}

#' @description
#' Draws the points from a single trajectory over a plotly-geo object.
#' @param fig the base plotly figure to paint over
#' @param traj_obj trajectory object
#' @returns the original plotly-geo object with the painted trajectory points
paint_points_traj <- function(fig, traj_obj) {
  hovertext <- paste("alt: ",traj_obj$Trajectory$Alt,"m")
  
  fig <- fig %>% add_markers(
    data = traj_obj$Trajectory, x = ~Long, y = ~Lat, text = I("texto"),
    size = I(1), hovertext = hovertext, alpha = 1
  )
  
  return(fig)
}

#' @description
#' Draws the points from a list of trajectories over a plotly-geo object.
#' @param fig the base plotly figure to paint over
#' @param traj_obj_list trajectory object list
#' @returns the original plotly-geo object with the painted trajectory list points
paint_points_traj_list <- function(fig, traj_obj_list){
  for (traj in traj_obj_list){
    fig <- fig %>% paint_points_traj(traj)
  }
  
  return(fig)
}

#' @description
#' Draws the endpoints (startpoints in some sense) from a list of trajectories
#'  over a plotly-geo object.
#' @param fig the base plotly figure to paint over
#' @param traj_obj_list trajectory object list
#' @returns the original plotly-geo object with the painted trajectory list
#'  endpoints
paint_endpoints_traj_list <- function(fig, traj_obj_list) {
  endpoints <- data.frame(matrix(NA, nrow = 0,ncol = 5))
  
  for (traj in traj_obj_list){
    #number of discretization points of the trajectory
    n.points <- length(traj$Trajectory$Lat)
    
    end.lat = traj$Trajectory$Lat[n.points]
    end.long = traj$Trajectory$Long[n.points]
    end.alt = traj$Trajectory$Alt[n.points]
    
    if(end.alt == 0){
      size = 4
      symb = "x"
    }
    else {
      size = 4
      symb = "star-triangle-up"
    }
    
    if(traj$Metadata$Reanalysis == "ERA5") col = "blue"
    else col = "orange"
    
    endpoints <- rbind(endpoints, c(end.lat,end.long,end.alt,col,size,symb))
  }
  
  names(endpoints) <- c("Lat", "Long", "Alt","Col", "Size","Symb")
  
  fig <- fig %>% add_markers(
    data = endpoints, x = ~Long, y = ~Lat,
    marker = list(symbol = ~Symb, color = ~Col, size = ~Size, opacity=1,
                  line = list(color = NULL, width = NULL)),
  )
  
  return(fig)
}

#' @description
#' Draws the sampling points
#' @param fig the base plotly geo figure to paint over
#' @param points sampling points numbers to be plotted. Defaults to all
#' @returns the original plotly-geo object with the painted points
paint_sampling_points <- function(fig, sampling_points = 1:10){
  puntos.muestreo <- readRDS("data/groenlandia/processed/PuntosMuestreoArticoClean.rds")
  fig <- fig %>% add_markers(
    data = puntos.muestreo,
    x = ~Long, y = ~Lat,
    marker = list(symbol = "circle-dot", color = "cyan", size = 7,
                  line = list(color = "blue4",width=1)
                  ),
    hovertext = ~Number,
    hoverinfo = "text"
  )
}
