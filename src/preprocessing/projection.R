library(proj4)
source("src/general-use-fcns/get-pointcloud-from-traj-list.R")

#' finds mean lat and long from a dataframe of coordinates
find_centroid <- function(pointcloud){
  mean.lat <- mean(pointcloud$Lat)
  mean.long <- mean(pointcloud$Long)
  
  centroid <- list()
  centroid$Lat <- mean.lat
  centroid$Long <- mean.long
  
  centroid
}


project_centered_equidist_azimutal <- function(btraj.obj.list){
  n.traj <- length(btraj.obj.list)
  pointcloud <- get_pointcloud_from_traj_list(btraj.obj.list)
  centroid_proj <- find_centroid(pointcloud)
  
  # Define the azimuthal equidistant projection parameters with the variable values
  eqd_params <- paste("+proj=aeqd +lat_0=", centroid_proj$Lat, " +lon_0=", centroid_proj$Long, sep = "")
  
  print("Transforming geographical coordinates into planar via azimuthal equidistant projection...")
  pb <- txtProgressBar(min = 1,max = n.traj,initial = 1,style = 3)
  

  for(i in 1:n.traj){
    setTxtProgressBar(pb, value = i)
    traj = btraj.obj.list[[i]]$Trajectory
    # Define coordinates (longitude, latitude) in decimal degrees
    input_coords <- matrix(data=c(traj$Long, traj$Lat), ncol = 2)
    
    # Transform coordinates to azimuthal equidistant projection
    output_coords <- proj4::project(input_coords, proj = eqd_params)
    # Assign to new variables in traj object, converting m to km
    traj$xProj <- output_coords[,1]/1000
    traj$yProj <- output_coords[,2]/1000
    
    btraj.obj.list[[i]]$Trajectory <- traj
  }
  close(pb)
  
  btraj.obj.list
}
