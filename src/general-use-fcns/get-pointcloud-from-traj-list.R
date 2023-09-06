library(data.table)

get_pointcloud_from_traj_list <- function(traj_obj_list, doProjected = FALSE) {
  points <- data.frame(matrix(NA,nrow =0, ncol = 2))
  names(points) <- c("Lat", "Long")
  
  print("Processing pointcloud...")
  pb <- txtProgressBar(min = 1,max = length(traj_obj_list),initial = 1,char = "~",style = 3)
  c <- 0
  for (traj in traj_obj_list) { 
    setTxtProgressBar(pb,c <- c+1)
    
    if (doProjected){
      x <- traj$Trajectory$xProj
      y <- traj$Trajectory$yProj
      traj_points <- cbind(x, y)
      points.names <- c("x","y")
    }
    else{
      lat <- traj$Trajectory$Lat
      long <- traj$Trajectory$Long
      traj_points <- cbind(lat,long)
      points.names <- c("Lat","Long")
    }
    
    points <- rbind(points, traj_points)
  }
  close(pb)
  
  names(points) <- points.names
  return(points)
}
