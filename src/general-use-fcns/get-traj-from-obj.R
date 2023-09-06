#'
#'@description 
#'Extract the trajectories of a btraj object list
#'@param btraj_object_list the backwards trajectory objects whose trajectory is to be extracted
#'@returns the trajectory encoded in a dataframe of 2 columns: Lat and Long
#'
get.trajs <- function(btraj_object_list){
  n.traj = length(btraj_object_list)
  trajectories <- vector(mode = "list", length = n.traj)
  
  for (i in 1:n.traj){
    btraj_object <- btraj_object_list[[i]]
    df <- cbind(btraj_object$Trajectory$Lat, btraj_object$Trajectory$Long)
    df <- as.data.frame(df)
    names(df) <- c("Lat", "Long")
    trajectories[[i]] <- df
  }
  
  return(trajectories)
}
