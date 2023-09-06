# Extract trajectory from a btraj object
extract_traj <- function(btraj.obj, projected = TRUE){
  traj <- btraj.obj$Trajectory
  
  if(projected){
    df <- data.frame(x = traj[,"xProj"], y = traj[, "yProj"])
  }
  else{
    df <- data.frame(Lat = traj[,"Lat"], Long = traj[, "Long"])
  }
  
  df
}

# Extract trajectories from a btraj object list
extract_traj_list <- function(btraj.obj.list, projected = TRUE){
  n.traj <- length(btraj.obj.list)
  traj.list <- list()
  for (i in 1:n.traj){
    traj.list[[i]] <- extract_traj(btraj.obj.list[[i]],projected = projected)
  }
  traj.list
}