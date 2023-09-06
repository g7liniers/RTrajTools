source("src/general-use-fcns/extract_discretization_lengths.R")

#' extends all trajectories of a list to have the same number of points
#' of discretization as the longest one. The values used for row completion
#' are the ones on the last row of the original trajectory
extend_trajs <- function(btraj.obj.list) {
  n.traj <- length(btraj.obj.list)
  traj.lengths <- sapply(btraj.obj.list, extract_disc_length)
  max.length <- max(traj.lengths)
  
  for (i in 1:n.traj){
    traj.length <- traj.lengths[i]
    traj <- btraj.obj.list[[i]]$Trajectory
    
    #Reset unextended table
    traj <- traj[1:traj.length,]
    
    #Extend the table
    traj[traj.length:max.length,] = traj[traj.length,]
    
    btraj.obj.list[[i]]$Trajectory <- traj
  }
  
  btraj.obj.list
}