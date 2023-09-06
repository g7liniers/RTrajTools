library(geosphere)

source("src/general-use-fcns/get-traj-from-obj.R")

#' add a column to the trajectory dataframes with the step distance. First point
#' of the backwards trajectory (last one of the trajectory) corresponds to a 
#' null step distance
assign_step_travel_distances <- function(btraj.obj.list){
  n.trajs <- length(btraj.obj.list)
  traj.list <- get.trajs(btraj.obj.list)
  
  cat("Calculating step distances of trajectories.......................... \n")
  pb = txtProgressBar(min = 1, max = n.trajs, initial = 1,style = 3)
  for(i in 1:n.trajs){
    setTxtProgressBar(pb,i)
    n.discret <- btraj.obj.list[[i]]$Metadata$SampleSize
    step.dist <- vector(mode = "double",length = n.discret)
    
    traj <- traj.list[[i]] #unpack. the trajectory as lat long coordinates
    for (j in 2:n.discret){
      p1 <- traj[j-1,]
      p2 <- traj[j,]
      #inverse coordinate order, as required by distVincentySphere function
      p1 <- c(p1$Long,p1$Lat) 
      p2 <- c(p2$Long,p2$Lat) 
  
      step.dist[j] <- distVincentySphere(p1,p2)
    }
    
    btraj.obj.list[[i]]$Trajectory$StepDist <- step.dist
  }
  close(pb)
  btraj.obj.list
}

#' Add variable TrajectoryLength to Metadata

assign_total_travel_distances <- function(btraj.obj.list){
  n.trajs <- length(btraj.obj.list)
  
  for (i in 1:n.trajs){
    btraj.obj <- btraj.obj.list[[i]]
    btraj.obj$Metadata$TrajLength <- sum(btraj.obj$Trajectory$StepDist)
    btraj.obj.list[[i]] <- btraj.obj
  }
  
  btraj.obj.list
}

#' add a column to the trajectory dataframes with the mean step speed. First point
#' of the backwards trajectory (last one of the trajectory) corresponds to a
#' null step distance. Step distances MUST have been previously computed.
#' Speeds are computed in m/s (traj discretization points are spaced in hourly intervals)
assign_step_travel_speed <- function(btraj.obj.list) {
  n.trajs <- length(btraj.obj.list)
  
  cat("Calculating step mean speed of trajectories.......................... \n")
  pb = txtProgressBar(
    min = 1,
    max = n.trajs,
    initial = 1,
    style = 3
  )
  for (i in 1:n.trajs) {
    setTxtProgressBar(pb, i)
    stepdist <- btraj.obj.list[[i]]$Trajectory$StepDist
    n.discret.points <- length(stepdist)
    if(n.discret.points<3) next
    
    stepspeed <- stepdist / 3600
    smooth.stepspeed <-
      vector(mode = "double", length = n.discret.points)
    smooth.stepspeed[1] <- stepspeed[2]
    smooth.stepspeed[n.discret.points] <-
      stepspeed[n.discret.points]
    smooth.stepspeed[2:(n.discret.points - 1)] <-
      ((stepspeed[-n.discret.points] + stepspeed[-1]) / 2)[-1]
    
    btraj.obj.list[[i]]$Trajectory$StepSpeed <- smooth.stepspeed
  }
  
  btraj.obj.list
}

#' Add variable MeanSpeed to Metadata

assign_mean_travel_speed <- function(btraj.obj.list){
  n.trajs <- length(btraj.obj.list)
  
  for (i in 1:n.trajs){
    btraj.obj <- btraj.obj.list[[i]]
    btraj.obj$Metadata$MeanSpeed <- btraj.obj$Metadata$TrajLength/(3600*btraj.obj$Metadata$SampleSize)
    btraj.obj.list[[i]] <- btraj.obj
  }
  
  btraj.obj.list
}

assign_lengths_speeds <- function(btraj.obj.list){
  btraj.obj.list <- assign_step_travel_distances(btraj.obj.list)
  btraj.obj.list <- assign_total_travel_distances(btraj.obj.list)
  btraj.obj.list <- assign_step_travel_speed(btraj.obj.list)
  btraj.obj.list <- assign_mean_travel_speed(btraj.obj.list)
  btraj.obj.list
}
