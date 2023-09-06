library(fda)

assign_fd_trajs <- function(btraj.obj.list){
  n.trajs <- length(btraj.obj.list)
  
  cat(paste("\n Assigning ", n.trajs, "FdTrajectory objects... \n"))
  time.cum.sum <- 0
  for(i in 1:n.trajs){
    init.time <- Sys.time()
    
    traj <- btraj.obj.list[[i]]$Trajectory
    traj.length <- dim(traj)[1]
    
    traj.x <- traj$xProj
    traj.y <- traj$yProj
    traj.z <- traj$Alt
    traj.step.dist <- traj$StepDist
    
    traj.fd.x <- Data2fd(argvals = 1:traj.length, y = traj.x)
    traj.fd.y <- Data2fd(argvals = 1:traj.length, y = traj.y)
    traj.fd.z <- Data2fd(argvals = 1:traj.length, y = traj.z)
    traj.fd.step.dist <- Data2fd(argvals = 1:traj.length, y = traj.step.dist)

    btraj.obj.list[[i]]$FdTraj$x <- traj.fd.x
    btraj.obj.list[[i]]$FdTraj$y <- traj.fd.y
    btraj.obj.list[[i]]$FdTraj$z <- traj.fd.z
    btraj.obj.list[[i]]$FdTraj$StepDist <- traj.fd.step.dist
    
    timediff <- Sys.time() - init.time
    time.cum.sum <- time.cum.sum + timediff
    mean.iter.time <- time.cum.sum/i
    est.remaining.time <- (n.trajs - i)*mean.iter.time
        cat(paste(
      "\r Iteration", sprintf("%6.0f", i), "/", n.trajs, 
      "(", sprintf("%5.2f", 100*i/n.trajs),"%); ",
      "Iteration time:", sprintf("%8.4f", timediff), "s ; ",
      "Remaining time:", sprintf("%5.0f", est.remaining.time), "s"
    ))
  }

  btraj.obj.list
}
