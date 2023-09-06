library(MASS)

source("src/general-use-fcns/filter-traj.R")
source("src/preprocessing/assign-ids.R")

estimate_2d_density_trajs <-
  function(btraj.obj.list,
           n = NULL,
           lims = NULL) {
    x = c()
    y = c()
    
    for (btraj.obj in btraj.obj.list) {
      n.points <- btraj.obj$Metadata$SampleSize
      x.traj <- btraj.obj$Trajectory$xProj[1:n.points]
      y.traj <- btraj.obj$Trajectory$yProj[1:n.points]
      
      x <- c(x, x.traj)
      y <- c(y, y.traj)
    }
    
    
    if (is.null(n) && is.null(lims)) {
      return(kde2d(x, y))
    } else {
      return(kde2d(
        x = x,
        y = y,
        n = n,
        lims = lims
      ))
    }
  }
