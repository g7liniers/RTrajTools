# 0< margin<.5
frame.traj <- function(btraj){
  margin = 0
  mrg <- c(margin,1-margin)
  lat.bounds <- quantile(btraj$Lat,prob=mrg)
  long.bounds <- quantile(btraj$Long,prob=mrg)
  frame <- c(long.bounds[1],lat.bounds[1],long.bounds[2],lat.bounds[2])
  names(frame) <- c("long.min","lat.min","long.max","lat.max")
  return(frame)
}

frame.traj.list <- function(btraj.list){
  L <- vapply(btraj.list,frame.traj,FUN.VALUE = numeric(4))
  long.min <- min(L[1,])
  lat.min <- min(L[2,])
  long.max <- max(L[3,])
  lat.max <- max(L[4,])
  
  frame <- c(long.min,lat.min,long.max,lat.max)
  names(frame) <- c("long.min","lat.min","long.max","lat.max")
  return(frame)
}

