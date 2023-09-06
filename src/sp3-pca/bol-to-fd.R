library(fda)

bol_to_fd <- function(btraj.obj.list, basis){
  traj.x <- sapply(btraj.obj.list, function(x) x$Trajectory$xProj)
  traj.y <- sapply(btraj.obj.list, function(x) x$Trajectory$yProj)
  
  
  dim.table <- dim(traj.x)
  traj.length <- dim.table[1]
  n.traj <- dim.table[2]
  
  argM <- matrix(rep(1:traj.length, n.traj), nrow = traj.length)
  
  fd.x <- Data2fd(argvals = argM, y=traj.x, basisobj = basis)
  
  fd.y <- Data2fd(argvals = argM, y = traj.y, basisobj = basis)
  
  return(list(X=fd.x,Y=fd.y))
}





