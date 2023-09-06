unpack_trajs_x <- function(btraj.obj.list){
  trajs.x <- sapply(btraj.obj.list, function(x) x$Trajectory$xProj)
  trajs.x
}

unpack_trajs_y <- function(btraj.obj.list){
  trajs.y <- sapply(btraj.obj.list, function(x) x$Trajectory$yProj)
  trajs.y
}
