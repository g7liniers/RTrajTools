find_corresp_traj_position <- function(btraj.obj, btraj.obj.list){
  id <- btraj.obj$Metadata$Id
  pairid <- btraj.obj$Metadata$PairId
  
  equalpairid.unequalid <- sapply(btraj.obj.list, function(x) x$Metadata$Id != id && x$Metadata$PairId == pairid )
  position <- which(equalpairid.unequalid)
  if(length(position) != 1) stop("Either no corresponding trajectory in the list or the corresponding trajectory is at least duplicate")
  position
}
