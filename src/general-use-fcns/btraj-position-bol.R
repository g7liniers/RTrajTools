btraj_position_bol <- function(btraj.obj, btraj.obj.list){
  id <- btraj.obj$Metadata$Id
  index <- which(sapply(btraj.obj.list, function(x) x$Metadata$Id == id))
  if(length(index) != 1) stop("the specified object was either not found or found duplicate in the provided list")
  index
  }
