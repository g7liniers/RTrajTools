extract_disc_length <- function(btraj.obj) {
  btraj.obj$Metadata$SampleSize
}

extract_disc_lengths <- function(btraj.obj.list){
  sapply(btraj.obj.list, extract_length)
}

extract_extended_disc_length <- function(btraj.obj){
  dim(btraj.obj$Trajectory)[1]
}

extract_extended_disc_lengths <- function(btraj.obj.list){
  sapply(btraj.obj.list, extract_extended_disc_length)
}