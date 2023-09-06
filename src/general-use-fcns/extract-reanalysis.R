extract_reanalysis <- function(btraj.obj.list){
  sapply(btraj.obj.list, function(x) x$Metadata$Reanalysis)
}

#' 0 <--> ERA5
#' 1 <--> GDAS
extract_binary_reanalysis <- function(btraj.obj.list){
  reanal <- extract_reanalysis(btraj.obj.list)
  as.numeric(sapply(reanal, function(x) x == "GDAS"))
}

