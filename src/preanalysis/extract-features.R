source("src/general-use-fcns/filter-traj.R")
source("src/general-use-fcns/find-corresp-traj-position.R")

get_length <- function(x) x$Metadata$TrajLength
get_ndiscp <- function(x) x$Metadata$SampleSize
get_median_xproj <- function(x) median(x$Trajectory$xProj)
get_median_yproj <- function(x) median(x$Trajectory$yProj)
get_median_height <- function(x) median(x$Trajectory$Alt)

#' this function separates a btraj.obj.list by reanalysis, then sorts the 
#' GDAS list to match its ERA5 counterpart, then applies the specified 
#' function element-wise and returns a 2vector list with the outputs.
paired_apply <- function(btraj.obj.list, varfcn){
  
  bol <- btraj.obj.list
  bol.ERA5 <- filter_traj(bol, reanalysis = "ERA5")
  bol.GDAS <- filter_traj(bol, reanalysis = "GDAS")
  
  n.traj <- length(bol.ERA5)
  if(n.traj != length((bol.GDAS))) stop("Trajectories must be paired")
  
  bol.GDAS.unsorted <- bol.GDAS
  for(i in 1:n.traj){
    position <- find_corresp_traj_position(bol.ERA5[[i]], bol.GDAS.unsorted)
    bol.GDAS[[i]] <- bol.GDAS.unsorted[[position]]
  }
  
  v.ERA5 <- sapply(bol.ERA5, varfcn)
  v.GDAS <- sapply(bol.GDAS, varfcn)
  
  list(vERA5 = v.ERA5, vGDAS = v.GDAS)
}


