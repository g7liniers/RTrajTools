library(lubridate)

source("src/preprocessing/puntos-muestreo-artico.R")

#'
#' Read hysplit trajectories
#'
#'@author Ana Justel
#'@description
#'Read and load a hysplit trajectories and its associated meteorological 
#'conditions into a dataframe. Working directory must be set to data directory.
#'@param tr.hy hysplit trajectory filename
#'@returns List of pairs of the form (trajectory, metadata)
#'
read.hysplit.traj <-  function(tr.hy){ # Function for reading hysplit trajectories
  TRAJ = read.table(tr.hy,header=FALSE,nrows = 1)
  n.skip = 1+TRAJ$V1
  TRAJ = read.table(tr.hy,header=FALSE,skip=n.skip,nrows = 1)
  n.mix = TRAJ$V1             # num of traj in hysplit output file
  n.skip = n.skip + 1
  TRAJ = read.table(tr.hy,header=FALSE,skip=n.skip,nrows = n.mix)
  mix = TRAJ$V7
  n.skip = n.skip + n.mix  
  TRAJ = read.table(tr.hy,header=FALSE,skip=n.skip,nrows = 1,colClasses = "character")
  name.var = c("Year","Month","Day","Hr","Lat","Long","Alt")    
  for (i in 2:dim(TRAJ)[2]) {
    name.var = c(name.var,TRAJ[1,i])
  }
  n.skip = n.skip + 1
  TRAJ = read.table(tr.hy,header=FALSE,skip=n.skip)
  # Create trajectory list
  trajectory.obj.list = vector(mode="list", length=n.mix)
  for (i in 1:n.mix) {
    trajectory.obj <- vector(mode = "list",length = 2)
    names(trajectory.obj) <- c("Trajectory","Metadata")
    
    #populate the trajectory data frame
    X= TRAJ[TRAJ$V1 == i ,-c(1:2,7:9)] 
    colnames(X) = name.var
    # When Alt = 0, cut trajectories from the next point (keep first 0). Initial point is excluded.
    zz = which(X$Alt[2:(length(X[,1])-1)] == 0)+1
    if(length(zz) > 0) {
      X=X[-((min(zz)+1):length(X[,1])),]
    } 
    trajectory.obj[[1]] <- X
    
    #create and populate the metadata list
    metadata <- vector(mode = "list", length=8)
    names(metadata) <- c("Filename","Mix","Reanalysis","SampleSize","StartDatetime", "EndDatetime", "SamplingPoint","SamplingId")
    
    metadata["Filename"] <- tr.hy
    
    metadata["Mix"] <- mix
    
    # Reanalysis property is populated in save_hysplit_traj.R'
    
    sample.size <- dim(X)[1]
    metadata["SampleSize"] <- sample.size
    
    # Since they are retrotrajectories, the first timestamp corresponds to the last entry
    start.DT = X[sample.size, 1:4]
    start.datetime = make_datetime(year = (2000 + start.DT[1,1]), month = start.DT[1,2],
                                   day = start.DT[1,3], hour = start.DT[1,4],
                                   min = 0,sec = 0,tz = "UTC")
    metadata["StartDatetime"] = start.datetime
    
    end.DT = X[1, 1:4]
    end.datetime = make_datetime(year = (2000 + end.DT[1,1]), month = end.DT[1,2],
                                   day = end.DT[1,3], hour = end.DT[1,4],
                                   min = 0,sec = 0,tz = "UTC")
    metadata["EndDatetime"] = end.datetime
    
    
    
    trajectory.obj[[2]] <- metadata
    trajectory.obj.list[[i]] <-  trajectory.obj
    
  }
  return(trajectory.obj.list)} # El output es una lista de todas las trayectorias que hay dentro del fichero.