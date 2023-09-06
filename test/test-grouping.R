source("src/loading/load_all-traj.R")
source("src/general-use-fcns/groupings.R")

go.GL <- group_btraj.list(btraj.GL, reanalysis = c("ERA5", "GDAS"), traj_pcloud_ids = 1:114)

length(go.GL$BOLL[[5]])



bol <- go.GL$BOLL[[1]]
sapply(bol, function(x) x$Metadata$Reanalysis)
sapply(bol, function(x) x$Metadata$SPointId) #Algo aquí va mal...

#Probamos otra agrupacion
go.GL <- group_btraj.list(btraj.GL, reanalysis = c("ERA5", "GDAS"), sampling_points =  1:10)
