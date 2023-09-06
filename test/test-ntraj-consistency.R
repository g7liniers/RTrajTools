source("src/loading/load_traj-arctic.R")
source("src/general-use-fcns/filter-traj.R")
source("src/preprocessing/puntos-muestreo-artico.R")
source("src/preprocessing/puntos-muestreo-antartida.R")



# Greenland
    dseconds(muestreos.artico$`Duracion-segundos`)
    muestreos.artico$`Duracion-horas` <- muestreos.artico$`Duracion-segundos`/3600
    
    ntraj.list <- c()
    for(i in 1:17){
      ntraj <- length(filter_traj(btraj.GL,sampling_ids = i))
      ntraj.list[i] <- ntraj
    }
    
    muestreos.artico$`Trajectory_number` <- ntraj.list
    
    # Search for inconsistency in sampling id and sampling point assignment
    sampling.points.list <- c()
    for(sample.id in 1:17){
      for(sampling.point.id in 1:10){
        if(length(filter_traj(btraj.GL,sampling_ids = sample.id, sampling_points = sampling.point.id)) != 0){
          sampling.points.list[sample.id] <- paste(sampling.points.list[sample.id], sampling.point.id)
        }
      }
    }
    sampling.points.list
    
    ## Search for repeateted trajectories
    trajlist <- lapply(btraj.GL,function(btraj_obj){btraj_obj$Trajectory})
    length(trajlist)
    length(unique(trajlist))
    trajlist[[1]] == trajlist[[2]]
    
    ## Verify that each endpoint time is before its corresponding sampling endtime
    is.time.consistent <- sapply(btraj.GL, function(btraj){
      sampling.id <- btraj$Metadata$SamplingId
      muestreos.artico$`Timestamp-fin`[sampling.id] >= btraj$Metadata$EndDatetime
    })

# Antartica
    
    sampled.btraj.ANT.ERA5 <- readRDS("data/antartida/processed/sampled/antartida_sampled_ERA5_btraj.rds")
    sampled.btraj.ANT.GDAS <- readRDS("data/antartida/processed/sampled/antartida_sampled_ERA5_btraj.rds")
    ntraj.list <- c()
    for(i in 18:37){
      ntraj <-
        length(filter_traj(sampled.btraj.ANT.ERA5, sampling_ids = i)) + 
        length(filter_traj(sampled.btraj.ANT.GDAS, sampling_ids = i))
      ntraj.list[i] <- ntraj
    }
    
    muestreos.ant$`Trajectory_number` <- ntraj.list[18:37]
    
    points <- sapply(sampled.btraj.ANT.ERA5, function(x) x$Metadata$SamplingPoint)
    
    
    #otracosa
    
    
 M.ANT <- muestreos.ant[,c(1,4,12, 13)]
 M.ANT[,2] = as.integer(M.ANT[,2])
names(M.ANT) = c("Sampling id", "Sampling point id", "Duration (Hours)", "Number of trajectories" )
M.ANT <- rbind(M.ANT, sapply(M.ANT, sum))

M.GL <- muestreos.artico[,c(1,8,12,13)]
M.GL[,2] = as.integer(M.GL[,2])
names(M.GL) = c("Sampling id", "Sampling point id", "Duration (hours)", "Number of trajectories" )
M.GL <- rbind(M.GL, sapply(M.GL, sum))

library(xtable)
print(xtable(M.GL), include.rownames =F)
print(xtable(M.ANT), include.rownames = F)
