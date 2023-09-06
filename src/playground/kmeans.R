source("src/loading/load_all-traj.R")
source("src/general-use-fcns/filter-traj.R")
source("src/general-use-fcns/get-ids-from-obj-list.R")
source("src/loading/frechet-distance/load_all-frechet-dist.R")

#TODO: use global load. 
dist.mat.ANT <- readRDS("output/distances/antartida/frechet/Frechet-ANT_2023-08-20_06-42-39.rds")
dist.mat.GL <- readRDS("output/distances/groenlandia/frechet/Frechet-GL_2023-08-20_06-42-48.rds")

#TODO: generalize distance usage
distances.ANT <- as.dist(dist.mat.ANT)
distances.GL <- as.dist(dist.mat.GL)

# sum(km2$Cluster==2)
# sp7 <- filter_traj(btraj.GL,sampling_points = c(7))
# 
# sp7.ids <- get_ids_from_traj_list(sp7)

reduced_distance <- function(ids, dist.matrix){
  return(as.dist(dist.matrix[ids,ids]))
}

# 
# sp7.dist <- reduced_distance(sp7.ids)
# 
# kmeans(sp7.dist,centers = 4)

# COMPARE CLUSTERINGS FOR SOME SAMPLING
# library(aricode)
# 
# for(sid1 in 1:17){
#   for(sid2 in 1:17){
#     gdas <- filter_traj(btraj.GL,reanalysis = "GDAS",sampling_ids = c(1,sid2))
#     era5 <- filter_traj(btraj.GL,reanalysis = "ERA5",sampling_ids = c(1,sid2))
#     
#     gdas.ids <- get_ids_from_traj_list(gdas)
#     era5.ids <- get_ids_from_traj_list(era5)
#     
#     gdas.dist <-reduced_distance(gdas.ids)
#     era5.dist <- reduced_distance(era5.ids)
#     
#     gdas.km <- kmeans(gdas.dist,2)
#     era5.km <- kmeans(era5.dist,2)
#     ARI <- aricode::ARI(gdas.km$Cluster, era5.km$Cluster)
#     
#     
#     print(paste(round(ARI, 3)," contraposition ",sid1," - ",sid2))
#   }
# }

