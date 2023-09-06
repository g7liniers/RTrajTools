source("src/m2-cluster/kmedoids.R")
source("src/m1-depth/get_starting_medoids.R")
source("src/m2-cluster/deterministic-cluster/cluster-by-sampling.R")
source("src/m2-cluster/deterministic-cluster/cluster-by-spoint.R")

library(aricode)

#measure using ari if the provided data has cluster structure by sampling point or sampling
cluster_structure_measure_ari <- function(localization, distance, grouping, btraj.obj.list){
  
  centers <- get_starting_medoids(localization, distance, btraj.obj.list, grouping)
  cluster <- kmedoids_traj(localization, distance, btraj.obj.list, centers)$Cluster
  
  if(grouping == "sampling"){
    compare.cluster <- cluster_by_sampling(btraj.obj.list)$Cluster
  }
  if(grouping == "spoint"){
    compare.cluster <- cluster_by_spoint(btraj.obj.list)$Cluster
  }
  ari <- aricode::ARI(cluster, compare.cluster)
  ari
  
}
