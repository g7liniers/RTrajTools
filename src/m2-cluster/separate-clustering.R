source("src/m2-cluster/kmedoids.R")
source("src/general-use-fcns/filter-traj.R")
source("src/m1-depth/distance-based-depth.R")
source("src/general-use-fcns/btraj-position-bol.R")
source("src/m1-depth/get_starting_medoids.R")
library(aricode)

separate_clustering <- function(localization, distance, btraj.obj.list, centers.obj = list(ERA5 = 2, GDAS = 2)){
  bol.ERA5 <- filter_traj(btraj.obj.list, reanalysis = "ERA5")
  bol.GDAS <- filter_traj(btraj.obj.list, reanalysis = "GDAS")
  n.pairs <- length(bol.ERA5)
  if(n.pairs != length(bol.GDAS)) stop("This method only works with paired trajectories. Check btraj.obj.list input.")

  kmedoids.ERA5 <- kmedoids_traj(localization, distance, bol.ERA5, centers.obj$ERA5)
  kmedoids.GDAS <- kmedoids_traj(localization, distance, bol.GDAS, centers.obj$GDAS)
  
  ari <- aricode::ARI(kmedoids.ERA5$Cluster, kmedoids.GDAS$Cluster)
  
  M2A.comparison.obj <- list(KMedoidsERA5 = kmedoids.ERA5, KMedoidsGDAS = kmedoids.GDAS, ARI = ari)
  M2A.comparison.obj
}


separate_clustering_2samplings <- function(s1, s2, distance, mix = 1:10/10){
  isGL <- all(c(s1,s2) %in% 1:17)
  isANT <- all(c(s1,s2) %in% 18:37)
  if(!isGL & !isANT) stop("invalid sampling ids")
  
  if(isGL) {
    localization <- "GL"
    global.bol <- filter_traj(btraj.GL, mix_values = mix)
  }
  if(isANT){
    localization <- "ANT"
    global.bol <- filter_traj(btraj.ANT, mix_values = mix)
  }
  
  # Assign to start centers the most central curve of each sampling
  bol <- filter_traj(global.bol, sampling_ids = c(s1, s2))
  bol.ERA5 <- filter_traj(bol, reanalysis = "ERA5")
  bol.GDAS <- filter_traj(bol, reanalysis = "GDAS")
  
  bol.ERA5.s1 <- filter_traj(bol, reanalysis = "ERA5", sampling_ids = s1)
  c.ERA5.s1 <- btraj_position_bol(get_deepest_obj(localization, distance, bol.ERA5.s1), bol.ERA5)
  bol.ERA5.s2 <- filter_traj(bol, reanalysis = "ERA5", sampling_ids = s2)
  c.ERA5.s2 <- btraj_position_bol(get_deepest_obj(localization, distance, bol.ERA5.s2), bol.ERA5)
  
  bol.GDAS.s1 <- filter_traj(bol, reanalysis = "GDAS", sampling_ids = s1)
  c.GDAS.s1 <- btraj_position_bol(get_deepest_obj(localization, distance, bol.GDAS.s1), bol.GDAS)
  bol.GDAS.s2 <- filter_traj(bol, reanalysis = "GDAS", sampling_ids = s2)
  c.GDAS.s2 <- btraj_position_bol(get_deepest_obj(localization, distance, bol.GDAS.s2), bol.GDAS)
  
  centers <- list(ERA5 = c(c.ERA5.s1, c.ERA5.s2), GDAS = c(c.GDAS.s1, c.GDAS.s2))
  return(separate_clustering(localization, distance, bol, centers.obj = centers))
}

separate_clustering_2spoints <- function(sp1, sp2, distance, mix = 1:10/10){
  isGL <- all(c(sp1,sp2) %in% 1:10)
  isANT <- all(c(sp1,sp2) %in% 11:20)
  if(!isGL & !isANT) stop("invalid sampling points")
  
  if(isGL) {
    localization <- "GL"
    global.bol <- filter_traj(btraj.GL, mix_values = mix)
  }
  if(isANT){
    localization <- "ANT"
    global.bol <- filter_traj(btraj.ANT, mix_values = mix)
  }
  
  # Assign to start centers the most central curve of each sampling point
  bol <- filter_traj(global.bol, sampling_points = c(sp1, sp2))
  bol.ERA5 <- filter_traj(bol, reanalysis = "ERA5")
  bol.GDAS <- filter_traj(bol, reanalysis = "GDAS")
  
  bol.ERA5.sp1 <- filter_traj(bol, reanalysis = "ERA5", sampling_points =  sp1)
  c.ERA5.sp1 <- btraj_position_bol(get_deepest_obj(localization, distance, bol.ERA5.sp1), bol.ERA5)
  bol.ERA5.sp2 <- filter_traj(bol, reanalysis = "ERA5", sampling_points = sp2)
  c.ERA5.sp2 <- btraj_position_bol(get_deepest_obj(localization, distance, bol.ERA5.sp2), bol.ERA5)
  
  bol.GDAS.sp1 <- filter_traj(bol, reanalysis = "GDAS", sampling_points = sp1)
  c.GDAS.sp1 <- btraj_position_bol(get_deepest_obj(localization, distance, bol.GDAS.sp1), bol.GDAS)
  bol.GDAS.sp2 <- filter_traj(bol, reanalysis = "GDAS", sampling_points = sp2)
  c.GDAS.sp2 <- btraj_position_bol(get_deepest_obj(localization, distance, bol.GDAS.sp2), bol.GDAS)
  
  centers <- list(ERA5 = c(c.ERA5.sp1, c.ERA5.sp2), GDAS = c(c.GDAS.sp1, c.GDAS.sp2))
  return(separate_clustering(localization, distance, bol, centers.obj = centers))
}

