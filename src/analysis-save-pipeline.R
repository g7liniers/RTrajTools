source("src/loading/load_all-traj.R")
source("src/loading/load_all-dist.R")

source("src/m1-depth/distance-based-depth.R")
source("src/m2-cluster/joint-clustering.R")
source("src/m2-cluster/separate-clustering.R")
source("src/m3-supervised/loo-cv_by-sampling.R")
source("src/m3-supervised/loo-cv_by-spoint.R")
source("src/m2-cluster/check-cluster-structure.R")

source("src/general-use-fcns/matrix-fcns.R")


save_by_samplings <- function(localization, mix = 1:10 / 10) {
  isValidLoc = F
  
  if (localization == "ANT") {
    global.bol <- btraj.ANT
    samplings <- 18:37
    isValidLoc = T
  }
  if (localization == "GL") {
    global.bol <- btraj.GL
    samplings <- 1:17
    isValidLoc = T
  }
  
  if (!isValidLoc)
    stop("Localization must be either 'ANT' or 'GL'")
  
  Distances <- c("Frechet", "Hellinger", "Rdimf")
  
  n.samplings <- length(samplings)
  
  depth_indexes <- list()
  joint_cluster_ari <- list()
  separate_cluster_ari <- list()
  loo_cv <- list()
  
  for (d in 1:3) {
    distance <- Distances[d]
    cat(paste("Started ", distance, "... \n"))
    
    
    depth.matrix <- matrix(0, n.samplings, n.samplings)
    joint.cluster.matrix <- matrix(0, n.samplings, n.samplings)
    sep.cluster.matrix <- matrix(0, n.samplings, n.samplings)
    loo.cv.list <- c()
    
    for (i in 1:(n.samplings - 1)) {
      for (j in (i + 1):n.samplings) {
        s1 <- samplings[i]
        s2 <- samplings[j]
        
        depth.matrix[i, j] <-
          depth_analysis_2samplings(s1, s2, distance, mix)$NormalizedDeepestsDistance
        
        joint.cluster.matrix[i, j] <-
          joint_clustering_2samplings(s1, s2, distance, mix)$ARI1
        
        sep.cluster.matrix[i, j] <-
          separate_clustering_2samplings(s1, s2, distance, mix)$ARI
      }
    }
    
    depth_indexes[[d]] <- sym_fill(depth.matrix)
    joint_cluster_ari[[d]] <- sym_fill(joint.cluster.matrix)
    separate_cluster_ari[[d]] <- sym_fill(sep.cluster.matrix)
    loo_cv[[d]] <-
      sapply(loocv_sampling(localization, distance, mix), function(x)
        x$CorrectClassificationRate)
  }
  
  names(depth_indexes) <- Distances
  names(joint_cluster_ari) <- Distances
  names(separate_cluster_ari) <- Distances
  names(loo_cv) <- Distances

  
  OBJ <-
    list(
      grouping = "SAMPLING",
      localization = localization,
      DEPTH = depth_indexes,
      JOINTCLUST = joint_cluster_ari,
      SEPCLUST = separate_cluster_ari,
      LOOCV = loo_cv
    )
  
  saveRDS(OBJ,
          file = paste0("output/analyzed/", localization, "-by-samplings.rds"))
  cat(paste(localization, distance, " by sampling", "DONE \n"))
  
}


save_by_spoints <- function(localization, mix = 1:10 / 10) {
  isValidLoc = F
  
  if (localization == "ANT") {
    global.bol <- btraj.ANT
    spoints <- 11:20
    isValidLoc = T
  }
  if (localization == "GL") {
    global.bol <- btraj.GL
    spoints <- 1:10
    isValidLoc = T
  }
  
  if (!isValidLoc)
    stop("Localization must be either 'ANT' or 'GL'")
  
  Distances <- c("Frechet", "Hellinger", "Rdimf")
  
  n.spoints <- length(spoints)
  
  depth_indexes <- list()
  joint_cluster_ari <- list()
  separate_cluster_ari <- list()
  loo_cv <- list()
  
  for (d in 1:3) {
    distance <- Distances[d]
    cat(paste("Started ", distance, "... \n"))
    
    depth.matrix <- matrix(0, n.spoints, n.spoints)
    joint.cluster.matrix <- matrix(0, n.spoints, n.spoints)
    sep.cluster.matrix <- matrix(0, n.spoints, n.spoints)
    loo.cv.list <- c()
    
    for (i in 1:(n.spoints - 1)) {
      for (j in (i + 1):n.spoints) {
        sp1 <- spoints[i]
        sp2 <- spoints[j]
        
        depth.matrix[i, j] <-
          depth_analysis_2spoints(sp1, sp2, distance, mix)$NormalizedDeepestsDistance
        
        joint.cluster.matrix[i, j] <-
          joint_clustering_2spoints(sp1, sp2, distance, mix)$ARI1
        
        sep.cluster.matrix[i, j] <-
          separate_clustering_2spoints(sp1, sp2, distance, mix)$ARI
      }
    }
    
    depth_indexes[[d]] <- sym_fill(depth.matrix)
    joint_cluster_ari[[d]] <- sym_fill(joint.cluster.matrix)
    separate_cluster_ari[[d]] <- sym_fill(sep.cluster.matrix)
    loo_cv[[d]] <-
      sapply(loocv_spoint(localization, distance, mix), function(x)
        x$CorrectClassificationRate)
  }
  
  names(depth_indexes) <- Distances
  names(joint_cluster_ari) <- Distances
  names(separate_cluster_ari) <- Distances
  names(loo_cv) <- Distances
  
  
  OBJ <-
    list(
      grouping = "SPOINT",
      localization = localization,
      DEPTH = depth_indexes,
      JOINTCLUST = joint_cluster_ari,
      SEPCLUST = separate_cluster_ari,
      LOOCV = loo_cv
      )
  
  saveRDS(
    OBJ,
    file = paste0(
      "output/analyzed/",
      localization,
      alternative_name(mix),
      "-by-spoints.rds"
    )
  )
  cat(paste(localization, distance, " by sampling", "DONE \n"))
  
}

alternative_name <- function(mix) {
  if (mix == 1:10 / 10)
    return("")
  if (mix == 1:5 / 10)
    return("-low")
  if (mix == 6:10 / 10)
    return("-high")
  return("-unknown-mix")
  
}

save_global_analysis <- function(localization, mix = 1:10/10){

  Distances <- c("Frechet", "Hellinger", "Rdimf")
  if(localization =="GL") global.bol <- filter_traj(btraj.GL, mix_values = mix)
  if(localization == "ANT") global.bol <- filter_traj(btraj.ANT, mix_values = mix)
  
  separate_cluster <- list()
  joint_cluster <- list()
  loocv_glob <- list()

  for (d in 1:3){
    distance <- Distances[d]
    
  #Separate cluster
    bol.ERA5 <- filter_traj(global.bol, reanalysis = "ERA5", mix_values = mix)
    bol.GDAS <- filter_traj(global.bol, reanalysis = "GDAS", mix_values = mix)
    ##by sampling
    c.ERA5 <- get_starting_medoids(localization, distance, bol.ERA5, grouping = "sampling")
    c.GDAS <- get_starting_medoids(localization, distance, bol.GDAS, grouping = "sampling")
    separate_cluster_ari_samplings <- separate_clustering(localization, distance,global.bol, centers.obj = list(ERA5 = c.ERA5,  GDAS = c.GDAS))$ARI
    
    ## by spoint
    c.ERA5 <- get_starting_medoids(localization, distance, bol.ERA5, grouping = "spoint")
    c.GDAS <- get_starting_medoids(localization, distance, bol.GDAS, grouping = "spoint")
    separate_cluster_ari_spoints <- separate_clustering(localization, distance,global.bol, centers.obj = list(ERA5 = c.ERA5,  GDAS = c.GDAS))$ARI
    
    
    separate_cluster[[d]] <- list(BySampling = separate_cluster_ari_samplings, BySpoint = separate_cluster_ari_spoints)
    
  #Joint cluster - detect cluster structure
    ## by sampling
    ari.kmedoids.vs.sampling.global <- cluster_structure_measure_ari(localization, distance, "sampling", global.bol)
    ari.kmedoids.vs.sampling.ERA5 <- cluster_structure_measure_ari(localization, distance, "sampling", filter_traj(global.bol, reanalysis = "ERA5"))
    ari.kmedoids.vs.sampling.GDAS <- cluster_structure_measure_ari(localization, distance, "sampling", filter_traj(global.bol, reanalysis = "GDAS"))
    ## by spoint
    ari.kmedoids.vs.spoint.global <- cluster_structure_measure_ari(localization, distance, "spoint", global.bol)
    ari.kmedoids.vs.spoint.ERA5 <- cluster_structure_measure_ari(localization, distance, "spoint", filter_traj(global.bol, reanalysis = "ERA5"))
    ari.kmedoids.vs.spoint.GDAS <- cluster_structure_measure_ari(localization, distance, "spoint", filter_traj(global.bol, reanalysis = "GDAS"))
  
    joint_cluster[[d]] <- list(BySampling = list(Global = ari.kmedoids.vs.sampling.global,
                                            ERA5 = ari.kmedoids.vs.sampling.ERA5,
                                            GDAS = ari.kmedoids.vs.sampling.GDAS),
                          BySpoint = list(Global = ari.kmedoids.vs.spoint.global,
                                          ERA5 = ari.kmedoids.vs.spoint.ERA5,
                                          GDAS= ari.kmedoids.vs.spoint.GDAS))
  # LOOCV global
     loocv_glob[[d]] <- loocv_global(localization, distance, mix)$CorrectClassificationRate
  }
  
  names(joint_cluster) <- Distances
  names(separate_cluster) <- Distances
  names(loocv_glob) <-  Distances
  
  OBJ = list(
    localization = localization,
    JOINTCLUST = joint_cluster,
    SEPCLUST = separate_cluster,
    LOOCV = loocv_glob
  )
  
  saveRDS(OBJ, paste0("output/analyzed/global/", localization,".rds"))
}


save_by_samplings("ANT")
save_by_samplings("GL")
save_by_spoints("ANT")
save_by_spoints("GL")

save_global_analysis("GL")
save_global_analysis("ANT")

# save_by_samplings("ANT", mix = 1:5/10)
# save_by_samplings("GL", mix = 1:5/10)
# save_by_spoints("ANT", mix = 1:5/10)
# save_by_spoints("GL", mix = 1:5/10)
# 
# save_by_samplings("ANT", mix = 6:10/10)
# save_by_samplings("GL", mix = 6:10/10)
# save_by_spoints("ANT", mix = 6:10/10)
# save_by_spoints("GL", mix = 6:10/10)