source("src/m2-cluster/kmedoids.R")
source("src/general-use-fcns/filter-traj.R")
source("src/general-use-fcns/find-corresp-traj-position.R")
source("src/m2-cluster/deterministic-cluster/cluster-by-reanalysis.R")
source("src/m2-cluster/deterministic-cluster/cluster-by-sampling.R")
source("src/m2-cluster/deterministic-cluster/cluster-by-spoint.R")

library(aricode)

joint_clustering_2samplings <-
  function(s1, s2, distance, mix = 1:10 / 10) {
    isGL <- all(c(s1, s2) %in% 1:17)
    isANT <- all(c(s1, s2) %in% 18:37)
    if (!isGL & !isANT)
      stop("invalid sampling ids")
    
    if (isGL) {
      localization <- "GL"
      global.bol <- filter_traj(btraj.GL, mix_values = mix)
    }
    if (isANT) {
      localization <- "ANT"
      global.bol <- filter_traj(btraj.ANT, mix_values = mix)
    }
    
    
    bol <- filter_traj(global.bol, sampling_ids = c(s1, s2))
    # Determine starting centers
    
    bol.s1 <- filter_traj(bol, sampling_ids = s1)
    c.s1 <-
      btraj_position_bol(get_deepest_obj(localization, distance, bol.s1), bol)
    
    bol.s2 <- filter_traj(bol, sampling_ids = s2)
    c.s2 <-
      btraj_position_bol(get_deepest_obj(localization, distance, bol.s2), bol)
    
    kmedoids <-
      kmedoids_traj(localization, distance, bol, c(c.s1, c.s2))
    kmedoids.cluster <- kmedoids$Cluster
    
    reanalysis.cluster.obj <- cluster_by_reanalysis(bol)
    reanalysis.cluster <- reanalysis.cluster.obj$Cluster
    
    sampling.cluster.obj <- cluster_by_sampling(bol)
    sampling.cluster <- sampling.cluster.obj$Cluster
    
    kmed.v.reanal <-
      aricode::ARI(kmedoids.cluster, reanalysis.cluster)
    kmed.v.sampling <-
      aricode::ARI(kmedoids.cluster, sampling.cluster)
    reanal.v.sampling <- aricode::ARI(reanalysis.cluster, sampling.cluster)
    
    M2B.comparison.obj <-
      list(
        BySampling = T,
        BySpoint = F,
        GroupId1 = s1,
        GroupId2 = s2,
        KMedoids = kmedoids,
        GroupCluster = sampling.cluster.obj,
        ReanalysisCluster = reanalysis.cluster.obj,
        ARI1 = kmed.v.sampling,
        ARI2 =kmed.v.reanal,
        ARI3 = reanal.v.sampling
      )
    
    M2B.comparison.obj
  }


joint_clustering_2spoints <-
  function(sp1, sp2, distance, mix = 1:10 / 10) {
    isGL <- all(c(sp1, sp2) %in% 1:10)
    isANT <- all(c(sp1, sp2) %in% 11:20)
    if (!isGL & !isANT)
      stop("invalid sampling points")
    
    if (isGL) {
      localization <- "GL"
      global.bol <- filter_traj(btraj.GL, mix_values = mix)
    }
    if (isANT) {
      localization <- "ANT"
      global.bol <- filter_traj(btraj.ANT, mix_values = mix)
    }
    
    
    bol <- filter_traj(global.bol, sampling_points = c(sp1, sp2))
    # Determine starting centers
    
    bol.sp1 <- filter_traj(bol, sampling_points = sp1)
    c.sp1 <-
      btraj_position_bol(get_deepest_obj(localization, distance, bol.sp1), bol)
    
    bol.sp2 <- filter_traj(bol, sampling_points = sp2)
    c.sp2 <-
      btraj_position_bol(get_deepest_obj(localization, distance, bol.sp2), bol)
    
    kmedoids <-
      kmedoids_traj(localization, distance, bol, c(c.sp1, c.sp2))
    kmedoids.cluster <- kmedoids$Cluster
    
    reanalysis.cluster.obj <- cluster_by_reanalysis(bol)
    reanalysis.cluster <- reanalysis.cluster.obj$Cluster
    
    spoint.cluster.obj <- cluster_by_spoint(bol)
    spoint.cluster <- spoint.cluster.obj$Cluster
    
    kmed.v.reanal <-
      aricode::ARI(kmedoids.cluster, reanalysis.cluster)
    kmed.v.spoint <-
      aricode::ARI(kmedoids.cluster, spoint.cluster)
    reanal.v.spoint <- aricode::ARI(reanalysis.cluster, spoint.cluster)
    
    M2B.comparison.obj <-
      list(
        BySampling = F,
        BySpoint = T,
        GroupId1 = sp1,
        GroupId2 = sp2,
        KMedoids = kmedoids,
        GroupCluster = spoint.cluster.obj,
        ReanalysisCluster = reanalysis.cluster.obj,
        ARI1 = kmed.v.spoint,
        ARI2 =kmed.v.reanal,
        ARI3 = reanal.v.spoint
      )
    
    M2B.comparison.obj
  }

