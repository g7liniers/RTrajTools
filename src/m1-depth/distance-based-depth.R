source("src/general-use-fcns/reduce_dist_matrix.R")
source("src/general-use-fcns/get-distance-matrix.R")
source("src/loading/load_all-dist.R")
source("src/general-use-fcns/filter-traj.R")

#' returns deepest object within a list. these objects must have an id attribute
#' accesible by '$Metadata$Id', related to the index in a distance matrix
get_deepest_obj <- function(localization, distance, btraj.obj.list){
  dm <- get_distance_matrix(localization, distance, btraj.obj.list)
  
  objective_fcn <- rowSums(dm)
  minimizer_index <- which(objective_fcn == min(objective_fcn))
  
  if(length(minimizer_index) > 1) warning("Multiple minimizers where found while searching for the deepest element")
  minimizer_index <- minimizer_index[1]
  
  return(btraj.obj.list[[minimizer_index]])
}

get_mean_and_sd_distance_to_deepest <- function(localization, distance, btraj.obj.list){
  dm <- get_distance_matrix(localization, distance, btraj.obj.list)
  
  objective_fcn <- rowSums(dm)
  minimizer_index <- which(objective_fcn == min(objective_fcn))
  
  if(length(minimizer_index) > 1) warning("Multiple minimizers where found while searching for the deepest element")
  minimizer_index <- minimizer_index[1]
  
  return(list(mean = mean(dm[minimizer_index, ]), sd = sd(dm[minimizer_index, ])))
}

#' returns a M1.comparison object relative to the 2 objs provided
#' TODO: remove obj lists and distance matrix parameter and retrieve it dynamically
#' using standpoint and localization
get_depth_comparison_obj <- function(localization, distance, obj.list.1, obj.list.2) {
  M1.comparison.obj <- list()

  deepest.btraj.1 <- get_deepest_obj(localization, distance, obj.list.1)
  deepest.btraj.2 <- get_deepest_obj(localization, distance, obj.list.2)
  
  M1.comparison.obj$Deepest.1 <- deepest.btraj.1
  M1.comparison.obj$Deepest.2 <- deepest.btraj.2
  
  deep.id1 <- M1.comparison.obj$Deepest.1$Metadata$Id
  deep.id2 <- M1.comparison.obj$Deepest.2$Metadata$Id
  
  deepest.dist <- get_distance_matrix(localization, distance, list(deepest.btraj.1, deepest.btraj.2))[1,2]
  meansd1 <- get_mean_and_sd_distance_to_deepest(localization, distance, obj.list.1)
  meansd2 <- get_mean_and_sd_distance_to_deepest(localization, distance, obj.list.2)
  
  mean1 <- meansd1$mean
  mean2 <- meansd2$mean
  sd1 <- meansd1$sd
  sd2 <- meansd2$sd
  
  M1.comparison.obj$DeepestsDistance <- deepest.dist
  M1.comparison.obj$MeanDist1 <- mean1
  M1.comparison.obj$MeanDist2 <- mean2
  M1.comparison.obj$SdDist1 <- sd1
  M1.comparison.obj$SdDist2 <- sd2
  norm.deep.dist.1 <- (deepest.dist-mean1)/sd1
  norm.deep.dist.2 <- (deepest.dist - mean2)/sd2
  M1.comparison.obj$NormalizedDeepestsDistance <- mean(c(norm.deep.dist.1, norm.deep.dist.2))
  
  # M1.comparison.obj$DistanceDistribution.G1.D1 <- distance.matrix[ids1, deep.id1]
  # M1.comparison.obj$DistanceDistribution.G2.D2 <- distance.matrix[ids2, deep.id2]
  # M1.comparison.obj$DistanceDistribution.G1.D2 <- distance.matrix[ids1, deep.id2]
  # M1.comparison.obj$DistanceDistribution.G2.D1 <- distance.matrix[ids2, deep.id1]
    
  # Fill the rest of the object's properties
  M1.comparison.obj$Localization <- localization
  M1.comparison.obj$Distance <- distance
  
  M1.comparison.obj
}


#

depth_analysis_2samplings <- function(s1, s2, distance, mix = 1:10/10){
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
  
  bol.s1 <- filter_traj(global.bol, sampling_ids = s1)
  bol.s2 <- filter_traj(global.bol, sampling_ids = s2)
  
  get_depth_comparison_obj(localization, distance, bol.s1, bol.s2)
}

depth_analysis_2spoints <- function(sp1, sp2, distance, mix = 1:10/10){
  isGL <- all(c(sp1, sp2) %in% 1:10)
  isANT <- all(c(sp1, sp2) %in% 11:20)
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
  
  bol.s1 <- filter_traj(global.bol, sampling_points = sp1)
  bol.s2 <- filter_traj(global.bol, sampling_points = sp2)
  
  get_depth_comparison_obj(localization, distance, bol.s1, bol.s2)
}

distance_2reanalysis_1sampling <- function(sampling.id, distance, mix = 1:10/10){
  isGL <- (sampling.id %in% 1:17)
  isANT <- (sampling.id %in% 18:37)

  if (!isGL & !isANT)
    stop("invalid sampling id")
  
  if (isGL) {
    localization <- "GL"
    global.bol <- filter_traj(btraj.GL, mix_values = mix)
  }
  if (isANT) {
    localization <- "ANT"
    global.bol <- filter_traj(btraj.ANT, mix_values = mix)
  }
  
  bol.ERA5 <- filter_traj(global.bol,reanalysis = "ERA5", sampling_ids = sampling.id)
  bol.GDAS <-  filter_traj(global.bol,reanalysis = "GDAS", sampling_ids = sampling.id)
  
  get_depth_comparison_obj(localization, distance, bol.ERA5, bol.GDAS)$DeepestsDistance
}

distance_2reanalysis_1spoint <- function(spoint.id, distance, mix = 1:10/10){
  isGL <- (spoint.id %in% 1:10)
  isANT <- (spoint.id %in% 11:20)
  
  if (!isGL & !isANT)
    stop("invalid sampling id")
  
  if (isGL) {
    localization <- "GL"
    global.bol <- filter_traj(btraj.GL, mix_values = mix)
  }
  if (isANT) {
    localization <- "ANT"
    global.bol <- filter_traj(btraj.ANT, mix_values = mix)
  }
  
  bol.ERA5 <- filter_traj(global.bol,reanalysis = "ERA5", sampling_points = spoint.id)
  bol.GDAS <-  filter_traj(global.bol,reanalysis = "GDAS", sampling_points = spoint.id)
  
  get_depth_comparison_obj(localization, distance, bol.ERA5, bol.GDAS)$DeepestsDistance
}


######



