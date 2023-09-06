source("src/general-use-fcns/get-distance-matrix.R")
source("src/general-use-fcns/filter-traj.R")
library(plot.matrix)
library(qgraph)


plot_distance_matrix <- function(localization,
                                 distance,
                                 type = "matrix",
                                 reanalysis = NULL,
                                 sampling_points = NULL,
                                 sampling_ids = NULL,
                                 mix_values = NULL,
                                 min_sample_size = 0,
                                 max_sample_size = Inf,
                                 traj_ids = NULL,
                                 traj_pair_ids = NULL,
                                 traj_pcloud_ids = NULL) {
  if (localization == "ANT")
    global.bol <- btraj.ANT
  if (localization == "GL")
    global.bol <- btraj.GL
  bol <- filter_traj(
    global.bol,
    reanalysis,
    sampling_points,
    sampling_ids,
    mix_values,
    min_sample_size,
    max_sample_size,
    traj_ids,
    traj_pair_ids,
    traj_pcloud_ids
  )
  M <-
    get_distance_matrix(
      localization = localization,
      distance = distance,
      btraj.obj.list = bol
    )
  
  #matrix plot
  if(type == "matrix"){
    plot(M)
  }
  
  # hierarchical clustering + dendrogram
  if(type == "hclust"){
    hclust_result <- hclust(as.dist(M), method = "complete")
    plot(hclust_result, main = "Hierarchical Clustering Dendrogram",
         xlab = "Objects", ylab = "Distance")
  }

  if(type == "graph"){
    qgraph(1/M, layout='spring', vsize=3)
  }


  
  
}

plot_distance_matrix("GL", "Hellinger", "graph", sampling_ids = c(4),mix_values = c(1))

bol <- filter_traj(btraj.ANT,sampling_ids = 23, mix_values = 1)
M <- get_distance_matrix("ANT", "Frechet", bol )

qgraph(1-M, layout='spring', vsize=3)
