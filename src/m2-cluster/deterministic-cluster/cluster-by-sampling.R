source("src/general-use-fcns/cluster-sizes.R")

cluster_by_sampling <- function(btraj.obj.list){
  bol <- btraj.obj.list
  
  clusters <- sapply(bol, function(x) x$Metadata$SamplingId)
  spoints <- unique(clusters)
  
  for (i in 1:length(clusters)){
    for (j in 1:length(spoints)){
      if(clusters[i] == spoints[j]){
        clusters[i] <- j
        break
      }
    }
  }
  
  cluster.sizes <- cluster_sizes(clusters)
  list(Cluster = clusters, ClusterSizes = cluster.sizes)
}
