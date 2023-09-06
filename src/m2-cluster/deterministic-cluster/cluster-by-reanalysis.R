source("src/general-use-fcns/cluster-sizes.R")

cluster_by_reanalysis <- function(btraj.obj.list){
  
  clusters <- as.numeric(sapply(btraj.obj.list, check_ERA5)) + 1
  cluster.sizes <- cluster_sizes(clusters)
  list(Cluster = clusters, ClusterSizes = cluster.sizes)
}

check_ERA5 <- function(btraj.obj){
  reanalysis <- btraj.obj$Metadata$Reanalysis
  if(is.null(reanalysis)) stop("Reanalysis property is NULL")
  isSupported <- reanalysis %in% c("ERA5", "GDAS") 
  if(!isSupported) stop("Reanalysis property must be 'ERA5' or 'GDAS'" )
  if(reanalysis == "ERA5") return(T)
  else(return(F))
}
