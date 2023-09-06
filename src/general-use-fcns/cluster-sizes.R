cluster_sizes <- function(cluster){
  groups <- sort(unique(cluster))
  counts <- sapply(groups, function(group) sum(cluster == group))
  return(counts)
}
