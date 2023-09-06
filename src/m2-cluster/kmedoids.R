source("src/general-use-fcns/get-distance-matrix.R")
source("src/general-use-fcns/cluster-sizes.R")

custom_kmedoids <-
  function(distance_matrix, centers , max_iters = 100) {
    n.objects <- dim(distance_matrix)[1]
    
    # the case where centers is given as a number
    if (length(centers) == 1) {
      warning(paste(centers, "medoids chosen at random \n"))
      centers <- sample(1:n.objects, size = centers)
    }
    
    n.centers <- length(centers)
    clusters <- rep(0, n.objects)
    
    for (it in 1:max_iters) {
      
      # Assign elements to centers
      for (i in 1:n.objects) {
        closest.center.index <- 1
        for (j in 1:n.centers) {
          if (distance_matrix[i, centers[j]] < distance_matrix[i, centers[closest.center.index]]) {
            closest.center.index <- j
          }
        }
        
        clusters[i] <- closest.center.index
      }

      # Recalculate centers
      og.centers <- centers
      for (j in 1:n.centers) {
        clust.j.indices <- which(clusters == j)
        
        if(length(clust.j.indices) == 1){
          next
        }
        
        rdm <- distance_matrix[clust.j.indices, clust.j.indices]

        rowsums <- apply(rdm, 1, sum)
        center.rdm.index <- which.min(rowsums)
        centers[j] <- clust.j.indices[center.rdm.index]
      }
      
      # If there is no change in the centers, end the process
      if (identical(centers, og.centers)) break
    }
    
    cluster.sizes <- cluster_sizes(clusters)
    list(Centers = centers, Cluster = clusters, ClusterSizes = cluster.sizes)
  }




kmedoids_traj <-
  function(localization,
           distance,
           btraj.obj.list,
           centers) {
    dist.matrix <-
      get_distance_matrix(localization, distance, btraj.obj.list)
    
    custom_kmedoids(dist.matrix, centers = centers)
  }

#######################3
