source("src/sp2-pointcloud/hellinger-dist.R")
source("src/general-use-fcns/groupings.R")

#' @description
#' Given a trajectory group object (GO), pointcloud estimated densities are
#' computed and their hellinger distances are encoded in a distance matrix.
#'
hellinger_dist_matrix_grouped <- function(GO, gridpoints = 25) {
  n.groups <- length(GO$BOLL)
  distance.matrix <- matrix(0, nrow = n.groups, ncol = n.groups)
  
  total.iter <- 0.5 * n.groups * (n.groups - 1)
  print(paste("Computing  ", total.iter, " Hellinger distances...",sep=""))
  # pb <- txtProgressBar(min = 0, max = total.iter, style = 3)
  n.iter <- 0
  time.cum.sum <- 0
  for (i in 1:(n.groups-1)) {
    for (j in (i + 1):n.groups) {
      #
      init.time.iter <- Sys.time()
      #
      bol1 <- GO$BOLL[[i]]
      bol2 <- GO$BOLL[[j]]
      
      distance.matrix[i, j] <-
        hellinger_dist(bol1, bol2, gridpoints = gridpoints)
      
      # # setTxtProgressBar(pb, n.iter <- n.iter+1)
      # message(paste(n.iter <- n.iter+1,"/",total.iter,sep = ""))
      # #
      # print(paste("Iteration total time:", end.time.iter - init.time.iter, "s(?)"))
      # #
      timediff <- Sys.time() - init.time.iter
      time.cum.sum <- time.cum.sum + timediff
      mean.iter.time <- time.cum.sum/n.iter
      est.remaining.time <- (total.iter - n.iter)*mean.iter.time
      
      cat(paste("\r Iteration",n.iter <- n.iter+1,"/",total.iter, "; ", "Iteration time:", timediff, "; Remaining time: ", est.remaining.time))
    }
  }
  # close(pb)
  
  sym_dist_matrix <- distance.matrix
  sym_dist_matrix[lower.tri(sym_dist_matrix)] <- t(sym_dist_matrix)[lower.tri(t(sym_dist_matrix))]
  
  sym_dist_matrix
}

#' For larger datasets, this will definitely be slow. It would be best to save 
#' the kdes and iteratively compute their distances,
#' instead of computing them each time. 

hellinger_dist_matrix_reanal_pcid <- function(btraj.obj.list, gridpoints = 25){
  
  max.poincloud.id <- max(sapply(btraj.obj.list, function(x) x$Metadata$PCloudId))
  GO.reanal.pcid <- group_btraj.list(btraj.obj.list = btraj.obj.list, reanalysis = c("ERA5", "GDAS"), traj_pcloud_ids = 1:max.poincloud.id)
  
  dist.matrix <- hellinger_dist_matrix_grouped(GO.reanal.pcid,gridpoints = gridpoints)
  dist.matrix
}

hellinger_dist_matrix_traj_id <- function(btraj.obj.list, gridpoints = 25){
  max.id <- max(sapply(btraj.obj.list, function(x) x$Metadata$Id))
  GO.id <- group_btraj.list(btraj.obj.list, traj_ids = 1:max.id)
  
  dist.matrix <- hellinger_dist_matrix_grouped(GO.id, gridpoints = gridpoints)
}

