library(SimilarityMeasures)
library(sf)

source("src/general-use-fcns/extract_trajectory.R")

#' Compute the frechet distance matrix corresponding to a list of btraj objects
#' using SimilarityMeasures implementation. It is significantly slower than its 
#' sf counterpart!

frechet_dist_matrix_sm <- function(btraj.obj.list) {
  n.traj <- length(btraj.obj.list)
  dist_matrix <- matrix(0,nrow = n.traj, ncol = n.traj)
  traj.list <- extract_traj_list(btraj.obj.list)
  
  total.iter <- 0.5*n.traj*(n.traj - 1)
  print(paste("Computing ",total.iter, " Frechet distances using SimilarityMeasures method..."))
  pb <- txtProgressBar(min = 0, max =total.iter, style = 3)
  n.iter <- 0
  for (i in 1:(n.traj - 1)) {
    traj1 <- as.matrix(traj.list[[i]])
    for (j in (i + 1):n.traj) {
      cat(n.iter)
      setTxtProgressBar(pb, n.iter <- n.iter+1)
      traj2 <- as.matrix(traj.list[[j]])
      dist_matrix[i, j] <- Frechet(traj1,traj2)
    }
  }
  close(pb)
  #' For efficiency, we have just computed the upper diagonal of the distance
  #' of the distance matrix. Now we symmetrize it:
  sym_dist_matrix <- dist_matrix
  sym_dist_matrix[lower.tri(sym_dist_matrix)] <- t(sym_dist_matrix)[lower.tri(t(sym_dist_matrix))]
  
  sym_dist_matrix
}


traj_to_curves <- function(btraj.obj.list){
  ntraj <- length(btraj.obj.list)
  lstr.obj.list <- vector(mode = "list", length = ntraj)
  lstr.list <- vector(mode="list", length = ntraj)
  
  for (traj.obj in btraj.obj.list){
    id <- traj.obj$Metadata$Id
    traj <- matrix(data = c(traj.obj$Trajectory$x, traj.obj$Trajectory$y), ncol = 2)
    lstr <- sf::st_linestring(x = traj, dim = "XY")
    
    
    obj <- list(id, lstr)
    names(obj) <- c("Id","Linestring")
    
    lstr.obj.list[[id]] <- obj
    lstr.list[[id]] <- lstr
  }
  return(lstr.list)
  
}


frechet_dist_sf <- function(linestr1, linestr2){
  return(sf::st_distance(linestr1,linestr2,which = "Frechet"))
}

frechet_dist_matrix_sf <- function(btraj.obj.list){
  start.time <- Sys.time()
  lstr.list <- traj_to_curves(btraj.obj.list)
  ntraj <- length(lstr.list)
  dist_matrix <- matrix(0,nrow = ntraj, ncol = ntraj)
  
  n.iter <- 0
  total.iter <- (ntraj*(ntraj-1))/2
  print(paste("Computation of ", total.iter,"Frechet distances..."))
  pb <- txtProgressBar(min=0,max =total.iter,style = 3)
  for (i in 1:(ntraj-1) ) {
    for (j in (i+1):ntraj){
      setTxtProgressBar(pb,value = n.iter <- n.iter+1)
      dist_matrix[i,j] <- frechet_dist_sf(lstr.list[[i]],lstr.list[[j]])
      #indicate iteration number
      #cat(paste(n.iter <- n.iter+1,"/",total.iter,"\r",sep = ""))
    }
  }
  close(pb)
  
  sym_dist_matrix <- dist_matrix
  sym_dist_matrix[lower.tri(sym_dist_matrix)] <- t(sym_dist_matrix)[lower.tri(t(sym_dist_matrix))]
  
  timediff <- Sys.time() - start.time
  print(timediff)
  return(sym_dist_matrix)
}
