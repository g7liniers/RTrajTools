#This functionality has been moved to src/sp1-trajectories/projected_frechet.R


library(sf)


source("src/loading/load_traj-arctic.R")
source("src/loading/load_traj-ant.R")


# ntraj.GL <- length(btraj.GL)
# ntraj.ANT <- length(btraj.ANT)
# 
# lstr.obj.list <- vector(mode = "list", length = ntraj)
# lstr.list <- vector(mode="list", length = ntraj)

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


frechet_dist <- function(linestr1, linestr2){
  return(sf::st_distance(linestr1,linestr2,which = "Frechet"))
}

frechet_dist_matrix <- function(btraj.obj.list){
  lstr.list <- traj_to_curves(btraj.obj.list)
  ntraj <- length(lstr.list)
  dist_matrix <- matrix(0,nrow = ntraj, ncol = ntraj)
  
  pb <- txtProgressBar(min=0,max =total.iter, )
  n.iter <- 0
  total.iter <- (ntraj*(ntraj-1))/2
  for (i in 1:(ntraj-1) ) {
    for (j in (i+1):ntraj){
      dist_matrix[i,j] <- frechet_dist(lstr.list[[i]],lstr.list[[j]])
      #indicate iteration number
      cat(paste(n.iter <- n.iter+1,"/",total.iter,"\r",sep = ""))
    }
  }
  
  sym_dist_matrix <- dist_matrix
  sym_dist_matrix[lower.tri(sym_dist_matrix)] <- t(sym_dist_matrix)[lower.tri(t(sym_dist_matrix))]
  
  return(sym_dist_matrix)
}

