rdimf.ANT.X <- readRDS("output/rdimf/antartida/rdimf-ANT-X.rds")
rdimf.ANT.Y <- readRDS("output/rdimf/antartida/rdimf-ANT-Y.rds")

rdimf.GL.X <- readRDS("output/rdimf/groenlandia/rdimf-GL-X.rds")
rdimf.GL.Y <- readRDS("output/rdimf/groenlandia/rdimf-GL-Y.rds")

rdimf_dist_matrix <- function(rdimf.X, rdimf.Y){
  n.obs <- dim(rdimf.X)[1]
  dist.matrix = matrix(0, nrow = n.obs, ncol = n.obs)
  
  names(rdimf.X) <- paste(names(rdimf.X), "X", sep = "_")
  names(rdimf.Y) <- paste(names(rdimf.Y), "Y", sep = "_")
  
  
  n.it <- 0
  total.iterations <- (n.obs-1)*n.obs/2
  cat("\r Computing euclidean distances between dimension reduced trajectories \n")
  pb <- txtProgressBar(min = 1, max = total.iterations, initial = 1, style = 3)
  for(i in 1:(n.obs-1)){
    for (j in i:n.obs){
      setTxtProgressBar(pb, n.it <- n.it + 1)
      x1 <- as.vector(rdimf.X[i,], mode = "numeric")
      y1 <- as.vector(rdimf.Y[i,], mode = "numeric")
      x2 <- as.vector(rdimf.X[j,], mode = "numeric")
      y2 <- as.vector(rdimf.Y[j,], mode = "numeric")
      
      dist.matrix[i,j] <- sqrt(sum((c(x1,y1) - c(x2,y2))^2))
    }
  }
  
  close(pb)
  
  sym_dist_matrix <- dist.matrix
  sym_dist_matrix[lower.tri(sym_dist_matrix)] <- t(sym_dist_matrix)[lower.tri(t(sym_dist_matrix))]
  
  sym_dist_matrix
}
