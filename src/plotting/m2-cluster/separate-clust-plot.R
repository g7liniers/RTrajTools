source("src/m2-cluster/separate-clustering.R")
library(qgraph)
library(plot.matrix)

ari_similarity_plot <- function(localization, distance, doSamplings = T, mix = 1:10/10) {
  if (localization == "ANT") {
    samplings <- 18:37
    spoints <- 11:20
  }
  if (localization == "GL") {
    samplings <- 1:17
    spoints <- 1:10
  }
  
  if(doSamplings){
    n.samplings <- length(samplings)
    similarity.mat <-
      matrix(1, ncol = n.samplings, nrow = n.samplings)
    
    for (i in 1:(n.samplings-1)) {
      for (j in (i+1):n.samplings) {
        s1 <- samplings[i]
        s2 <- samplings[j]
        
        ari <-  separate_clustering_2samplings(s1,s2, distance, mix)$ARI
        similarity.mat[i, j] <- ari
        similarity.mat[j,i] <- ari
      }
    }
  } else{
    n.spoints <- length(spoints)
    similarity.mat <-
      matrix(1, ncol = n.spoints, nrow = n.spoints)
    
    for (i in 1:(n.spoints-1)) {
      for (j in (i+1):n.spoints) {
        sp1 <- spoints[i]
        sp2 <- spoints[j]
        
        ari <-  separate_clustering_2spoints(sp1,sp2, distance, mix)$ARI
        similarity.mat[i, j] <- ari
        similarity.mat[j,i] <- ari
        
      }
    }
    
  }
  # qgraph(similarity.mat, layout='spring', vsize=3)
  plot(similarity.mat)
}
