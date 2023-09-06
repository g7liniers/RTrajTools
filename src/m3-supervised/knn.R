#' @description
#' A KNN algorithm implementation with a custom distance matrix as an input
#' @returns boolean indicating if predicted class using loo cross val corresponds
#' to its real class
#' 
knn_cdm_loo <- function(labs, dm, k = NULL, weighted = FALSE){
  results <- vector(mode = "list", length = length(labs))
  n.trajs <- length(labels)
  if(is.null(k)) {
    k <- ceiling(sqrt(n.trajs))
    k <- min(k + (k%%2), 49)
  }
  for (i in 1:length(labs)){ 
    distances.to.i <- dm[, i]
    #Assuming d(x,y) = 0 <==>  x = y
    knn.indices <- order(distances.to.i)[2:(k+1)]
    knn.distances <- distances.to.i[knn.indices]
    knn.labs <- labs[knn.indices]

    if(weighted){
      vote_weights <- 1 - (knn.distances/sum(knn.distances))
      vote_weights <- vote_weights/sum(vote_weights)
    } else{
      vote_weights <- rep(1, k)/k
    }
    vote <- sum(vote_weights*knn.labs)

    
    if(vote == 0.5){
      predicted.class = knn.indices[1]
    } else{
      predicted.class = as.numeric(vote > 0.5)
    }
    results[i] <- (predicted.class == labs[i])
  }
  results
}
