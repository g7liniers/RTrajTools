source("src/loading/load_all-traj.R")
source("src/general-use-fcns/get-distance-matrix.R")
source("src/general-use-fcns/filter-traj.R")
source("src/m3-supervised/knn.R")
source("src/general-use-fcns/extract-reanalysis.R")

#' 
loocv_spoint <- function(localization, distance, mix = 1:10/10, k = NULL, weighted = F){
  if(localization =="ANT"){
    global.bol <- btraj.ANT
    spoints <- 11:20
  }
  if(localization == "GL"){
    global.bol <- btraj.GL
    spoints <- 1:10
  }
  
  n.spoints <- length(spoints)
  
  loocv <- vector(mode = "list", length = n.spoints)
  for(i in 1:n.spoints){
    bol <- filter_traj(global.bol, sampling_points = spoints[i], mix_values = mix)
    dm <- get_distance_matrix(localization, distance, bol)
    labs <- extract_binary_reanalysis(bol)
    
    results <- knn_cdm_loo(labs, dm, k, weighted)
    num.results <- as.numeric(results)
    
    loocv[[i]]$SPoint <- spoints[i]
    loocv[[i]]$CorrectClassificationRate <- sum(num.results)/length(results)
    loocv[[i]]$CorrectClassificationResults <- results
  }
  loocv
}

loocv_spoint_mairp <- function(k=NULL, mix = 1:10/10){
  #ANT
  loocv.spoint.ANT.frechet <- loocv_spoint("ANT", "Frechet", mix)
  loocv.spoint.ANT.hellinger <- loocv_spoint("ANT", "Hellinger", mix)
  loocv.spoint.ANT.rdimf <- loocv_spoint("ANT", "Rdimf", mix)
  
  #GL
  loocv.spoint.GL.frechet <- loocv_spoint("GL", "Frechet", mix)
  loocv.spoint.GL.hellinger <- loocv_spoint("GL", "Hellinger", mix)
  loocv.spoint.GL.rdimf <- loocv_spoint("GL", "Rdimf", mix)
}
