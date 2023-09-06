source("src/loading/load_all-traj.R")
source("src/general-use-fcns/get-distance-matrix.R")
source("src/general-use-fcns/filter-traj.R")
source("src/m3-supervised/knn.R")
source("src/general-use-fcns/extract-reanalysis.R")

#' 
loocv_sampling <- function(localization, distance, mix = 1:10/10, k = NULL, weighted = F){
  if(localization =="ANT"){
    global.bol <- btraj.ANT
    samplings <- 18:37
  }
  if(localization == "GL"){
    global.bol <- btraj.GL
    samplings <- 1:17
  }
  
  n.samplings <- length(samplings)
  
  loocv <- vector(mode = "list", length = n.samplings)
  for(i in 1:n.samplings){
    bol <- filter_traj(global.bol, sampling_ids = samplings[i], mix_values = mix)
    dm <- get_distance_matrix(localization, distance, bol)
    labs <- extract_binary_reanalysis(bol)
    
    results <- knn_cdm_loo(labs, dm, k,weighted)
    num.results <- as.numeric(results)
    
    loocv[[i]]$SamplingId <- samplings[i]
    loocv[[i]]$CorrectClassificationRate <- sum(num.results)/length(results)
    loocv[[i]]$CorrectClassificationResults <- results
    loocv[[i]]$Reanalysis <- extract_reanalysis(bol)
  }
  loocv
}

loocv_global <- function(localization,distance, mix = 1:10/10, k = NULL, weighted = F){
  if(localization =="ANT"){
    global.bol <- btraj.ANT
  }
  if(localization == "GL"){
    global.bol <- btraj.GL
  }
  
  bol <- filter_traj(global.bol,  mix_values = mix)
  dm <- get_distance_matrix(localization, distance, bol)
  labs <- extract_binary_reanalysis(bol)
  results <- knn_cdm_loo(labs, dm, k, weighted)
  num.results <- as.numeric(results)
  
  loocv <- list(CorrectClassificationRate = sum(num.results)/length(results), 
                CorrectClassificationResults = results,
                Reanalysis = extract_reanalysis(bol))
}

loocv_sampling_mairp <- function(k=NULL, mix = 1:10/10){
  #ANT
  loocv.sampling.ANT.frechet <<- loocv_sampling("ANT", "Frechet", mix)
  loocv.sampling.ANT.hellinger <<- loocv_sampling("ANT", "Hellinger", mix)
  loocv.sampling.ANT.rdimf <<- loocv_sampling("ANT", "Rdimf", mix)
  
  #GL
  loocv.sampling.GL.frechet <<- loocv_sampling("GL", "Frechet", mix)
  loocv.sampling.GL.hellinger <<- loocv_sampling("GL", "Hellinger", mix)
  loocv.sampling.GL.rdimf <<- loocv_sampling("GL", "Rdimf", mix)
}

