source("src/m3-supervised/loo-cv_by-sampling.R")
source("src/m3-supervised/loo-cv_by-spoint.R")

library(plot.matrix)

parameter_selection_plot <- function(localization, distance, weighted = F){
  
  if(localization =="ANT") global.bol <- btraj.ANT
  if(localization == "GL") global.bol <- btraj.GL
  
  if(localization =="ANT" ){
    global.bol <- btraj.ANT
    spoints <- 11:20
    samplings <- 18:37
  }
  if(localization == "GL"){
    global.bol <- btraj.GL
    spoints <- 1:10
    samplings <- 1:17
  }
  
  mix.groupings <- list(1:10/10, 1:5/10, 6:10/10, 1:3/10,4:6/10, 1:10/10, 1:2/10, 3:4/10, 5:6/10, 7:8/10, 9:10/10)
  parsel <- vector(mode = "list")
  parsel$Spoint <- vector(mode = "list",length =  length(mix.groupings))
  parsel$Sampling <- vector(mode = "list",length =  length(mix.groupings))
  i <- 0
    for (mix in mix.groupings){
      i <- i+1
      parsel$Spoint[[i]] <- sapply(loocv_spoint(localization, distance, mix), function(x) x$CorrectClassificationRate)
      parsel$Sampling[[i]] <- sapply(loocv_sampling(localization, distance, mix), function(x) x$CorrectClassificationRate)
  }
  parsel
}

plot_mixsel_spoint <- function(localization, distance){
  m <- as.data.frame(parameter_selection_plot(localization, distance)$Spoint)
  names(m) <- paste("G",1:dim(m)[2])
  m <- as.matrix(m)
  plot(m, main=paste(localization, distance, "; Grouped by SPOINT"))
}

plot_mixsel_sampling <- function(localization, distance){
  m <- as.data.frame(parameter_selection_plot(localization, distance)$Sampling)
  names(m) <- paste("G",1:dim(m)[2])
  m <- as.matrix(m)
  plot(m, main=paste(localization, distance, "; Grouped by SAMPLING"))
}

#No se por que dan error!? antes funcionaban

par(mar=c(5,5,5,5))

plot_mixsel_spoint("ANT", "Frechet")
plot_mixsel_sampling("ANT", "Frechet")

plot_mixsel_spoint("ANT", "Hellinger")
plot_mixsel_sampling("ANT", "Hellinger")


plot_mixsel_spoint("ANT", "Rdimf")
plot_mixsel_sampling("ANT", "Rdimf")



plot_mixsel_spoint("GL", "Frechet")
plot_mixsel_sampling("GL", "Frechet")

plot_mixsel_spoint("GL", "Hellinger")
plot_mixsel_sampling("GL", "Hellinger")

plot_mixsel_spoint("GL", "Rdimf")
plot_mixsel_sampling("GL", "Rdimf")

################3
bol <- filter_traj(btraj.ANT, mix_values = 1:5/10)
m <- get_distance_matrix("ANT", distance = "Frechet",bol )




