library(fda)
source("src/loading/fd-object/load_all-fd-objs.R")


pca_dimred <- function(fd, var.th = .99, n.harm = 10){
  pca <- pca.fd(fd,nharm = 10)
  varprop <- pca$varprop
  print(paste("Proportion of explained variability"))
  varprop.cs <- cumsum(varprop)
  
  is.geq.th <- varprop.cs > var.th
  indices <- which(is.geq.th == TRUE)
  
  if(length(indices) == 0) {
    n.basis <- n.harm
    warning("More harmonics needed in order to meet variability sum threshold")
  } else{
    n.basis <- indices[1]
  }
  
  print(paste("Harmonics used: ", n.basis, ". Variability sum achieved:", sum(varprop)))
  pca
}

