source("src/sp3-pca/bol-to-fd.R")
source("src/general-use-fcns/unpack-trajs-from-bol.R")
source("src/loading/load_all-traj.R")

basis_fit_plot <-
  function(btraj.obj.list, basis, coord, title = "") {
    bol <- btraj.obj.list
    

    
    if (coord == "x") {
      fd <- bol_to_fd(bol, basis)$X
      M <- unpack_trajs_x(bol)
    } else{
      fd <- bol_to_fd(bol, basis)$Y
      M <- unpack_trajs_y(bol)
    }
    
    plot(fd, main = title)
    
    for (i in 1:dim(M)[2]) {
      points(1:121, M[, i],  cex = .5)
    }
    
  }

########
save_fit_plots <- function(basis, basis.name,bol.GL = btraj.GL, bol.ANT = btraj.ANT){
  dir.GL.x <- paste("output/figs/sp3-pca/basis-fit-plot/GL/",basis.name,"/x-coord/",sep = "")
  dir.GL.y <-  paste("output/figs/sp3-pca/basis-fit-plot/GL/",basis.name,"/y-coord/",sep = "")
  dir.ANT.x  <- paste("output/figs/sp3-pca/basis-fit-plot/ANT/",basis.name,"/x-coord/",sep = "")
  dir.ANT.y <- paste("output/figs/sp3-pca/basis-fit-plot/ANT/",basis.name,"/y-coord/",sep = "")
  
  directory_paths <- c(GL.x = dir.GL.x, GL.y = dir.GL.y, ANT.x = dir.ANT.x, ANT.y = dir.ANT.y)
  
  for(dir in directory_paths){
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  
  for (i in 1:length(bol.GL)){
    id <- bol.GL[[i]]$Metadata$Id
    
    # GL X COORD
    png(filename = paste0(dir.GL.x,id,".png"), width = 1080, height = 720)
  basis_fit_plot(bol.GL[i:i],basis = basis,coord = "x", title = paste("GL traj #",id, "; coord x; ", basis.name, sep = "") )
    dev.off()
    
    #GL Y COORD
    png(filename = paste0(dir.GL.y,id,".png"), width = 1080, height = 720)
    basis_fit_plot(bol.GL[i:i],basis = basis,coord = "y", title = paste("GL traj #",id, "; coord y; ", basis.name, sep = "") )
    dev.off()
  }
  
  for (i in 1:length(bol.ANT)){
    id <- bol.ANT[[i]]$Metadata$Id
    
    # ANT X COORD
    png(filename = paste0(dir.ANT.x,id,".png"), width = 1080, height = 720)
    basis_fit_plot(bol.ANT[i:i],basis = basis,coord = "x", title = paste("ANT traj #",id, "; coord x; ", basis.name, sep = "") )
    dev.off()
    
    #ANT Y COORD
    png(filename = paste0(dir.ANT.y,id,".png"), width = 1080, height = 720)
    basis_fit_plot(bol.ANT[i:i],basis = basis,coord = "y", title = paste("ANT traj #",id, "; coord y; ", basis.name, sep = "") )
    dev.off()
  }
}

test_basis <- function(basis, basis.name, sample.size = 100){
  btraj.obj.sample.GL <- btraj.GL[sample(1:length(btraj.GL), size = sample.size)]
  btraj.obj.sample.ANT <- btraj.ANT[sample(1:length(btraj.ANT), size = sample.size)]
  
  save_fit_plots(basis,basis.name, bol.GL = btraj.obj.sample.GL, bol.ANT = btraj.obj.sample.ANT)
}
