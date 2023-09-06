library(ggplot2)
library(hexbin)
library(gridExtra)
source("src/general-use-fcns/get_legend.R")

source("src/loading/load_all-traj.R")
source("src/general-use-fcns/filter-traj.R")

length.vs.discpoints <- function(btraj.obj.list) {
  lengths <-
    sapply(btraj.obj.list, function(obj) {
      obj$Metadata$TrajLength / 1000
    })
  discpoints <-
    sapply(btraj.obj.list, function(obj) {
      obj$Metadata$SampleSize
    })
  reanalysis <-
    sapply(btraj.obj.list, function(obj)
      obj$Metadata$Reanalysis)
  
  data.frame(length = lengths,
             discpoint = discpoints,
             reanalysis = reanalysis)
}



gen_plotlist <- function(df, rowtitle) {
  # Pair Plot
  pair_plot <-
    ggplot(df, aes(x = discpoint, y = length, color = reanalysis)) +
    geom_point(alpha = .2, size = .5) +
    # scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +
    geom_smooth(method = "lm", color = "blue") +
    labs(title = rowtitle, x = "NDISCP", y = "LEN") + guides(color = "none")
  
  # Area + contour
  density_2d <- ggplot(df, aes(x = discpoint, y = length)) +
    stat_density_2d(aes(fill = ..level..),
                    geom = "polygon",
                    colour = "white") + guides(fill = "none")
  
  # Density of #discpoints
  density_discpoints <-
    density_plot <-
    ggplot(data = df, aes(x = discpoint, fill = reanalysis)) +
    # geom_density(fill = "skyblue",
    #              alpha = 0.6,
    #              color = "darkblue") +
    geom_density(alpha = .4) +
    labs(title = "" ,#"NDISCP Denslity", 
         x = "NDISCP", y = "Density") + guides(fill = "none")
  
  # Density of length
  density_length <-
    density_plot <-
    ggplot(data = df, aes(x = length, fill = reanalysis)) +
    # geom_density(fill = "skyblue",
    #              alpha = 0.6,
    #              color = "darkblue") +
    geom_density(alpha = .4) +
    labs(title = "", # "LEN Density",
         x = "LEN", y = "Density") + guides(fill = "none")
  
  # Scaling of legend - must provide both trans and inv functions
  hexbinplot <-
    hexbinplot(length ~ discpoint,
               data = df,
               trans = log,
               inv = exp)
  
  # histograms
  histogram_discpoints <-
    ggplot(df, aes(x = discpoint, fill = reanalysis)) +
    geom_histogram(alpha = .6) +
    labs(title = "",  x = "NDISCP") + guides(fill = "none")
  
  histogram_length <-
    ggplot(df, aes(x = length, fill = reanalysis)) +
    geom_histogram(alpha = .6) +
    labs(title = "",  x = "LEN")
  
  gridplot_legend <<- get_legend(histogram_length)
  
  
  histogram_length <- histogram_length + guides(fill = "none")
  
  plotlist <-
    list(
      pair_plot,
      # density_2d,
      # hexbinplot,
      density_discpoints,
      density_length,
      histogram_discpoints,
      histogram_length,
      gridplot_legend
      # Important! no comma after the last element
    )
  plotlist
}



gridplot <- function(fam, n.row, n.col) {
  plotlist <- list()
  titles = c("LOW", "HIGH", "ALL")
  for (i in 1:3) {
    plotlist <- append(plotlist, gen_plotlist(fam[[i]], titles[i]))
  }
  do.call(grid.arrange, c(plotlist, ncol = n.col, nrow = n.row))
  #  + theme(legend.position = "bottom", legend.direction = "horizontal",
  #         legend.key = element_rect(fill = "white"),
  #         legend.box = "horizontal", guide = "combine")
  
}


#############3 usage

save_poster_plots <- function() {
  mix_sets <- list(1:5 / 10, 6:10 / 10, 1:10 / 10)
  ANT.fam <- list()
  GL.fam <- list()
  
  for (i in 1:3) {
    mix <- mix_sets[[i]]
    
    ANT <- filter_traj(btraj.ANT, mix_values = mix)
    GL <- filter_traj(btraj.GL, mix_values = mix)
    
    
    ANT <- length.vs.discpoints(ANT)
    GL <- length.vs.discpoints(GL)
    
    # ANT.fam <- list(ANT, ANT.ERA5, ANT.GDAS)
    # GL.fam <- list(GL, GL.ERA5, GL.GDAS)
    ANT.fam[[i]] <- ANT
    GL.fam [[i]] <- GL
    
    
    type = c("low", "high", "global")[i]
    
  }
  
  savepath <- "output/figs/preanalysis/length-ndiscp/"
  
  
  filepath.ANT <- paste0(savepath, "poster-ANT", ".png")
  png(filename = filepath.ANT,
      width = 1080,
      height = 720)
  gridplot(ANT.fam, n.row = 3, n.col = 6)
  dev.off()
  
  filepath.GL <- paste0(savepath, "poster-GL", ".png")
  png(filename = filepath.GL,
      width = 1080,
      height = 720)
  gridplot(GL.fam, n.row = 3, n.col = 6)
  dev.off()
  
}

# Combinar las filas en un solo plot!. pasar de 6 a 2 posters
