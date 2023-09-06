source("src/sp2-pointcloud/hellinger-dist.R")
source("src/loading/load_all-traj.R")

plot_contour_densities <- function(btraj.obj.list, reanalysis, sampling.id, spoint )

for (spoint in 1:10) {
  era5 <-
    filter_traj(btraj.GL,
                reanalysis = "ERA5",
                sampling_points = spoint)
  gdas <-
    filter_traj(btraj.GL,
                reanalysis = "GDAS",
                sampling_points = spoint)
  lims.era5 <- get_density_lims(era5)
  lims.gdas <- get_density_lims(gdas)
  lims.joint <- get_density_lims(c(era5, gdas))
  
  dens.era5 <- estimate_2d_density_trajs(era5,n = 200, lims = lims.era5)
  dens.gdas <- estimate_2d_density_trajs(gdas, n=200, lims=lims.gdas)
  
  
  
  png(
    paste(
      "output/figs/sp2-pointcloud/density-by-spoint-reanal/ERA5/ERA5_spoint_",
      spoint,
      ".png",
      sep = ""
    ),
    width = 1080,
    height = 720
  )
  
  filled.contour(
    dens.era5,
    xlim = lims.era5[1:2],
    ylim = lims.era5[3:4],
    main = "Pointcloud density",
  )
  title(sub = paste("reanalysis = ERA5, spoint = ", spoint), line = -2)
  
  dev.off()
  
  
  png(
    paste(
      "output/figs/sp2-pointcloud/density-by-spoint-reanal/GDAS/GDAS_spoint-",
      spoint,
      ".png"
    ),
    width = 3840,
    height = 2160
  )
  
  filled.contour(
    dens.gdas,
    xlim = lims.gdas[1:2],
    ylim = lims.gdas[3:4],
    main = "Pointcloud density",
  )
  title(sub = paste("reanalysis = ERA5, spoint = ", spoint), line = -2)
  
  dev.off()
  
  png(
    paste(
      "output/figs/sp2-pointcloud/density-by-spoint-reanal/joint/joint_spoint-",
      spoint,
      ".png"
    ),
    width = 1080,
    height = 720
  )
  
  dev.off()
}

##########################3
library(ggplot2)

dens.era5 <- estimate_2d_density_trajs(filter_traj(btraj.GL,reanalysis = "ERA5", sampling_ids = 1))
dens.gdas <- filter_traj(btraj.GL,reanalysis = "GDAS", sampling_ids = 1)


# Example data
x <- dens.era5$x
y <- dens.era5$y
X <- dens.era5$x
Y <- dens.era5$y


coord_to_index <- function(vector, value){
  index <- which(vector == value)
  if(length(index)>1) warning(paste("Repeated values in grid vector:",index))
  index[1]
}

z1 <- outer(x, y, function(x,y) dens.era5$z[coord_to_index(X,x),coord_to_index(Y,y)])
z2 <- outer(x, y, function(x, y) x + y)

# Combine the data into a data frame
data1 <- expand.grid(x = x, y = y)
data1$z <- as.vector(z1)

data2 <- expand.grid(x = x, y = y)
data2$z <- as.vector(z2)

# Create a ggplot object
gg <- ggplot()

# Add the first contour plot
gg <- gg + geom_contour(data = data1, aes(x, y, z = z), bins = 20, color = "blue")

# Add the second contour plot on top
gg <- gg + geom_contour(data = data2, aes(x, y, z = z), bins = 20, color = "red")

# Add titles and labels
gg <- gg + labs(title = "Overlapping Contour Plots", x = "X", y = "Y")

# Display the plot
print(gg)
