source("src/sp2-pointcloud/density-estimation.R")
source("src/sp2-pointcloud/hellinger-dist-computation.R")
source("src/loading/load_all-traj.R")

global.dens.ANT <- estimate_2d_density_trajs(btraj.ANT)
global.dens.GL <- estimate_2d_density_trajs(btraj.GL)

filled.contour(global.dens.GL,
               xlim = c(-200, 200),
               ylim = c(-500, 500))
filled.contour(global.dens.ANT)



# plot_density <- function(btraj.obj.list)
g1 <- btraj.ANT[1:10]
g2 <- btraj.ANT[1691:1700]
g3 <- btraj.ANT[c(23, 8, 1032, 234, 103, 123, 186, 111, 2222, 222)]

g1 <-
  filter_traj(btraj.GL,
              reanalysis = "ERA5",
              sampling_points = c(1))
g2 <-
  filter_traj(btraj.GL,
              reanalysis = "GDAS",
              sampling_points = c(1))

#make a mock traj group for testing - punctual mass
coordx <- seq(0, 500000, by = 500000 / 121)
coordy <- seq(1000000, 900000, by = -100000 / 121)

mock.btraj <- btraj.GL[[1232]]
n.points <- mock.btraj$Metadata$SampleSize
mock.btraj$Trajectory$xProj = coordx
mock.btraj$Trajectory$yProj = coordy

g3 <- list(mock.btraj)

lims <- get_density_lims(c(g1, g2, g3))

dens.g1 <- estimate_2d_density_trajs(g1)
dens.g2 <- estimate_2d_density_trajs(g2)
dens.g3 <- estimate_2d_density_trajs(g3)

filled.contour(dens.g1, xlim = lims[1:2], ylim = lims[3:4])
filled.contour(dens.g2, xlim = lims[1:2], ylim = lims[3:4])
filled.contour(dens.g3, xlim = lims[1:2], ylim = lims[3:4])

hellinger_dist(g1, g2, gridpoints = 200)
hellinger_dist(g1, g3, gridpoints = 200)
hellinger_dist(g2, g3, gridpoints = 200)


##################################

GO.GL.reanal.sid <- group_btraj.list(btraj.GL, reanalysis = c("ERA5", "GDAS"), sampling_ids = 1:17)
GO.ANT.reanal.sid <- group_btraj.list(btraj.ANT, reanalysis = c("ERA5", "GDAS"), sampling_ids = 18:36)

MGL <- hellinger_dist_matrix_grouped(GO.GL.reanal.sid)
# distancias entre las asociadas por sampling de cada reanalisis
paired_dist <- diag(MGL[-34,-1])[(1:17)*2-1]