source("src/general-use-fcns/filter-traj.R")

btraj.GL.ERA5 <- readRDS("data/groenlandia/processed/unsampled/groenlandia_ERA5_btraj.rds")
btraj.GL.GDAS <- readRDS("data/groenlandia/processed/unsampled/groenlandia_GDAS_btraj.rds")
btraj.ANT.ERA5 <- readRDS("data/antartida/processed/unsampled/antartida_ERA5_btraj.rds")
btraj.ANT.GDAS <- readRDS("data/antartida/processed/unsampled/antartida_GDAS_btraj.rds")


# Filter out the trajectories with no associated sampling
sampled.btraj.GL.ERA5 <- filter_traj(btraj.GL.ERA5, sampling_ids = 1:17, sampling_points = 1:10)
sampled.btraj.GL.GDAS <- filter_traj(btraj.GL.GDAS, sampling_ids = 1:17, sampling_points = 1:10)

sampled.btraj.ANT.ERA5 <- filter_traj(btraj.ANT.ERA5, sampling_ids = 18:37)
sampled.btraj.ANT.GDAS <- filter_traj(btraj.ANT.GDAS, sampling_ids = 18:37)

#Save the filtered lists
saveRDS(sampled.btraj.GL.ERA5,file = "data/groenlandia/processed/sampled/groenlandia_sampled_ERA5_btraj.rds")
saveRDS(sampled.btraj.GL.GDAS,file = "data/groenlandia/processed/sampled/groenlandia_sampled_GDAS_btraj.rds")

saveRDS(sampled.btraj.ANT.ERA5,file = "data/antartida/processed/sampled/antartida_sampled_ERA5_btraj.rds")
saveRDS(sampled.btraj.ANT.GDAS,file = "data/antartida/processed/sampled/antartida_sampled_GDAS_btraj.rds")

