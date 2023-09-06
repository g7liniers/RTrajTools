source("src/preprocessing/simple-features/traj-to-curves.R")

source("src/loading/load_traj-arctic.R")
source("src/loading/load_traj-ant.R")

frechet.GL <- frechet_dist_matrix(btraj.GL)
saveRDS(frechet.GL,file = "data/groenlandia/processed/dist_frechet_v2_GL.rds")

frechet.ANT <- frechet_dist_matrix(btraj.ANT)
saveRDS(frechet.GL,file = "data/antartida/processed/dist_frechet_ANT.rds")
