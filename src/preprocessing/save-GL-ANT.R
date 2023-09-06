source("src/preprocessing/save_hysplit-traj.R")

save.traj.obj(localization = "GL", reanalysis = "ERA5")
save.traj.obj(localization = "GL", reanalysis = "GDAS")

save.traj.obj(localization = "ANT", reanalysis = "ERA5")
save.traj.obj(localization = "ANT", reanalysis = "GDAS")