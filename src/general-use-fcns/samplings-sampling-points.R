source("src/loading/load_all-traj.R")

samplings_table <- function(btraj.obj.list){
  SPOINT <- sapply(btraj.obj.list, function(x) x$Metadata$SamplingPoint)
  SAMPLING <- sapply(btraj.obj.list, function(x) x$Metadata$SamplingId)
  
  data.frame(spoint = SPOINT, sampling = SAMPLING)
}

df <- samplings_table(btraj.ANT)
t(unique(df))

