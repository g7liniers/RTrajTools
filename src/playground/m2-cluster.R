source("src/m2-cluster/kmedoids.R")
source("src/general-use-fcns/filter-traj.R")
source("src/general-use-fcns/find-corresp-traj-position.R")
source("src/m2-cluster/separate-clustering.R")
source("src/m2-cluster/joint-clustering.R")
library(aricode)

bol1 <-
  filter_traj(btraj.GL,
              sampling_ids = c(11, 12),
              reanalysis = "ERA5")
bol2 <-
  filter_traj(btraj.GL,
              sampling_ids = c(11,12),
              reanalysis = "GDAS")

bol <- c(bol1, bol2)

# M2B: GDAS + ERA5
for (i in 1:16) {
  for (j in (i + 1):17) {
    print(paste("Samplings:", i, ",", j))
    bol <- filter_traj(btraj.GL, sampling_ids = c(i,j))
    for (dist in c("Frechet", "Hellinger", "Rdimf")) {
      ari <-  separate_clustering_2samplings(i, j, dist)$ARI
      print(paste(dist, "ARI = ", ari))
    }
    cat("\n")
  }
}



# M2A: GDAS || ERA5
source("src/m2-cluster/separate-clustering.R")
M2A.comp <- separate_clustering("GL", "Frechet", bol)


for (i in 1:16) {
  for (j in (i + 1):17) {
    print(paste("Samplings:", i, ",", j))
    bol <- filter_traj(btraj.GL, sampling_ids = c(i,j))
    for (dist in c("Frechet", "Hellinger", "Rdimf")) {
      M2B <-  joint_clustering_2samplings(i,j, dist)
      ari1 <- M2B$ARI1
      ari2 <-  M2B$ARI2
      ari3 <-  M2B$ARI3
      
      print(paste("ARI1: ", sprintf("%.2f", ari1),"ARI2: ", sprintf("%.2f", ari2),"ARI3: ", sprintf("%.2f", ari3), "(",dist,")"))
    }
    cat("\n")
  }
}

for (i in 1:9) {
  for (j in (i + 1):10) {
    print(paste("Samplings:", i, ",", j))
    bol <- filter_traj(btraj.GL, sampling_ids = c(i,j))
    for (dist in c("Frechet", "Hellinger", "Rdimf")) {
      M2B <-  joint_clustering_2spoints(i,j, dist)
      ari1 <- M2B$ARI1
      ari2 <-  M2B$ARI2
      ari3 <-  M2B$ARI3
      
      print(paste("ARI1: ", sprintf("%.2f", ari1),"ARI2: ", sprintf("%.2f", ari2),"ARI3: ", sprintf("%.2f", ari3), "(",dist,")"))
    }
    cat("\n")
  }
}

for (dist in c("Frechet", "Hellinger", "Rdimf")) {
  ari <-  separate_clustering_2samplings(i, j, dist)
  print(paste(dist, "ARI = ", ari))
}
