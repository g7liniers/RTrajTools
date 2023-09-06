library(xtable)
library(tidyr)

ANT.global <- readRDS("output/analyzed/global/ANT.rds")
GL.global <- readRDS("output/analyzed/global/GL.rds")

jointclust <- melt(ANT.global$JOINTCLUST)
sepclust <- melt(ANT.global$SEPCLUST)
loocv <- melt(ANT.global$LOOCV)

output_latex_tables <- function(object, group) {
  
}
############## jointclust tables - confirm cluster structure by spoint samplings
jointclust_table <- function(object, group) {
  jointclust <- melt(object$JOINTCLUST)
  names(jointclust) <-
    c("ARI", "Reanalysis", "Grouping", "Distance")
  filtered.df <- subset(jointclust, Grouping == group)
  filtered.df[which(filtered.df[, 2] == "Global"), 2] <- "Both"
  
  final.df <- filtered.df[, c(2, 4, 1)]
  XT <- xtable(
    final.df,
    caption = "ARI of the k means clustering and the partition by ",
    label = paste("global-jointclust", group, object$localization, sep = "-"),
    digits = 4
    )
  print(XT, include.rownames =F)
}

### SEPCLUST TABLE - similARIty of ERA5 and GDAS partitions

sepclust_table <- function(group){
  sepclust <- melt(list(GL = GL.global$SEPCLUST, ANT = ANT.global$SEPCLUST))
  names(sepclust) <- c("ARI", "Grouping", "Distance", "Localization")
  sepclust <- spread(sepclust, Distance, ARI)
  sepclust <- subset(sepclust, Grouping == group )
  sepclust$Grouping <- NULL
  sepclust
  
  XT <- xtable(
    sepclust,
    caption = "ARI values for the separate k clustering  of ERA5 and GDAS trajectories ",
    label = paste("global-sepclust",group, sep = "-"),
    digits = 4
  )
  print(XT, include.rownames =F)
}


# LOOCV global
loocv_table <- function(){
  df <- melt(list(GL = GL.global$LOOCV, ANT = ANT.global$LOOCV))
  names(df) <- c("LOOCV", "Distance", "Localization")
  reshaped <- spread(df, Distance, LOOCV)
  rownames(reshaped) <- reshaped$L2
  reshaped$L2 <- NULL
  
  
  XT <- xtable(
    reshaped,
    caption = "Leave-one-out Cross-validation Success Rate",
    label = "global-loocv",
    digits = 4
  )
  print(XT, include.rownames =F)
}





{
  jointclust_table(ANT.global, "BySampling")
jointclust_table(ANT.global, "BySpoint")
jointclust_table(GL.global, "BySampling")
jointclust_table(GL.global, "BySpoint")

sepclust_table("BySampling")
sepclust_table("BySpoint")


loocv_table()
}
