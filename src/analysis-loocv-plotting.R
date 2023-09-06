library(ggplot2)
source("src/general-use-fcns/get-group-ids.R")

OBJ.list <-
  list(ANT.by.samplings,
       ANT.by.spoints,
       GL.by.samplings,
       GL.by.spoints)

plot_loocv_list <- function(loocv, ids, title, xlab){
  df <- melt(loocv)
  id <- rep(ids, 3)
  df[,3] = id
  
  names(df) <- c("loocv", "distance", "id")
  
  y_min <- min(df$loocv)
  y_max <- max(df$loocv)
  
  ggplot(df, aes(x = factor(id), y = loocv, fill = distance)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = title,
      x = xlab,
      y = "LOO CV Success Rate"
    ) +
    theme_minimal() + coord_cartesian(ylim=c(y_min -.02,y_max + .02))
}

save_loocv_plot <- function(){
  for (OBJ in OBJ.list) {
    group = OBJ$grouping
    localization = OBJ$localization
    
    loocv <- OBJ$LOOCV
    
    ids <- get_group_ids(localization, group)
    
    title <- paste(localization, "-", "BY ", group, "-", "Leave-One-Out Cross-Validation Success Rate")
    
    if(group == "SAMPLING") xlab <- "Sampling Id"
    if(group == "SPOINT") xlab <- "Sampling Point Id"
    
    
 
    ##
    path <- paste0("output/analyzed/plots/LOOCV/",group,"/")
    if(!dir.exists(path)) dir.create(path,recursive = T)
    
    filename = paste0(localization,".png")
    
    png(paste0(path, filename), width = 360, height = 360)
    plot(plot_loocv_list(loocv, ids, title, xlab))
    dev.off()
    print(paste0(path, filename, " saved"))
  }
}

save_loocv_plot()
