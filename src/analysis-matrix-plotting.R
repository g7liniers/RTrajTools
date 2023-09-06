source("src/analysis-load-objects.R")
source("src/general-use-fcns/get-group-ids.R")

library(ggplot2)
library(reshape2)
library(gridExtra)

OBJ.list <-
  list(ANT.by.samplings,
       ANT.by.spoints,
       GL.by.samplings,
       GL.by.spoints)
OBJ.properties <-
  c("group", "localization", "DEPTH", "JOINTCLUST", "SEPCLUST")

Distances <- c("Frechet", "Hellinger", "Rdimf Euclidean")

custom_plot_matrix <-
  function(matrix,
           ids,
           gradcols = c("red", "yellow"),
           title = "",
           showNumbers = NULL,
           group = NULL) {
    if(is.null(showNumbers)){
      showNumbers = max(dim(matrix)) <=12
    }
    
    names(matrix) <- ids
    rownames(matrix) <- ids
    
    
    
    data_df <- melt(matrix)
    if (group == "SAMPLING")
      axislabel <- "Sampling Id"
    if (group == "SPOINT")
      axislabel <- "Sampling Point Id"
    
    
    data_df$Var2 <- rep(ids, each = length(ids))
    
    
    # Create a ggplot2 object
    plot <-
      ggplot(data = data_df, aes(x = Var2, y = Var1, fill = value)) +
      geom_tile() +     scale_fill_gradient(low = gradcols[1], high = gradcols[2]) +
      labs(title = title, x = axislabel, y = axislabel) +
      theme_minimal() + 
      scale_x_continuous(breaks = seq(min(data_df$Var2), max(data_df$Var2), by = 1)) +
      scale_y_continuous(breaks = seq(min(data_df$Var1), max(data_df$Var1), by = 1))
    

    if (showNumbers)
      plot <-
      plot +  geom_text(aes(label = round(value, 2)), vjust = 1)
    
    
    plot
  }

generate_title <- function(localization, distance, method, group) {
  
}

choose_object <- function(localization, group) {
  OBJ <- NULL
  if (localization == "ANT") {
    if (group == "SAMPLING")
      OBJ <- ANT.by.samplings
    if (group == "SPOINT")
      OBJ <- ANT.by.spoints
  }
  if (localization == "GL") {
    if (group == "SAMPLING")
      OBJ <- GL.by.samplings
    if (group == "SPOINT")
      OBJ <- GL.by.spoints
  }
  if (is.null(OBJ))
    stop(
      "Invalid input: localization must be either 'ANT' or 'GL'. group must be either SAMPLING or SPOINT"
    )
  
  OBJ
}

threeplot <- function(method.object, localization,group,top_title) {
  MO <- method.object
  grid.arrange(
    custom_plot_matrix(MO$Frechet,ids = get_group_ids(localization,group), group = group, title = "Frechet", gradcols = c("#DE11D0","#11DE1E" )),
    custom_plot_matrix(MO$Hellinger,ids = get_group_ids(localization,group), group = group, title = "Hellinger", gradcols = c("#180CF3","#E7F30C")),
    custom_plot_matrix(MO$Rdimf,ids = get_group_ids(localization,group), group = group, title = "Rdimf Euclidean", gradcols = c("#E01F1F","#1FE0E0")),
    ncol = 3,
    top = top_title
  )
}

save_matrix_plots <- function() {
  for (OBJ in OBJ.list) {
    group = OBJ$grouping
    localization = OBJ$localization
    
    
    
    for (m in 3:5) {
      path <- paste0("output/analyzed/plots/", OBJ.properties[m], "/",group,"/")
      if(!dir.exists(path)) dir.create(path,recursive = T)
      
      MO = OBJ[[m]]
      filename = paste0(localization,".png")
      
      top.title <- paste(OBJ.properties[m], " - " , localization, " - BY ", group)
      
      png(paste0(path, filename), width = 1080, height = 360)
      threeplot(MO,localization, group, top.title)
      dev.off()
      print(paste0(path, filename, " saved"))
    }
  }
}




