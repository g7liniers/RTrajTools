library(ggplot2)

source("src/loading/load_traj-ant.R")
source("src/loading/load_traj-arctic.R")
source("src/general-use-fcns/filter-traj.R")



extract_length <- function(btraj.obj) {
  btraj.obj$Metadata$SampleSize
}


#' plot of 4 density functions by location (antartica, arctic) and
#' reanalysis (ERA5,GDAS) of filtered trajectories

plot_trajectory_lengths_density <- function(sampling_points = NULL,
                                            sampling_ids = NULL,
                                            mix_values = NULL,
                                            bw=NULL) {
  btraj.ANT.ERA5 <-
    filter_traj(
      btraj.ANT,
      reanalysis = "ERA5",
      sampling_points = sampling_points,
      sampling_ids = sampling_ids,
      mix_values = mix_values
    )
  
  btraj.ANT.GDAS <-
    filter_traj(
      btraj.ANT,
      reanalysis = "GDAS",
      sampling_points = sampling_points,
      sampling_ids = sampling_ids,
      mix_values = mix_values
    )
  
  btraj.GL.ERA5 <-
    filter_traj(
      btraj.GL,
      reanalysis = "ERA5",
      sampling_points = sampling_points,
      sampling_ids = sampling_ids,
      mix_values = mix_values
    )
  
  btraj.GL.GDAS <-
    filter_traj(
      btraj.GL,
      reanalysis = "GDAS",
      sampling_points = sampling_points,
      sampling_ids = sampling_ids,
      mix_values = mix_values
    )
  
  lengths.ANT.ERA5 <- sapply(btraj.ANT.ERA5, extract_length)
  lengths.ANT.GDAS <- sapply(btraj.ANT.GDAS, extract_length)
  lengths.GL.ERA5 <- sapply(btraj.GL.ERA5, extract_length)
  lengths.GL.GDAS <- sapply(btraj.GL.GDAS, extract_length)
  
  if(is.null(bw)) bw="nrd0"
  
  dens.ANT.ERA5 <- density(lengths.ANT.ERA5,bw = bw)
  dens.ANT.GDAS <- density(lengths.ANT.GDAS,bw = bw)
  dens.GL.ERA5 <- density(lengths.GL.ERA5, bw = bw)
  dens.GL.GDAS <- density(lengths.GL.GDAS,bw= bw)
  
  # Find adequate x and y ranges for the plot
  x_range <-
    range(dens.ANT.ERA5$x,
          dens.ANT.GDAS$x,
          dens.GL.ERA5$x,
          dens.GL.GDAS$x)
  
  #TODO: fix scale for legend
  # x_range[2] <- x_range[2] + 30
  
  y_range <-
    range(dens.ANT.ERA5$y,
          dens.ANT.GDAS$y,
          dens.GL.ERA5$y,
          dens.GL.GDAS$y)
  
  # Plot the densities
  plot(
    rep(0, length(x_range)),
    type = "n",
    xlab = "X",
    ylab = "Density",
    main = "Density Functions",
    xlim = x_range,
    ylim = y_range
  )
  
  lines(dens.ANT.GDAS, lwd = 2, col = "pink") +
    lines(dens.ANT.ERA5, lwd = 2, col = "red") +
    lines(dens.GL.ERA5, lwd = 2, col = "blue") +
    lines(dens.GL.GDAS, lwd = 2, col = "cyan")
  
  # # Add legend
  # legend(
  #   "topright",
  #   legend = c("Vector 1", "Vector 2", "Vector 3", "Vector 4"),
  #   col = c("blue", "green", "red", "purple"),
  #   lwd = 2
  # )
  
  legend(x = "topleft", y = y_range[2],  # Adjust x and y coordinates
         legend = c("ANT GDAS", "ANT ERA5", "GL ERA5", "GL GDAS"),
         col = c("pink", "red", "blue", "cyan"), lwd = 2, cex = 0.5, xpd = TRUE)  # Adjust font size
}

plot_trajectory_lengths_density(mix_values = c(.1,.2,.3,.4),bw=2)
plot_trajectory_lengths_density(mix_values = c(.5,.6,.7,.8,.9,1),bw=2)



#' plot of 4 histograms by location (antartica, arctic) and
#' reanalysis (ERA5,GDAS) of filtered trajectories

plot_trajectory_lengths_hist <- function(localization,
                                         sampling_points = NULL,
                                            sampling_ids = NULL,
                                            mix_values = NULL,
                                            bw=NULL) {
  if (localization == "ANT") {
    btraj.ERA5 <-
      filter_traj(
        btraj.ANT,
        reanalysis = "ERA5",
        sampling_points = sampling_points,
        sampling_ids = sampling_ids,
        mix_values = mix_values
      )
    
    btraj.GDAS <-
      filter_traj(
        btraj.ANT,
        reanalysis = "GDAS",
        sampling_points = sampling_points,
        sampling_ids = sampling_ids,
        mix_values = mix_values
      )
    loc <- "Antartica"
  }
  
  if (localization == "GL") {
    btraj.ERA5 <-
      filter_traj(
        btraj.GL,
        reanalysis = "ERA5",
        sampling_points = sampling_points,
        sampling_ids = sampling_ids,
        mix_values = mix_values
      )
    
    btraj.GDAS <-
      filter_traj(
        btraj.GL,
        reanalysis = "GDAS",
        sampling_points = sampling_points,
        sampling_ids = sampling_ids,
        mix_values = mix_values
      )
    
    loc <- "Greenland"
  }
  
  lengths.ERA5 <- sapply(btraj.ERA5, extract_length)
  lengths.GDAS <- sapply(btraj.GDAS, extract_length)
  
  # Combine the vectors and create a data frame
  data <- data.frame(
    group = factor(rep(c("ERA5","GDAS"), times = c(length(lengths.ERA5), length(lengths.GDAS)))),
    value = c(lengths.ERA5, lengths.GDAS)
  )
  
  # Create the comparative histogram using ggplot2
  hplot <- ggplot(data, aes(x = value, color = group)) +
    geom_histogram(position = "identity" ,alpha = 0.05, binwidth = 5) +
    labs(x = "Value", y = "Frequency", title = paste("Number of discretization points of curves in",loc,", mix values: ",mix_values)) +
    scale_fill_brewer(palette="Dark2")+
    scale_color_brewer(palette = "Dark2") +
    theme_minimal() +  labs(color="Reanalysis") +
    theme(
      title = element_text(size = 9),  # Change legend title font size
    )

  
  hplot
  
}

plot_trajectory_lengths_hist("ANT",mix_values = c(.1,.2,.3))

get_proportion_long_curves <- function(threshold=80){
  
  proportion_long <- data.frame(
    matrix(nrow = 10,ncol = 5)
  )
  
  names(proportion_long) <- c("mix","ANT.ERA5","ANT.GDAS","GL.ERA5","GL.GDAS")
  proportion_long$mix <- 1:10/10
  
  for( i in 1:10){
    btraj.ANT.ERA5 <-
      filter_traj(
        btraj.ANT,
        reanalysis = "ERA5",
        mix_values = proportion_long$mix[i]
      )
    
    btraj.ANT.GDAS <-
      filter_traj(
        btraj.ANT,
        reanalysis = "GDAS",
        mix_values = proportion_long$mix[i]
      )
    
    btraj.GL.ERA5 <-
      filter_traj(
        btraj.GL,
        reanalysis = "ERA5",
        mix_values = proportion_long$mix[i]
      )
    
    btraj.GL.GDAS <-
      filter_traj(
        btraj.GL,
        reanalysis = "GDAS",
        mix_values = proportion_long$mix[i]
      )
    
    lengths.ANT.ERA5 <- sapply(btraj.ANT.ERA5, extract_length)
    lengths.ANT.GDAS <- sapply(btraj.ANT.GDAS, extract_length)
    lengths.GL.ERA5 <- sapply(btraj.GL.ERA5, extract_length)
    lengths.GL.GDAS <- sapply(btraj.GL.GDAS, extract_length)
    
    proportion_long$ANT.ERA5[i] <- length(which(lengths.ANT.ERA5 >= threshold))/length(lengths.ANT.ERA5)
    proportion_long$ANT.GDAS[i] <- length(which(lengths.ANT.GDAS >= threshold))/length(lengths.ANT.GDAS)
    proportion_long$GL.ERA5[i] <- length(which(lengths.GL.ERA5 >= threshold))/length(lengths.GL.ERA5)
    proportion_long$GL.GDAS[i] <- length(which(lengths.GL.GDAS >= threshold))/length(lengths.GL.GDAS)
  }
  
  proportion_long
  
}

proportions <- get_proportion_long_curves(100)
ggplot(proportions, aes(x=mix)) + 
  geom_point(aes(y = ANT.ERA5), size = 1, color = "red") +
  geom_point(aes(y = ANT.GDAS), size = 1, color = "pink") +
  geom_point(aes(y = GL.ERA5), size = 1, color = "blue") +
  geom_point(aes(y = GL.GDAS), size = 1, color = "cyan") +
  labs(title = "Scatter Plots of Y Values",
       x = "Mix Value",
       y = "proportion of long curves",
       color = "Legend Title") +
  scale_color_manual(values = c("ANT.ERA5" = "red",
                                "ANT.GDAS" = "pink",
                                "GL.ERA5" = "blue",
                                "GL.GDAS" = "cyan")) +
  theme_minimal() 


##
library(ggplot2)

# Create a sample data frame with different y values for demonstration
proportions <- get_proportion_long_curves(115)

# Create the ggplot object
plot <- ggplot(proportions, aes(x = mix)) +
  geom_point(aes(y = ANT.ERA5, color = "ANT.ERA5"), size = 2) +
  geom_point(aes(y = ANT.GDAS, color = "ANT.GDAS"), size = 2) +
  geom_point(aes(y = GL.ERA5, color = "GL.ERA5"), size = 2) +
  geom_point(aes(y = GL.GDAS, color = "GL.GDAS"), size = 2) +
  labs(title = "Scatter Plots of Y Values",
       x = "Mix Value",
       y = "Proportion of Long Curves",
       color = "Legend Title") +
  scale_color_manual(values = c("ANT.ERA5" = "red",
                                "ANT.GDAS" = "pink",
                                "GL.ERA5" = "blue",
                                "GL.GDAS" = "cyan")) +
  theme_bw()


# Display the plot
print(plot)

