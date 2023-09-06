source("src/analysis-matrix-plotting.R")
save_matrix_plots()
rm(list = ls())

source("src/analysis-loocv-plotting.R")
save_loocv_plot()
rm(list = ls())

source("src/preanalysis/length_and_discretization_correl.R")
save_poster_plots()
rm(list = ls())


