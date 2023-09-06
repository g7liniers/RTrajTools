library(fda)
source("src/loading/load_all-traj.R")
source("src/general-use-fcns/filter-traj.R")


traj <- btraj.ANT[[1]]$Trajectory
traj.length <- dim(traj)[1]

traj.x <- traj$xProj
traj.y <- traj$yProj
traj.z <- traj$Alt

fun.df <- data.frame(x = traj.x, y=traj.y, z = traj.z)

traj.step.dist <- traj$StepDist
traj.step.speed <- traj$StepSpeed

traj.fd.x <- Data2fd(argvals = 1:traj.length, y = traj.x)
traj.fd.y <- Data2fd(argvals = 1:traj.length, y = traj.y)
traj.fd.z <- Data2fd(argvals = 1:traj.length, y = traj.z)
traj.fd.step.dist <- Data2fd(argvals = 1:traj.length, y = traj.step.dist)
traj.fd.step.speed <- Data2fd(argvals = 1:traj.length, y = traj.step.speed)


# test how the selected basis fits the data

plot(1:121, traj.x)
lines(traj.fd.x, col = "blue")

plot(1:121, traj.y)
lines(traj.fd.y, col="red")

plot(1:121, traj.z)
lines(traj.fd.z, col="green")

plot(1:121, traj.step.dist)
lines(traj.fd.step.dist, col="orange")

# dual/ternary plot
par(mfrow = c(3, 1))
plot(traj.fd.x, col="blue")
plot(traj.fd.y, col="red")
plot(traj.fd.z, col="green")
par(1)

# Spaghetti plot? (adjust ylims)

########## fd object
Data2fd(argvals = 1:122, y = fun.df)




############## 
time.span <- 122
times_basis = seq(0,time_span,1)
knots    = c(seq(0,time_span,1)) #Location of knots
n_knots   = length(knots) #Number of knots
n_order   = 4 # order of basis functions: cubic bspline: order = 3 + 1
n_basis   = length(knots) + n_order - 2;
basis = create.bspline.basis(c(min(times_basis),max(times_basis)),n_basis,n_order,knots)
n_basis
plot(basis)

################
library(fda)
library(tidyverse)
library(plotly)

# Function to simulate data
fake_curves <- function(n_curves = 100, n_points = 80, max_time = 100){
  ID <- 1:n_curves
  x <- vector(mode = "list", length = n_curves)
  t <- vector(mode = "list", length = n_curves)
  
  for (i in 1:n_curves){
    t[i] <- list(sort(runif(n_points,0,max_time)))
    x[i] <- list(cumsum(rnorm(n_points)) / sqrt(n_points))
  }
  df <- tibble(ID,t,x)
  names(df) <- c("ID", "Time", "Curve")
  return(df)
}

set.seed(123)
n_curves <- 40
n_points <- 80
max_time <- 100
df <- fake_curves(n_curves = n_curves,n_points = n_points, max_time = max_time)
head(df)

df_1 <- df %>% select(!c(ID,Curve)) %>% unnest_longer(Time) 
df_2 <- df %>% select(!c(ID,Time)) %>% unnest_longer(Curve)
ID <- sort(rep(1:n_curves,n_points))
df_l <- cbind(ID,df_1,df_2)
p <- ggplot(df_l, aes(x = Time, y = Curve, group = ID, col = ID)) +
  geom_line()
p

knots    = c(seq(0,max_time,5)) #Location of knots
n_knots   = length(knots) #Number of knots
n_order   = 4 # order of basis functions: for cubic b-splines: order = 3 + 1
n_basis   = length(knots) + n_order - 2;
basis = create.bspline.basis(rangeval = c(0,max_time), n_basis)
plot(basis)

argvals <- matrix(df_l$Time, nrow = n_points, ncol = n_curves)
y_mat <- matrix(df_l$Curve, nrow = n_points, ncol = n_curves)

W.obj <- Data2fd(argvals = argvals, y = y_mat, basisobj = basis, lambda = 0.5)


############### Me toca

source("src/loading/load_all-traj.R")
source("src/sp3-pca/bol-to-fd.R")
source("src/general-use-fcns/filter-traj.R")
source("src/general-use-fcns/unpack-trajs-from-bol.R")


cbasis <- create.constant.basis(rangeval = c(1,121),)
ebasis <- create.exponential.basis(rangeval = c(1,121))
bsbasis <- create.bspline.basis(rangeval = c(1,121),breaks = seq(1,121,20))

fd <- bol_to_fd(btraj.GL, bsbasis)
plot(fd$X)

bol <- filter_traj(btraj.GL, reanalysis = "ERA5",traj_pcloud_ids = 2)

fd <- bol_to_fd(bol, bsbasis)
plot(fd$X)

X <- unpack_trajs_x(bol)

for (i in 1:dim(X)[2]){
  points(1:121,X[,i],  cex=.5)
}

bsbasis20 <- create.bspline.basis(rangeval = c(1,121),breaks = seq(1,121,20))
save_fit_plots(bsbasis20, "bspline-knotsep20")

bsbasis10 <-create.bspline.basis(rangeval = c(1,121),breaks = seq(1,121,10))
save_fit_plots(bsbasis10, "bspline-knotsep10")

bsbasis5 <- create.bspline.basis(rangeval = c(1,121), breaks = seq(1,121,5))
save_fit_plots(bsbasis5, "bspline-knotsep5")

print("splines done")

fbasis <- create.fourier.basis(rangeval = c(1,121),nbasis = 20)
test_basis(fbasis, "fourier")
print("fourier done")

polbasis <- create.polygonal.basis(rangeval = c(1,121),argvals = seq(1,121,5))
test_basis(polbasis, "polygonal-argvalssep20",sample.size = 25)
print("polygonal done")

pbasis <- create.power.basis(rangeval = c(1,121),nbasis = 20)
test_basis(pbasis,"power-20",sample.size = 25)
print("power done")

expbasis <- create.exponential.basis(rangeval = c(1,121),nbasis = 30,ratevec = c(-20:-1/10000,1:10 ))
test_basis(expbasis,basis.name = "exponential-tuned", sample.size = 25)
print("exponential done")


bsbasis.deg9.sep5 <- create.bspline.basis(rangeval = c(1,121), breaks = nodes, norder = 10 )
test_basis(bsbasis.deg9.sep5, basis.name = "spline-deg9-sep5", sample.size = 10)

process_time_test <- function(btraj.obj.list, nodesep = 5){
  range.val <- c(1,121)
  nodes <- seq(1,121,nodesep)
  bsbasis <- create.bspline.basis(rangeval = range.val, breaks = nodes )
  
  init.time <- Sys.time()
  FD <- bol_to_fd(btraj.obj.list, basis = bsbasis)
  timediff <- Sys.time() - init.time
  
  print(paste("FD conversion of ", length(btraj.obj.list), "trajectories took ", round(timediff), "seconds"))
  print(paste0("spline basis with nodesep=", nodesep))
}
