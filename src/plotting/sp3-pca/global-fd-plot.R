source("src/loading/fd-object/load_all-fd-objs.R")

plot(fd.ANT$X, ylab = "x", xlab = "time (h)", main = "ANT")
plot(fd.ANT$Y, ylab = "y", xlab = "time (h)", main = "ANT")

plot(fd.GL$X, ylab = "x", xlab = "time (h)", main = "GL")
plot(fd.GL$Y, ylab = "y", xlab = "time (h)", main = "GL")