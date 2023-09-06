#### Cluster variables meteo medias de las TRAYECTORIAS generadas por hysplit

library(dtw)

### ARCHIVOS
# setwd("~/Dropbox/000- LMFDA-Cluster/Ana - Analisis con R")
load(file="T2_BackByers2016_cut.Rdata")
rm(V,meanV,sdV)
r.length = length(coor.list)

### DISTANCIA HARVESING ENTRE DOS PUNTOS DE LAS ESFERA
## Escritura de los puntos
## z1 = (lat,lon); z2 = (lat,lon)
## R = 6356.78; radio de la tierra en los polos


d.harvesing = function(z1,z2)
{
#  r = 6356.78
r = 6371
pi = 3.14159265358979323846
  p =  pi/180
  difLat = (z1[1] - z2[1])*p
  difLong = (z1[2] - z2[2])*p
  h = (1 - cos(difLat))/2 + cos(z1[1]*p)*cos(z2[1]*p)*(1 - cos(difLong))/2
  d = 2*r*asin(sqrt(h))
  return(d)
}

### HARVESING MATRIX DISTANCE (no cuadrada)

harvesing.matrix.fun = function(a,b) {
  a = as.matrix(a)
  b = as.matrix(b)
  n = nrow(a)
  m = nrow(b)
  M = matrix(0,nrow=n,ncol=m)
  for (s in 1:n) {
    for (l in 1:m) M[s,l] = d.harvesing(as.numeric(a[s,]),as.numeric(b[l,]))
  }
  return(M)
}

### MATRIZ DE DISTANCIAS DTW (con correccion Harvesine)

Dis.DTW.h = matrix(0,nrow=r.length,ncol=r.length)
# tiempo=timestamp()
for (i in 2:r.length) {
  for (j in 1:(i-1)) {
    harvesing.matrix = harvesing.matrix.fun(coor.list[[i]],
                                            coor.list[[j]])
    Dis.DTW.h[i,j] = dtw(harvesing.matrix,distance.only = TRUE)$distance
  }
  print(i)
#  tiempo=rbind(tiempo,timestamp())
  MD = Dis.DTW.h[c(1:i),]
  save(MD,file="Control_DistMatrix_DTW_h.Rdata")    
#  rm(MD)  
}

Dis.DTW.h = as.dist(Dis.DTW.h)
save(Dis.DTW.h,file="DistMatrix_DTW_h.Rdata")
