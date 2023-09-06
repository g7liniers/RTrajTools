#### Cluster variables meteo medias de las TRAYECTORIAS generadas por hysplit

# setwd("/home/proyectos/micropolar/ana/lmfda")

library(SimilarityMeasures)

load(file="T2_BackByers2016_cut.Rdata")
rm(V,meanV,sdV)

r.length = length(coor.list)


# Metrica de Frechet
Dis.Frechet = matrix(0,nrow=r.length,ncol=r.length)
#tiempo=timestamp()

load("Control_DistMatrix_Frechet.Rdata")
tiempo=rbind(tiempo,timestamp())

Dis.Frechet[c(1:dim(MDF)[1]),]=MDF
Dis.Frechet[,c(1:dim(MDF)[1])]=t(MDF)

#for (i in 1:r.length) {
for (i in (dim(MDF)[1]+1):r.length) {
  for (j in i:r.length) {
    if(j!=i){
      Dis.Frechet[i,j] = Frechet(as.matrix(coor.list[[i]]),as.matrix(coor.list[[j]]),testLeash=-1)
      Dis.Frechet[j,i] = Dis.Frechet[i,j]
    }
  }
  tiempo=rbind(tiempo,timestamp())
  MDF = Dis.Frechet[c(1:i),]
  save(MDF,tiempo,file="Control_DistMatrix_Frechet.Rdata")    
  rm(MDF)
}

Dis.Frechet=as.dist(Dis.Frechet)
save(Dis.Frechet,file="DistMatrix_Frechet.Rdata")    





