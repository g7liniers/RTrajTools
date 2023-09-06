library(pracma)

load(file="T2_BackByers2016_cut.Rdata")
r.length = length(coor.list)


# Metrica de HySpplit con distancia haversine

Dis.Hy.harv = matrix(0,nrow=r.length,ncol=r.length)
num_points_h = matrix(0,nrow=r.length,ncol=r.length)
harvM = vector(121,mode="list") 

for (k in 2:121) {
  for (i in 1:r.length) {
    for (j in 1:i) {
      p1=as.matrix(coor.list[[i]][k,])
      p2=as.matrix(coor.list[[j]][k,])
      if(is.na(p1[1,1]) == FALSE){
        #        print("pasa por aqui 1")
        if(is.na(p2[1,1]) == FALSE){
          #          print("pasa por aqui 2")
          Dis.Hy.harv[i,j]=Dis.Hy.harv[i,j]+haversine(p1,p2,R = 6371)
          Dis.Hy.harv[j,i]=Dis.Hy.harv[i,j]
          num_points_h[i,j]=num_points_h[i,j]+1
          num_points_h[j,i]=num_points_h[j,i]+1        
        }
      }
    }
  } 
}

Dis.Hy.harv=Dis.Hy.harv/num_points_h

save(Dis.Hy.harv,num_points_h,file="DistMatrix_Hy_harv.Rdata")

rm(list=ls())
