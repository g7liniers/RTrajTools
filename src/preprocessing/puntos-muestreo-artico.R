library(lubridate)

muestreos_raw <- read.csv("data/groenlandia/raw/PuntosMuestreoArtico.csv")

muestreos_raw$Latitud..grados. <- as.numeric(gsub(",",".", muestreos_raw$Latitud..grados.))
muestreos_raw$Longitud..grados. <- as.numeric(gsub(",",".", muestreos_raw$Longitud..grados.))

muestreos <- unique(muestreos_raw)
muestreos <- muestreos[which(muestreos$Hora.inicio..UTC.3. != muestreos$Hora.fin..UTC.3.),]

names(muestreos) <- c("Muestreo","Latitud_grados","Longitud_grados"  , "Dia-inicio"  , "Hora-inicio_UTC-3" ,"Dia-fin" , "Hora-fin_UTC-3")
muestreos$'Punto-Muestreo' <- c(1,2,3,4,5,5,6,6,7,7,8,8,8,8,9,10,10)

puntos.muestreo.artico <- vector(mode = "list", length = 10)
names(puntos.muestreo.artico) <- 1:10

coord.puntos.muestreo.artico <- unique(muestreos[,2:3])

for (i in 1:10){
  puntos.muestreo.artico[[i]] <- coord.puntos.muestreo.artico[i,]
}

# internal use function for converting date and hour strings to a timestamp format
# date must be given in a dd mm yyyy format
timestamp.from.muestreo <- function(string.date.time){
  return(dmy_hm(string.date.time,tz = "Etc/GMT+3"))
}

muestreos.inicio.timestamp <- sapply(
                                paste(muestreos$`Dia-inicio`, "-",
                                      muestreos$`Hora-inicio_UTC-3`), 
                                timestamp.from.muestreo)

muestreos.fin.timestamp <- sapply(
                                paste(muestreos$`Dia-fin`, "-",
                                  muestreos$`Hora-fin_UTC-3`), 
                                timestamp.from.muestreo)

muestreos$`Timestamp-inicio` <- muestreos.inicio.timestamp
muestreos$`Timestamp-fin` <- muestreos.fin.timestamp
muestreos$`Duracion-segundos` <- muestreos.fin.timestamp - muestreos.inicio.timestamp
muestreos$`Duracion-horas` <- muestreos$`Duracion-segundos`/3600

muestreos.artico <- muestreos


#'@description
#' Returns the sampling identifier corresponding to the given timestamp, searching
#' for it as the one which is the closest.
#'@param timestamp.traj timestamp corresponding to the trajectory for which
#'the sampling id is seeked.
#'@returns an integer from 1 to 17 corresponding to the sampling identifier,
#'as described in the list muestreos.artico
get.muestreo.artico <- function(timestamp.traj){
  diff =  as.numeric(timestamp.traj) - as.numeric(muestreos.artico$`Timestamp-inicio`) #+ 3600
  diff[which(diff<0)] <- Inf
  min.index = which(diff == min(diff))
  if(as.numeric(timestamp.traj) > as.numeric(muestreos.artico$`Timestamp-fin`[min.index]) ||
     as.numeric(timestamp.traj) < as.numeric(muestreos.artico$`Timestamp-inicio`[min.index])){
    return(-1)
  }
  return(muestreos.artico$`Muestreo`[min.index])
}



#'@description
#' Returns the sampling point corresponding to the given coordinates, allowing
#' a maximum error of 10^(-3) on each coordinate.
#'@param lat latitude
#'@param long longitude
#'@returns an integer from 1 to 10 corresponding to the sampling point,
#'as described in the list puntos.muestreo.artico
get.punto.muestreo.artico <- function(lat,long){
  for(i in 1:10){
    lat.p = coord.puntos.muestreo.artico[i,1]
    long.p = coord.puntos.muestreo.artico[i,2]
    approximate.equal.lat = all.equal(lat,lat.p,tolerance=0.001)
    approximate.equal.long = all.equal(long,long.p,tolerance = 0.001)
    if (isTRUE(approximate.equal.lat) && isTRUE(approximate.equal.long)){
      return(i)
    }
  }
  stop("No corresponding sampling point for the given coordinates")
}


## clean variables for global env
rm(muestreos)
rm(muestreos_raw)
rm(timestamp.from.muestreo)
rm(muestreos.fin.timestamp,muestreos.inicio.timestamp)
rm(i)
