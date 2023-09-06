library(lubridate)

muestreos <- read.csv2("data/antartida/Muestreos_Plateau_TdV19.csv",sep = ",")

names(muestreos) <- c("Muestreo","Latitud_grados","Longitud_grados","Punto-muestreo" , "Dia-inicio"  , "Hora-inicio_UTC+2" ,"Dia-fin" , "Hora-fin_UTC+2")
muestreos$Muestreo <- 18:(17+length(muestreos$Muestreo))
muestreos$Latitud_grados <- as.numeric(muestreos$Latitud_grados)
muestreos$Longitud_grados <- as.numeric(muestreos$Longitud_grados)

#### unify sampling points
length(unique(muestreos$`Punto-muestreo`))

#asignation: 2->11, 3->12, 4->13, 6->14, 7->15, 8->16, 9->17, 10->18, 11->19, 12->20
muestreos$`Punto-muestreo` <- c(11,12,13,14,15,15,15,15,15,15,15,15,16,17,17,17,18,19,20,20)


# convert a formatted date-time string in UTC+2 into a POSIXct datetime object
get_timestamp <- function(datetime){
  # in the IANA timezone database convention. UTC+2 corresponds to GMT-2
  dmy_hm(datetime,tz = 'Etc/GMT-2')
}

#create timestamp columns and duration of sampling
muestreos$`Timestamp-inicio` <- sapply(paste(muestreos$`Dia-inicio`,"-",muestreos$`Hora-inicio_UTC+2`),
                                      get_timestamp)
muestreos$`Timestamp-fin` <- sapply(paste(muestreos$`Dia-fin`,"-",muestreos$`Hora-fin_UTC+2`),
                                      get_timestamp)

muestreos$`Duracion_segundos` <-  muestreos$`Timestamp-fin` - muestreos$`Timestamp-inicio`
muestreos$Duracion_horas <- muestreos$Duracion_segundos/3600

muestreos.ant <- muestreos

#'@description
#' Returns the sampling identifier corresponding to the given timestamp, searching
#' for it as the one which is the closest.
#'@param timestamp.traj timestamp corresponding to the trajectory for which
#'the sampling id is seeked.
#'@returns an integer from 18 to 37 corresponding to the sampling identifier,
#'as described in the dataframe muestreos.ant
get.muestreo.ant <- function(timestamp.traj){
  diff =  as.numeric(timestamp.traj) - as.numeric(muestreos.ant$`Timestamp-inicio`) #+ 3600
  diff[which(diff<0)] <- Inf
  min.index = which(diff == min(diff))
  if(as.numeric(timestamp.traj) > as.numeric(muestreos.ant$`Timestamp-fin`[min.index]) ||
     as.numeric(timestamp.traj) < as.numeric(muestreos.ant$`Timestamp-inicio`[min.index])){
    return(-1)
  }
  return(muestreos.ant$`Muestreo`[min.index])
}


puntos.muestreo.ant <- unique(round(muestreos.ant[,2:4],digits = 3))

#'@description
#' Returns the sampling point corresponding to the given coordinates, allowing
#' a maximum error of 10^(-3) on each coordinate.
#'@param lat latitude
#'@param long longitude
#'@returns an integer from 1 to 10 corresponding to the sampling point,
#'as described in the list puntos.muestreo.artico
get.punto.muestreo.ant <- function(lat,long){
  for(i in 1:10){
    lat.p = puntos.muestreo.ant[i,1]
    long.p = puntos.muestreo.ant[i,2]
    approximate.equal.lat = all.equal(lat,lat.p,tolerance=0.001)
    approximate.equal.long = all.equal(long,long.p,tolerance = 0.001)
    if (isTRUE(approximate.equal.lat) && isTRUE(approximate.equal.long)){
      return(puntos.muestreo.ant[i,3])
    }
  }
  warning("No corresponding sampling point for the given coordinates")
  return(-1)
}


## clean variables for global env
rm(muestreos)
rm(get_timestamp)
