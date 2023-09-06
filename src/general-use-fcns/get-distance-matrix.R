source("src/loading/load_all-traj.R")
source("src/general-use-fcns/get-ids-from-obj-list.R")
source("src/loading/load_all-dist.R")

get_distance_matrix <- function(localization, distance, btraj.obj.list){
  M <- NA
  if(length(btraj.obj.list) == 0) stop("Empty btraj obj list")
  
  if(localization == "ANT"){
    if(distance == "Frechet") M <- frechet.distM.ANT
    if(distance == "Hellinger") M <- hellinger.distM.ANT
    if(distance == "Rdimf") M <- rdimf.distM.ANT
  }
  if(localization == "GL"){
    if(distance == "Frechet") M <- frechet.distM.GL
    if(distance == "Hellinger") M <- hellinger.distM.GL
    if(distance == "Rdimf") M <- rdimf.distM.GL
  }

  if(is.null(dim(M))){
    stop("Localization must be either 'ANT' or 'GL'. Distance must be 'Frechet', 'Hellinger' or 'Rdimf'.")
  }
  ids <- get_ids_from_obj_list(btraj.obj.list)
  # print(paste("ids obj type/class", typeof(ids),"/", class(ids) ))
  # print(paste("dim M:", dim(M), "; max id: ", max(ids)))
  M[ids,ids]
}

clean_distances <- function() {
  rm(
    frechet.distM.ANT,
    frechet.distM.GL,
    hellinger.distM.ANT,
    hellinger.distM.GL,
    rdimf.distM.ANT,
    rdimf.distM.GL ,
    envir = .GlobalEnv
  )
  message("distances unattached from global environment")
}

