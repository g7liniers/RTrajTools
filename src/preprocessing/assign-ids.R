#' @description
#' Assigns unique ids to the objects of a trajectory object list
#' @param traj_obj_list trajectory list where ids are to be set/overwritten
#' @returns original trajectory list with unique ids
assign_unique_ids <- function(traj_obj_list){
  for (i in 1:length(traj_obj_list)){
    traj_obj_list[[i]]$Metadata$Id <- i
  }
  return(traj_obj_list)
}


#' @description
#' Find the trajectory corresponding to the other reanalysis method relative to
#' the one from the input function. IMPORTANT: if no corresponding trajectory is 
#' found, the function will run, but will generate a warning and return NA. Aslo, 
#' the Id parameter of metadata must be populated.
#' @param traj_obj_list trajectory list where corresponding trajectory will be searched
#' @param traj trajectory whose correspondance is to be found
#' @returns the corresponding trajectory id
find_correspondance_id <- function(traj_obj_list, traj_obj){
  filename <- traj_obj$Metadata$Filename
  id <- traj_obj$Metadata$Id
  correspondance <- c()
  for (traj in traj_obj_list) {
    if(traj$Metadata$Filename == filename && traj$Metadata$Id != id){
      correspondance <- c(correspondance, traj$Metadata$Id)
    }
    
  }
    if(is.null(correspondance)) {
      warning("No corresponding trajectory found")
      return(NA)
    }
    
    if (length(correspondance)>1) {
      warning("Broken data: too many corresponding trajectories")
      return(NA)
    }
    
    return(correspondance[1])
}

which_index <- function(traj_obj_list,id){
  for (i in 1:length(traj_obj_list)){
    if (traj_obj_list[[i]]$Metadata$Id == id){
      return(i)
    }
  }
      warning("The provided id does not match any of the trajectories ids")
      return(NA)
}

assign_pair_ids <- function(traj_obj_list){
  n.trajs <- length(traj_obj_list)
  
  cat("Calculating pair IDs.......................... \n")
  pb = txtProgressBar(min = 1, max = n.trajs, initial = 1,style = 3)
  id = 1
  for (i in 1:n.trajs){
    setTxtProgressBar(pb,i)
    if (is.null(traj_obj_list[[i]]$Metadata$PairId)) {
      traj_obj_list[[i]]$Metadata$PairId <- id
      pair_id <- find_correspondance_id(traj_obj_list, traj_obj_list[[i]])
      if (is.na(pair_id)) stop(paste("No correspondance found for trajectory with id =",traj_obj_list[[i]]$Metadata$Id ))
      pair_index <- which_index(traj_obj_list, pair_id)
      traj_obj_list[[pair_index]]$Metadata$PairId <-  id
      
      id <- id+1
    }
  }
  close(pb)
  
  return(traj_obj_list)
}

#' @description
#' Populate a new metadata property 'PCloudId' that serves as an identifier
#' for the sp2 main objects. In this standpoint, trajectories are grouped by
#' time of arrival and sampling point.
#' @param traj_obj_list trajectory list where trajectory groups will be searched for
#' @returns the input list with filled parameter $Metadata$PCloudId
assign_pcloud_ids <- function(traj.obj.list){
  n.traj <- length(traj.obj.list)
  
  current.id <- 1
  for (i in 1:n.traj){
    traj <- traj.obj.list[[i]]
    if(!is.null(traj$Metadata$PCloudId)) next
    
    toa <- traj$Metadata$EndDatetime
    spoint <- traj$Metadata$SamplingPoint
    
    for (j in i:n.traj) {
      if (traj.obj.list[[j]]$Metadata$EndDatetime == toa & traj.obj.list[[j]]$Metadata$SamplingPoint == spoint){
        traj.obj.list[[j]]$Metadata$PCloudId <- current.id
      }
    }
    
    current.id <- current.id+1
  }
  traj.obj.list
}

assign_all_ids <- function(btraj.obj.list){
  bol <- btraj.obj.list
  bol <- assign_unique_ids(bol)
  bol <- assign_pair_ids(bol)
  bol <- assign_pcloud_ids(bol)
  
  bol
}
