source("src/general-use-fcns/get-ids-from-obj-list.R")
reduce_dist_matrix <- function(obj.list,dist.matrix){
  ids <- get_ids_from_obj_list(obj.list)
  
  if(max(ids) > dim(dist.matrix)[1]) stop("Distance matrix does not correspond to the data")
  
  return(dist.matrix[ids,ids])
}