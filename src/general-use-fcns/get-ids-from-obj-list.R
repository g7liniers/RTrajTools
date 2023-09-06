get_ids_from_obj_list <- function(obj.list){
  ids <- sapply(obj.list, function(element) element$Metadata$Id)
  ids
}
