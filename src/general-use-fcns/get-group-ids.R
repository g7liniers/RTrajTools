get_group_ids <- function(localization, group){
  ids = NULL
  if(group == "SAMPLING"){
    if(localization == "GL") ids = 1:17
    if(localization == "ANT") ids = 18:37
  }
  
  if(group == "SPOINT"){
    if(localization == "GL") ids = 1:10
    if(localization == "ANT") ids = 11:20
  }
  if(is.null(ids)) stop(
    "Invalid input: localization must be either 'ANT' or 'GL'. group must be either SAMPLING or SPOINT"
  )
  ids
}
