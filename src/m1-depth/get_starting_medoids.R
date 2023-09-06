source("src/general-use-fcns/filter-traj.R")

get_starting_medoids <- function(localization, distance, btraj.obj.list, grouping){
  
  if(localization == "GL"){
    samplings <- 1:17
    spoints <- 1:10
  }
  
  if(localization == "ANT"){
    samplings <- 18:37
    spoints <- 11:20
  }
  
  indices = c()
  
  if(grouping == "sampling"){
    for (i in 1:length(samplings)){
      bol = filter_traj(btraj.obj.list, sampling_ids  = samplings[i])
      index <- btraj_position_bol(get_deepest_obj(localization, distance, bol), btraj.obj.list)
      indices[i] <- index
    }
    return(indices)
  }
  
  if(grouping == "spoint"){
    for(i in 1:length(spoints)){
      bol = filter_traj(btraj.obj.list, sampling_points = spoints[i])
      index <- btraj_position_bol(get_deepest_obj(localization, distance, bol), btraj.obj.list)
      indices[i] <- index
    }
    return(indices)
    
  }
}
