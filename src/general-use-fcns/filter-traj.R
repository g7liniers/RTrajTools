#' @description
#' Given a list of btraj objects filter them according to metadata specifications
#' on the arguments
#' @param btraj_obj list of backwards trajectories to be filtered
#' @param reanalysis reanalysis method: ERA5 or GDAS. defaults to both
#' @param sampling_points vector of sampling points identifiers. defaults to all
#' @param sampling_ids vector of sampling identifiers. defaults to all
#' @param mix_values vector of proportions relative to the limit layer. defaults to all
#' @param min_sample_size minimum number of discretization points in the curve. defaults to 0
#' @param max_sample_size maximum number of discretization points in the curve. defaults to Inf
#' @returns A btraj object list with the filtered elements
filter_traj <-  function(btraj_obj,
                        reanalysis = NULL,
                        sampling_points = NULL,
                        sampling_ids = NULL,
                        mix_values = NULL,
                        min_sample_size = 0,
                        max_sample_size = Inf,
                        traj_ids = NULL,
                        traj_pair_ids = NULL,
                        traj_pcloud_ids = NULL){
  
  filtered.btraj <- btraj_obj
  
  # filter by sampling id
  if (!is.null(sampling_ids)&!anyNA(sampling_ids)){
    filtered.btraj <- Filter(function(x) x$Metadata$SamplingId %in% sampling_ids, filtered.btraj)
  }
  
  # filter by mix
  if (!is.null(mix_values)&!anyNA(mix_values)){
    filtered.btraj <- Filter(function(x) x$Metadata$Mix %in% mix_values, filtered.btraj)
  }
  
  # filter by sampling point
  if (!is.null(sampling_points)&!anyNA(sampling_points)){
    filtered.btraj <- Filter(function(x) x$Metadata$SamplingPoint %in% sampling_points, filtered.btraj)
  }
  
  # filter by reanalysis methods
  if (!is.null(reanalysis)&!anyNA(reanalysis)){
    filtered.btraj <- Filter(function(x) x$Metadata$Reanalysis %in% reanalysis, filtered.btraj)
  }
  
  
  #filter by min sample size
  if (min_sample_size > 0){
    filtered.btraj <- Filter(function(x) x$Metadata$SampleSize >= min_sample_size, filtered.btraj)
  }
  
  #filter by max sample size
  if (!is.infinite(max_sample_size)){
    filtered.btraj <- Filter(function(x) x$Metadata$SampleSize <= max_sample_size, filtered.btraj)
  }
  
  #filter by trajectory ids
  if(!is.null(traj_ids)&!anyNA(traj_ids)){
    filtered.btraj <- Filter(function(x) x$Metadata$Id %in% traj_ids, filtered.btraj)
  }
  
  #filter by pair trajectory ids
  if(!is.null(traj_pair_ids)&!anyNA(traj_pair_ids)){
    filtered.btraj <- Filter(function(x) x$Metadata$PairId %in% traj_pair_ids, filtered.btraj)
  }
  
  #filter by pointcloud trajectory ids
  if(!is.null(traj_pcloud_ids)&!anyNA(traj_pcloud_ids)){
    filtered.btraj <- Filter(function(x) x$Metadata$PCloudId %in% traj_pcloud_ids, filtered.btraj)
  }
  
  return(filtered.btraj)
}

