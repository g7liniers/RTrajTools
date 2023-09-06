source("src/general-use-fcns/filter-traj.R")

#' @description
#' This function produces a grouping (partition) of a btraj object list
#' attending to given parameters. The output is given in the form of a
#' list of btraj.obj group objects. A group object(GO) is a list with properties
#' GO$Grouping (object containing the definitory properties of the group) and
#' GO$BtrajObjList, a btraj obj list
#'

group_btraj.list <- function(btraj.obj.list,
                             reanalysis = NA,
                             sampling_points = NA,
                             sampling_ids = NA,
                             mix_values = NA,
                             traj_ids = NA,
                             traj_pair_ids = NA,
                             traj_pcloud_ids = NA) {
  grouping.grid <-
    expand.grid(
      reanalysis,
      sampling_points,
      sampling_ids,
      mix_values,
      traj_ids,
      traj_pair_ids,
      traj_pcloud_ids
    )
  names(grouping.grid) <-
    c("Reanalysis",
      "SamplingPoint",
      "SamplingId",
      "Mix",
      "Id",
      "PairId",
      "PCloudId")
  
  
  GO <- list()
  GO$BOLL <- list()
  n.groups <- dim(grouping.grid)[1]
  
  for (i in 1:n.groups) {
    GO$BOLL[[i]] <-
      filter_traj(
        btraj.obj.list,
        reanalysis = grouping.grid$Reanalysis[i],
        sampling_points = grouping.grid$SamplingPoint[i],
        sampling_ids = grouping.grid$SamplingId[i],
        mix_values = grouping.grid$Mix[i],
        traj_ids = grouping.grid$Id[i],
        traj_pair_ids = grouping.grid$PairId[i],
        traj_pcloud_ids = grouping.grid$PCloudId[i]
      )
  }
  
  
  # Populate the output object metadata
  GO$Grouping <- list()
  GO$Grouping$Reanalysis <- reanalysis
  GO$Grouping$SamplingPoints <- sampling_points
  GO$Grouping$SamplingIds <- sampling_ids
  GO$Grouping$SMixValues <- mix_values
  GO$Grouping$PairIds <- traj_pair_ids
  GO$Grouping$PCloudId <- traj_pcloud_ids
  
  GO
}
