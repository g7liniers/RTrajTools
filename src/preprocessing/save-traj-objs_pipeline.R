#' Full process of transforming hysplit trajectories in the form of
#' 1 file per traj into 2 RDS files each containing  a list of trajectory
#' objects with two subobjects: Metadata and Trajectory. Metadata corresponds
#' to information about the trajectory and Trajectory is a dataframe for which
#' each point correspond to a point of discretization.
#'

source("src/general-use-fcns/formatted-datetime.R")

save_pipeline <-
  function(doProcessRaw = FALSE,
           doRemoveUnsampled = FALSE,
           doPerformBlockA = TRUE,
           doPerformBlockB = TRUE,
           doProcessGlobalFds = FALSE,
           doPcaDimred = FALSE,
           doComputeFrechet = FALSE,
           doComputeHellinger = FALSE,
           doComputeRdimfDist = FALSE,
           doSaveFinal = TRUE) {
    #' Saves all trajectories in the corresponding directories as btraj objects,
    #' regardless of sample validity.4 Files are saved under
    #' data/<loc>/preprocessed/<loc>_<reanal>_btraj.rds
    if (doProcessRaw)
      source("src/preprocessing/save-GL-ANT.R")
    
    #' Removes objects corresponding to non sampled trajectories in the context
    #' of MicroAirPolar. 4 Files are saved under
    #' #' data/<loc>/preprocessed/<loc>_sampled_<reanal>_btraj.rds
    if (doRemoveUnsampled)
      source("src/preprocessing/remove-unsampled.R")
    
    
    if(doPerformBlockA){
      ########################### BLOCK A:
      #'  id assignment, distance and speed calculation, projection, extension
      #'  of trajectories
      
      # Load sampled trajectories and merge by location
      btraj.ERA5 <-
        readRDS("data/antartida/processed/sampled/antartida_sampled_ERA5_btraj.rds")
      btraj.GDAS <-
        readRDS("data/antartida/processed/sampled/antartida_sampled_GDAS_btraj.rds")
      btraj.ANT <- c(btraj.ERA5, btraj.GDAS)
      rm(btraj.ERA5, btraj.GDAS)
      
      btraj.ERA5 <-
        readRDS("data/groenlandia/processed/sampled/groenlandia_sampled_ERA5_btraj.rds")
      btraj.GDAS <-
        readRDS("data/groenlandia/processed/sampled/groenlandia_sampled_GDAS_btraj.rds")
      btraj.GL <- c(btraj.ERA5, btraj.GDAS)
      rm(btraj.ERA5, btraj.GDAS)
      
      # Assign ids
      source("src/preprocessing/assign-ids.R")
      btraj.ANT <- assign_all_ids(btraj.ANT)
      btraj.GL <- assign_all_ids(btraj.GL)
      
      
      # Compute step and total distances and step and mean speeds
      source("src/preprocessing/trajectory-lengths.R")
      btraj.ANT <- assign_lengths_speeds(btraj.ANT)
      btraj.GL <- assign_lengths_speeds(btraj.GL)
    } else{
      btraj.ANT <- readRDS("data/antartida/processed/final/antartida_final_btraj.rds")
      btraj.GL <- readRDS("data/groenlandia/processed/final/groenlandia_final_btraj.rds")
    }
    
    if(doPerformBlockB){
      # Apply azimuthal equidistant projection centered on mean point
      source("src/preprocessing/projection.R")
      btraj.ANT <- project_centered_equidist_azimutal(btraj.ANT)
      btraj.GL <- project_centered_equidist_azimutal(btraj.GL)
      
      # Extend trajectory lengths to the maximum of each group (121 in both cases)
      source("src/preprocessing/extend_trajectories.R")
      btraj.ANT <- extend_trajs(btraj.ANT)
      btraj.GL <- extend_trajs(btraj.GL)
      
      # Assign functional data objects under each trajobj$FdTrajectory
      # ¡¡ takes about 12 min on tpx1 !!
      # source("src/sp3-pca/trajs-to-functions.R")
      # btraj.ANT <- assign_fd_trajs(btraj.ANT)
      # btraj.GL <- assign_fd_trajs(btraj.GL)
        
      
    } else{
      btraj.ANT <- readRDS("data/antartida/processed/final/antartida_final_btraj.rds")
      btraj.GL <- readRDS("data/groenlandia/processed/final/groenlandia_final_btraj.rds")
    }
    
    #MESSY! refactor
    if(doProcessGlobalFds){
      #For nodesep=5 it takes around 2'40"
      source("src/sp3-pca/bol-to-fd.R")
      
      range.val <- c(1,121)
      nodes <- seq(1,121,5)
      n.nodes   = length(nodes) #Number of knots
      n.order   = 4 # order of basis functions: for cubic b-splines: order = 3 + 1
      n.basis   = length(nodes) + n.order - 2;
      bsbasis <- create.bspline.basis(rangeval = range.val,nbasis = n.basis, norder = n.order, breaks = nodes )
      
      fd.ANT <- bol_to_fd(btraj.ANT,basis = bsbasis)
      print("Antartica global fd created")
      
      path.ANT <- paste0("output/fdata/antartida/bspline_order-",n.order-1,"_nodes-",n.nodes)
      if(!dir.exists(path.ANT)){
        dir.create(path.ANT)
      }
      
      saveRDS(fd.ANT$X, file = paste0(path.ANT,"/x.rds"))
      saveRDS(fd.ANT$Y, file = paste0(path.ANT,"/y.rds"))
      
      
      fd.GL <- bol_to_fd(btraj.GL, basis = bsbasis)
      print("Greenland global fd created")
      
      path.GL <- paste0("output/fdata/groenlandia/bspline_order-",n.order-1,"_nodes-",n.nodes)
      if(!dir.exists(path.GL)){
        dir.create(path.GL)
      }

      saveRDS(fd.GL$X, file = paste0(path.GL,"/x.rds"))
      saveRDS(fd.GL$Y, file = paste0(path.GL,"/y.rds"))
    }
    
    if(doPcaDimred){
      source("src/sp3-pca/pca-computation.R")
      var.th <- .999
      n.harm <- 20
      
      
      rdimf.ANT.X <- pca_dimred(fd.ANT$X,var.th = var.th, n.harm = n.harm)
      rdimf.ANT.Y <- pca_dimred(fd.ANT$Y, var.th = var.th, n.harm = n.harm)
      rdimf.GL.X <- pca_dimred(fd.GL$X, var.th = var.th, n.harm = n.harm)
      rdimf.GL.Y <- pca_dimred(fd.GL$Y, var.th = var.th, n.harm = n.harm)
      
      saveRDS(rdimf.ANT.X, "output/rdimf/antartida/rdimf-ANT-X.rds")
      saveRDS(rdimf.ANT.Y, "output/rdimf/antartida/rdimf-ANT-Y.rds")
      saveRDS(rdimf.GL.X, "output/rdimf/groenlandia/rdimf-GL-X.rds")
      saveRDS(rdimf.GL.Y, "output/rdimf/groenlandia/rdimf-GL-Y.rds")
    }
    
    if(doComputeFrechet){
      source("src/sp1-trajectories/projected_frechet.R")
      
      d.frechet.ANT <- frechet_dist_matrix_sf(btraj.ANT)
      saveRDS(d.frechet.ANT, file = paste("output/distances/antartida/frechet/Frechet-ANT_",formatted_datetime(),".rds",sep = ""))
      
      d.frechet.GL <- frechet_dist_matrix_sf(btraj.GL)
      saveRDS(d.frechet.GL, file = paste("output/distances/groenlandia/frechet/Frechet-GL_",formatted_datetime(),".rds",sep = ""))
    }
    
    if (doComputeHellinger){
      source("src/sp2-pointcloud/hellinger-dist-computation.R")
      
      d.hell.GL <- hellinger_dist_matrix_reanal_pcid(btraj.GL,gridpoints = 100)
      saveRDS(d.hell.GL, file = paste("output/distances/groenlandia/hellinger/Hellinger-GL_",formatted_datetime(),".rds",sep = ""))
      
      d.hell.ANT <- hellinger_dist_matrix_reanal_pcid(btraj.ANT, gridpoints = 100)
      saveRDS(d.hell.ANT, file = paste("output/distances/antartida/hellinger/Hellinger-ANT_",formatted_datetime(),".rds",sep = "") )
      
      #Save hellinger dist group by id (single trajectories)
      d.hell.single.GL <- hellinger_dist_matrix_traj_id(btraj.GL, gridpoints = 200)
      saveRDS(d.hell.single.GL, file = paste("output/distances/groenlandia/hellinger/Trajwise-Hellinger-GL_", formatted_datetime(), ".rds", sep = ""))
      d.hell.single.ANT <- hellinger_dist_matrix_traj_id(btraj.ANT, gridpoints = 200)
      saveRDS(d.hell.single.ANT, file = paste("output/distances/antartida/hellinger/Trajwise-Hellinger-ANT_", formatted_datetime(), ".rds", sep = ""))
    
    }
    
    if(doComputeRdimfDist){
      source("src/sp3-pca/vweigh-dist.R")
      d.rdimf.ANT <- rdimf_dist_matrix(rdimf.ANT.X, rdimf.ANT.Y)
      d.rdimf.GL <- rdimf_dist_matrix(rdimf.GL.X, rdimf.GL.Y)
      
      saveRDS(d.rdimf.ANT, paste0("output/distances/antartida/rdimf/RdimF-Eucl-ANT_", formatted_datetime(),".rds"))
      saveRDS(d.rdimf.GL, paste0("output/distances/groenlandia/rdimf/RdimF-Eucl-GL_", formatted_datetime(),".rds"))

    }
    
    
    ###################### Remaining modifications
    #
    #
    #
    ######################
    
    if ((doPerformBlockA | doPerformBlockB) & doSaveFinal) {
      saveRDS(btraj.ANT,
              "data/antartida/processed/final/antartida_final_btraj.rds")
      cat("\n Antartica btraj data saved successfully\n")
      saveRDS(btraj.GL,
              "data/groenlandia//processed/final/groenlandia_final_btraj.rds")
      cat("\n Greenland btraj data saved successfully\n")
    }
    
    confirmation_message <- paste(
      "raw process: ", doProcessRaw, "\n",
      "remove unsampled: ", doRemoveUnsampled, "\n",
      "perform Block A:(merge by loc, id assignment, length computation) :", doPerformBlockA, "\n",
      "perform Block B(projection, trajectory extension): ", doPerformBlockB, "\n",
      "process global fdata: ", doProcessGlobalFds, "\n",
      "compute Frechet: ", doComputeFrechet, "\n",
      "compute Hellinger: ", doComputeHellinger, "\n",
      "save final parameter: ", doSaveFinal, "\n",
      "SAVED: ", (doPerformBlockA | doPerformBlockB) & doSaveFinal,"\n",
      sep = ""
    )
    
    cat(confirmation_message)
  }

