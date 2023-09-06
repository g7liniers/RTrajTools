source("src/loading/load_all-traj.R")
source("src/preanalysis/extract-features.R")

ks_btraj <- function(btraj.obj.list, mode){
  
  fcn <- NULL
  if(mode == "length")  fcn <- get_length
  if(mode == "ndiscp") fcn <- get_ndiscp
  if(mode == "xproj") fcn <- get_median_xproj
  if(mode == "yproj") fcn <- get_median_yproj
  if(mode == "height") fcn <- get_median_height
  
  
  if(is.null(fcn)) stop("Provide a valid mode")
  
  paired.vectors <- paired_apply(btraj.obj.list, fcn)
  v.ERA5 <- paired.vectors$vERA5
  v.GDAS <- paired.vectors$vGDAS
  
  ks.test(v.ERA5, v.GDAS)
}

ks_btraj_print_all <- function(btraj.obj.list){
  for(mode in c("length", "ndiscp", "xproj", "yproj","height")){
    cat(paste(mode, "\n"))
    print(ks_btraj(btraj.obj.list, mode))
  }
}

ks_btraj_all_pval <- function(btraj.obj.list){
  bol <- btraj.obj.list
  list(length = ks_btraj(bol, "length")$p.value,
       ndiscp = ks_btraj(bol, "ndiscp")$p.value,
       xproj = ks_btraj(bol, "xproj")$p.value,
       yproj = ks_btraj(bol, "yproj")$p.value,
       height = ks_btraj(bol, "height")$p.value)
}

