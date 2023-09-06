source("src/preanalysis/extract-features.R")

wilcoxon_btraj <- function(btraj.obj.list, mode){
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
  
  wilcox.test(v.ERA5, v.GDAS, paired = TRUE)
}

wilcoxon_btraj_print_all <- function(btraj.obj.list){
  for(mode in c("length", "ndiscp", "xproj", "yproj","height")){
    cat(paste(mode, "\n"))
    print(wilcoxon_btraj(btraj.obj.list, mode))
  }
}

wilcoxon_btraj_all_pval <- function(btraj.obj.list){
  bol <- btraj.obj.list
  list(length = wilcoxon_btraj(bol, "length")$p.value,
       ndiscp = wilcoxon_btraj(bol, "ndiscp")$p.value,
       xproj = wilcoxon_btraj(bol, "xproj")$p.value,
       yproj = wilcoxon_btraj(bol, "yproj")$p.value,
       height = ks_btraj(bol, "height")$p.value)
}

