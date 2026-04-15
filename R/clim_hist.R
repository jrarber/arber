






clim_hist <- function(var, res){
  web <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_"
  files <- paste0(web, res, "_", var, ".zip")
  for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
  download.file(files[i], dest, mode = "wb")}
}



 # https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_30s_tmin.zip
#https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_2.5m_tmax.zip



#https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_30s_bio.zip
