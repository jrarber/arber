#' Downloading state-specific soil properties from POLARIS
#'
#' Returning a state-specific raster of soil characteristics from POLARIS web repisoitory.
#'
#' @param var The soil variable to download.
#' @param Depth Discrete soil depth for var. "0_5", "5_15", "15_30", "30_60", "60_100", "100_200".
#' @param path The folder path for files to be downloaded to.
#' @param state State data to be downloaded.
#' @param download boolean. Determines if the files need to be downloaded or just merged.
#' @return State-wide raster file of soil data.
#' @family soils
#' @examples
#' # Example usage of the function
#' AR_clay_0_5 <- polaris2("clay", "0_5", "~/Downloads/, state = "Arkansas")
#' ND_om_30_60 <- polaris2("om", "30_60", ~/Users/john/documents/", state = "North Dakota")
#' @export


polaris2 <- function(var, depth, path, state, download = TRUE) {
  data("State_latlongs", package = "arber", envir = environment())
  data("usa", package = "arber", envir = environment())
  tmpfile <- tempfile(fileext = ".tif")
  coords <- State_latlongs[State_latlongs$Area == state,]
  lats <- seq(from = floor(coords$Lat_min)+1, to = ceiling(coords$Lat_max))
  lats_min1 <- lats - 1
  longs <- seq(from = floor(abs(coords$Lon_min))+1, to = ceiling(abs(coords$Lon_max)))
  longs_mini1 <- longs - 1
  latitudes <- paste0(lats_min1, lats)
  latitudes <- rep(latitudes, each = length(longs))
  longitudes <- paste0(rep(longs, length(lats)), "-", rep(longs_mini1, length(lats)))
  filenames <- paste0("lat", latitudes, "_", "lon", "-", longitudes, ".tif")

  if(download == TRUE){
    data("web", package = "arber")
      files = paste0(web, var, "/mean/", depth, "/", filenames)
      for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
      download.file(files[i], dest, mode = "wb")}
  }
  rasters <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
  r_list <- lapply(rasters, terra::rast)
  gr_list <- terra::sprc(r_list)
  cat("\n Merging files... \n")
  merast <- terra::merge(
    gr_list,
    filename = tmpfile,
    overwrite = TRUE,
    gdal = "BIGTIFF=YES")
  e <- terra::ext(coords$Lon_min,
                  coords$Lon_max,
                  coords$Lat_min,
                  coords$Lat_max)
  shape <- usa[usa$NAME == state,]
  shape <- shape["NAME"]
  shape <- terra::vect(shape)
  shape <- terra::project(shape, "EPSG:4326")
  lrast <- terra::mask(merast, shape)
  lrast <- terra::crop(lrast, e)
  terra::plot(lrast)
  unlink(tmpfile)
  return(lrast)
}



