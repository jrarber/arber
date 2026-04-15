#' Downloading POLARIS soil data for a given US region
#'
#' Returning a raster of soil characteristics of four regions of the United States (NE, SE, WE, MW).
#'
#' @param var The soil variable to download.
#' @param Depth Discrete soil depth for var. "0_5", "5_15", "15_30", "30_60", "60_100", "100_200".
#' @param path The folder path for files to be downloaded to.
#' @param region Section of contiguous United States to download.("South", "Northeast", "Midwest", "West")
#' @param crop boolean. If TRUE raster is returned cropped to the US Census Bureau region.
#' @param clear.files Boolean. If TRUE, downloaded files are deleted from path after merged into regional raster.
#' @param download Boolean. FALSE only if error occurred after downloading and need to remerge.
#' @return Raster values with 30m resolution.
#' @family soils
#' @examples
#' # Example usage of the function
#' southeast_clay_0_5 <- polaris("clay", "0_5", "~/Downloads/, region = "SE")
#' northwest_om_30_60 <- polaris("om", "30_60", ~/Users/john/documents/", region = "NW")
#' @export


polaris <- function(var, depth, path, region, crop = FALSE,
                    clear.files = FALSE, download = TRUE) {
  if(!(region %in% c("South", "Northeast","Midwest","West"))){
    return(message("Region must be 'South', 'Northeast', 'Midwest', or 'West'"))
  }
  if(download == TRUE){
  data("web", package = "arber")
  data("SEgridfiles", package = "arber", envir = environment())
  data( "NEgridfiles", package = "arber", envir = environment())
  data( "WEgridfiles", package = "arber", envir = environment())
  if(region == "South") {
    files = paste0(web, var, "/mean/", depth, "/", SEgridfiles)
    for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
    download.file(files[i], dest, mode = "wb")}
  }
  if(region == "Northeast") {
    files = paste0(web, var, "/mean/", depth, "/", NEgridfiles)
    for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
    download.file(files[i], dest, mode = "wb")}
  }
  if(region == "West") {
    files = paste0(web, var, "/mean/", depth, "/", WEgridfiles)
    for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
    download.file(files[i], dest, mode = "wb")}
  }
 }
  rasters <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
  r_list <- lapply(rasters, terra::rast)
  gr_list <- terra::sprc(r_list)
  cat("Merging files...\n")
  merast <- terra::merge(gr_list)
  if(crop == TRUE){
    cat("Cropping raster to regional boundaries...\n")
    data("usa_regions", package = "arber")
    e <- terra::ext(coords$Lon_min,
                    coords$Lon_max,
                    coords$Lat_min,
                    coords$Lat_max)
    shape <- usa_regions[usa_regions$NAME == region,]
    shape <- shape["NAME"]
    shape <- terra::vect(shape)
    shape <- terra::project(shape, "EPSG:4326")
    lrast <- terra::mask(raster, shape)
    lrast <- terra::crop(lrast, e)
    return(lrast)
  } else {return(merast)}
  if(clear.files == TRUE) {
    cat("Clearing...\n")
    unlink(path)
  }
}
