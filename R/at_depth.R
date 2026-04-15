#' Properties through soil depth layers
#'
#' Raster and summarize soil characteristics through 6 layers of soil depth.
#'
#' @param var The soil variable to download.
#' @param Depth Discrete soil depth for var. 0_5, 5_15, 15_30, 30_60, 60_100, 100_200
#' @param area Section of contiguous United States to download.(South, Northeast, Midwest, West)
#' @param summary Boolean. If TRUE returns an additional layer as an average of all soil depths.
#' @return Multiple layer raster of single soil property.
#' @examples
#' # Example usage of the function
#' polaris("clay", "0_5", "~/Downloads/, region = "SE")
#' polaris("om", "30_60", ~/Users/john/documents/", region = "NW")
#' @export


at_depth <- function(var, extent, area = "state",
                     summary = FALSE){
  data("State_latlongs", package = "arber")


 if(area == "state"){
   data("usa", package = "arber")
   coords <- State_latlongs[State_latlongs$Area == extent,]
   lats <- seq(from = floor(coords$Lat_min)+1, to = ceiling(coords$Lat_max))
   lats_min1 <- lats - 1
   longs <- seq(from = floor(abs(coords$Lon_min))+1, to = ceiling(abs(coords$Lon_max)))
   longs_mini1 <- longs - 1
   latitudes <- paste0(lats_min1, lats)
   latitudes <- rep(latitudes, each = length(longs))
   longitudes <- paste0(rep(longs, length(lats)), "-", rep(longs_mini1, length(lats)))
   filenames <- paste0("lat", latitudes, "_", "lon", "-", longitudes, ".tif")

  e <- terra::ext(coords$Lon_min,
                  coords$Lon_max,
                  coords$Lat_min,
                  coords$Lat_max)
  shape <- usa[usa$NAME == extent,]
  shape <- shape["NAME"]
  shape <- terra::vect(shape)
  shape <- terra::project(shape, "EPSG:4326")
 }
  else if(area == "region"){
  if(!(region %in% c("South", "Northeast","Midwest","West"))){
      return(message("Region must be 'South', 'Northeast', 'Midwest', or 'West'"))
    }
   if(extent == "South"){data("SEgridfiles", package = "arber")
     filenames <- SEgridfiles}
   if(extent == "Northeast"){data("NEgridfiles", package = "arber")
     filenames <- NEgridfiles}
   if(extent == "Midwest"){data("MWgridfiles", package = "arber")
     filenames <- MWgridfiles}
   if(extent == "West"){data("WEgridfiles", package = "arber")
     filenames <- WEgridfiles}

   data("usa_regions", package = "arber")
   e <- terra::ext(coords$Lon_min,
                   coords$Lon_max,
                   coords$Lat_min,
                   coords$Lat_max)
   shape <- usa_regions[usa_regions$NAME == extent,]
   shape <- shape["NAME"]
   shape <- vect(shape)
   shape <- project(shape, "EPSG:4326")
 }

  path <- paste0(getwd(),"/downloads/")

  dir.create(path)
  files = paste0(web, var, "/mean/", "0_5", "/", filenames)
  for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
  download.file(files[i], dest, mode = "wb")}
  rasters <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
  r_list <- lapply(rasters, terra::rast)
  gr_list <- terra::sprc(r_list)
  merast <- terra::merge(gr_list)
  lrast <- terra::mask(merast, shape)
  lvl_1 <- terra::crop(lrast, e)
  unlink(path, recursive = TRUE)


  dir.create(path)
  files = paste0(web, var, "/mean/", "5_15", "/", filenames)
  for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
  download.file(files[i], dest, mode = "wb")}
  rasters <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
  r_list <- lapply(rasters, terra::rast)
  gr_list <- terra::sprc(r_list)
  merast <- terra::merge(gr_list)
  lrast <- terra::mask(merast, shape)
  lvl_2 <- terra::crop(lrast, e)
  unlink(path, recursive = TRUE)

  dir.create(path)
  files = paste0(web, var, "/mean/", "15_30", "/", filenames)
  for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
  download.file(files[i], dest, mode = "wb")}
  rasters <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
  r_list <- lapply(rasters, terra::rast)
  gr_list <- terra::sprc(r_list)
  merast <- terra::merge(gr_list)
  lrast <- terra::mask(merast, shape)
  lvl_3 <- terra::crop(lrast, e)
  unlink(path, recursive = TRUE)

  dir.create(path)
  files = paste0(web, var, "/mean/", "30_60", "/", filenames)
  for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
  download.file(files[i], dest, mode = "wb")}
  rasters <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
  r_list <- lapply(rasters, terra::rast)
  gr_list <- terra::sprc(r_list)
  merast <- terra::merge(gr_list)
  lrast <- terra::mask(merast, shape)
  lvl_4 <- terra::crop(lrast, e)
  unlink(path, recursive = TRUE)

  dir.create(path)
  files = paste0(web, var, "/mean/", "60_100", "/", filenames)
  for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
  download.file(files[i], dest, mode = "wb")}
  rasters <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
  r_list <- lapply(rasters, terra::rast)
  gr_list <- terra::sprc(r_list)
  merast <- terra::merge(gr_list)
  lrast <- terra::mask(merast, shape)
  lvl_5 <- terra::crop(lrast, e)
  unlink(path, recursive = TRUE)

  dir.create(path)
  files = paste0(web, var, "/mean/", "100_200", "/", filenames)
  for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
  download.file(files[i], dest, mode = "wb")}
  rasters <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
  r_list <- lapply(rasters, terra::rast)
  gr_list <- terra::sprc(r_list)
  merast <- terra::merge(gr_list)
  lrast <- terra::mask(merast, shape)
  lvl_6 <- terra::crop(lrast, e)
  unlink(path, recursive = TRUE)

  rasterout <- terra::c(lvl_1, lvl_2, lvl_3, lvl_4, lvl_5, lvl_6)
  return(rasterout)
}
