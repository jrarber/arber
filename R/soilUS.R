#' Mapping soil properties across the contiguous United States
#'
#' Returning a raster of the variable mean at depth across the whole continental United States with POLARIS soil rasters at 30m resolution.
#'
#' @param var The soil variable to download.
#' @param Depth Discrete soil depth for var. 0_5, 5_15, 15_30, 30_60, 60_300.
#' @return Raster of variable at chosen depth across the continental US.
#' @examples
#' # Example usage of the function
#' US_clay_raster <- soilUS("clay", "0_5")
#' US_om_raster_30_60cm <- soilUS("om", "30_60")
#' @export


soilUS <- function(var, depth){
  data("web", package = "arber")
  data("SEgridfiles", package = "arber")
  data( "NEgridfiles", package = "arber")
  data( "WEgridfiles", package = "arber")
  path <- paste0(getwd(),"/tempdownloads/")

  dir.create(path)
  cat("\n\n\n\n\n\n\n\n\n Downloading files for the south...\n ....")
    files = paste0(web, var, "/mean/", depth, "/", SEgridfiles)
    for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
    download.file(files[i], dest, mode = "wb")}
    rasters <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
    r_list <- lapply(rasters, terra::rast)
    gr_list <- terra::sprc(r_list)
    cat("Merging files...\n")
    southeast <- terra::merge(gr_list)
    cat("Removing files for the south... \n")
    unlink(path, recursive = TRUE)

  dir.create(path)
  cat("Southeast Complete. \n\n\n\n\n\n\n\n\n Downloading files for the northeast... \n")
    files = paste0(web, var, "/mean/", depth, "/", NEgridfiles)
    for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
    download.file(files[i], dest, mode = "wb")}
    rasters <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
    r_list <- lapply(rasters, terra::rast)
    gr_list <- terra::sprc(r_list)
    cat("Merging files...\n")
    northeast <- terra::merge(gr_list)
    cat("Removing files for the northeast...\n")
    unlink(path, recursive = TRUE)

  dir.create(path)
  cat("Northeast Complete. \n\n\n\n\n\n\n\n\n Downloading files for the west...\n")
    files = paste0(web, var, "/mean/", depth, "/", WEgridfiles)
    for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
    download.file(files[i], dest, mode = "wb")}
    rasters <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
    r_list <- lapply(rasters, terra::rast)
    gr_list <- terra::sprc(r_list)
    cat("Merging files...\n")
    west <- terra::merge(gr_list)
    cat("Removing files for the west...\n")
    unlink(path, recursive = TRUE)

  dir.create(path)
  cat("West Complete. \n\n\n\n\n\n\n\n\n Downloading files for the midwest...\n")
    files = paste0(web, var, "/mean/", depth, "/", MWgridfiles)
    for(i in seq_along(files)) {dest <- file.path(path, basename(files[i]))
    download.file(files[i], dest, mode = "wb")}
    rasters <- list.files(path = path, pattern = "\\.tif$", full.names = TRUE)
    r_list <- lapply(rasters, terra::rast)
    gr_list <- terra::sprc(r_list)
    cat("Merging files...\n")
    midwest <- terra::merge(gr_list)
    cat("Removing files for the midwest...\n")
    unlink(path, recursive = TRUE)

    cat("Midwest Complete. \n\n\n\n\n\n Merging regional rasters...\n")
    regionals <- c(southeast, northeast, west, midwest)
    contUS <- terra::merge(regionals)
    cat("Merging finished. Raster for USA soil ", var, " at ", depth, "cm is complete." , sep = "")
    return(contUS)
}
