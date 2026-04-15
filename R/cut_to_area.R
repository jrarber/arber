#' Cutting Rasters to State and Regional Boundaries
#'
#' A more detailed description of what the function does.
#'
#' @param raster The uncropped raster to be cut.
#' @param extent The state or region to crop to. Regions: "South", "Northeast", "West", "Midwest"
#' @return A cropped raster
#' @examples
#' # Example usage of the function
#' cut(rawraster, "Arkansas", area = "state")
#' cut(rawraster, "SW", area = "region")
#' @export



## cut rasters to regions or state
cut_to_area <- function(raster, extent, area = "state"){
  require(sf)
  data("usa", package = "arber", envir = environment())
  data("usa_regions", package = "arber", envir = environment())
  data("State_latlongs", package = "arber", envir = environment())
  coords <- State_latlongs[State_latlongs$Area == extent,]
  e <- terra::ext(coords$Lon_min,
                  coords$Lon_max,
                  coords$Lat_min,
                  coords$Lat_max)
 if(area == "state") {
   shape <- usa[usa$NAME == extent,]
   shape <- shape["NAME"]
   shape <- vect(shape)
   shape <- project(shape, "EPSG:4326")
 } else if(area == "region"){
   shape <- usa_regions[usa_regions$NAME == extent,]
   shape <- shape["NAME"]
   shape <- vect(shape)
   shape <- project(shape, "EPSG:4326")
 }
  lrast <- terra::mask(raster, shape)
  lrast <- terra::crop(lrast, e)
  return(lrast)
  }


