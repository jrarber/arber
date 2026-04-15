#' Load in shape files for US regions or states as spatVectors
#'
#' Easily make available shapefiles and spatVectors of states and regions in the United States. Any state, and any of five regions ("West", "South", "Northeast", "Midwest") are used to provide built-in shapes. 'columns = TRUE' also provides area of land and water in each state.
#'
#' @param region The area in the US to output a shapefile for. Can be a region ("West", "South", "Midwest", "Northeast") or a state (including DC).
#' @param as.vector boolean. If TRUE, will return a spatVector of the chosen area's shapefile. If FALSE, will just return the shapefile.
#' @param columns boolean. If FALSE, will remove all columns from the shapefile except for row names. FALSE by default.
#' @return A loaded shapefile or spatVector for the given area.
#' @examples
#' # Example usage of the function
#' plot(shape("Arkansas"))
#' LA <- shape("Louisiana", as.vector = TRUE)
#' South <- shape("South")
#' @export





shape <- function(region, as.vector = FALSE, columns = FALSE){
  require("sf")
  if(region %in% c("West","South","Midwest","Northeast")){
    data("usa_regions", package = "arber")
    south <- usa_regions[usa_regions$NAME == region,]
   if(columns == FALSE){south <- south["NAME"]}
  } else {
    data("usa", package = "arber")
    south <- usa[usa$NAME == region,]
    if(columns == FALSE){south <- south["NAME"]}
  }
  if(as.vector == TRUE){
    ar <- terra::vect(south)
    ark <- terra::project(ar, "EPSG:4326")
    return(ark)}
  if(as.vector == FALSE){return(south)}

}
