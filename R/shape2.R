#' Regional shapefiles and spatVectors of the US with interior state borders
#'
#' Quickly retrieve shapefiles of United States regions with interior borders of states with an option to overlay level three EPA ecoregions.
#'
#' @param region The area in the US to output a shapefile for. Can be a region ("West", "South", "Midwest", "Northeast")
#' @param style Either "states" or "ecoregion" to decide the interior borders. "ecoregions" still returns state boundaries."states" by default.
#' @param as.vector boolean. If TRUE, will return a spatVector of the chosen area's shapefile. If FALSE, will just return the shapefile.
#' @param columns boolean. TRUE by default, removes all columns from the shapefile except for row names.
#' @return A loaded shapefile or spatVector for the given area.
#' @examples
#' South_Ecoregion_shapes <- shape2("South", style = "ecoregion")
#' West_spatvector <- shape2("West", as.vector = TRUE)
#' @export



shape2 <- function(region, style = "states", as.vector = FALSE, columns = FALSE){
  if(region == "South"){codes <- c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi", "North Carolina", "Oklahoma", "South Carolina", "Texas","Tennessee", "Virginia", "West Virginia")}
  if(region == "West"){codes <- c("California", "Colorado", "Oregon", "Washington", "Wyoming", "Montana", "Nevada", "Utah", "New Mexico", "Arizona", "Idaho")}
  if(region == "Northeast"){codes <- c("Pennsylvania", "New Jersey", "New York", "Connecticut", "Vermont", "Massachusetts", "Maine", "New Hampshire", "Rhode Island")}
  if(region == "Midwest"){codes <- c("Indiana", "Illinois", "Idaho", "Kansas", "Michigan", "Missouri","Nebraska", "North Dakota", "Nebraska", "Kansas", "Ohio", "South Dakota", "Wisconsin")}
  require("sf")

  if(style == "states"){
    data("usa", package = "arber", envir = environment())
    ar <- usa %>%
      dplyr::filter(NAME %in% codes)
    if(columns == FALSE){ar <- ar["NAME"]}
  } else if(style == "ecoregion"){
    data("ecol3", package = "arber", envir = environment())
    ar <- ecol3 %>%
      dplyr::filter(STATE_NAME %in% codes)
    if(columns == FALSE){ar <- ar["US_L3NAME"]}
  }
  if(as.vector == TRUE){
    ar <- terra::vect(ar)
    ark <- terra::project(ar, "EPSG:4326")
    return(ark)}
  if(as.vector == FALSE){return(ar)}
}
