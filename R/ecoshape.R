#'
#'
#'
#'
#'
#' @param region description
#' @param level
#' @param .by description
#' @param as.vector boolean. If TRUE, will return a spatVector of the chosen area's shapefile. If FALSE, will return the shapefile.
#' @param columns boolean. TRUE by default, removes all columns from the shapefile except for row names.
#'
#' @return description


ecoshape <- function(region,
                     level = 3,
                     .by = "entire",
                     as.vector = FALSE,
                     columns = FALSE){

      if(.by == "entire"){

      }
      if(.by == "state"){
        arber::ecol3
        shapes <- ecol3 %>%
          dplyr::filter(STATE_NAME %in% region)
      }
    if(columns == FALSE){shapes <- shapes["US_L3NAME"]}
    if(as.vector == TRUE){
      shapes <- terra::vect(shapes)
      shapes <- terra::project(shapes, "EPSG:4326")
      return(shapes)}
    if(as.vector == FALSE){return(shapes)}
}
