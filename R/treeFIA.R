#' Load in tree survey data from USFS Forest Inventory and Analysis database
#'
#' Load in a dataframe of forest survey data in its entirety, or filtered by a specific criteria (currently only state or species).
#'
#' @param filter.by How the data should be filtered by ("none", "state", "region"). If left blank, will return the unfiltered FIA database.
#' @param value Only if 'filter.by' is chosen. The criteria for which to match the filter.
#' @param native.only boolean. FALSE by default. If TRUE, will only return native species.
#' @return Raster values with 30m resolution.
#' @family treeFIA
#' @examples
#' # Example usage of the function
#' treeIV <- treeFIA()
#' ArkansasTreeIV <- treeFIA(filter.by = "state", "Arkansas")
#' @export




treeFIA <- function(filter.by = "none",
                    value,
                    native.only = FALSE)
 {data("treeIVdata", package = "arber", envir = environment())
  if(native.only == TRUE){treeIVdata <- dplyr::filter(treeIVdata, Native_Nonnative == "Native")}
  if(filter.by == "none"){return(treeIVdata)}
  else if(filter.by == "species") {
    sppIV <- dplyr::filter(treeIVdata, Species %in% value)
    return(sppIV)
    }
  else{
    data("State_latlongs", package = "arber", envir = environment())
  if(filter.by == "state"){
   codes <- State_latlongs$CD[match(value, State_latlongs$Area)]
   stateIV <- dplyr::filter(treeIVdata, STATECD %in% codes)
   return(stateIV)}
  }
  if(filter.by == "region"){
    if(!(value %in% c("south","west","northeast","midwest"))){stop("Regions must be 'northeast', 'midwest', 'west', or 'south'")}
    if(value == "south"){codes <- c(1, 5, 10, 11, 12, 13, 21, 22, 24, 28, 37, 40, 45, 47,48, 51, 54)}
    if(value == "west"){codes <- c(2, 4, 6, 8, 15, 16, 30, 32, 35, 41, 49, 53, 56)}
    if(value == "northeast"){codes <- c(9, 23, 25, 33, 34, 36, 42, 44, 50)}
    if(value == "midwest"){codes <- c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55)}

    regionIV <- dplyr::filter(treeIVdata, STATECD %in% codes)
  return(regionIV)
  }
}
