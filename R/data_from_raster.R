#' Extract data from a raster and add it to your data frame
#'
#' Get values from a raster that match latitudes and longitudes in your data frame.
#'
#' @param x The data frame with latitudes and longitudes in columns "LAT" and "LON"
#' @param raster A loaded raster with data to be extracted.
#' @param variable The desired name of the column containing the contents extracted from the raster.
#' @return The inputted dataframe with an added column of name 'variable' with values from the raster.
#' @examples
#' # Example usage of the function
#' sites_withsand <- data_from_raster(sitedata, sandraster, "Sand")
#' @export




data_from_raster <- function(x, raster, variable) {
  if(!all(c("LAT","LON") %in% names(x))){
    stop("Longitudes and latitudes must be in data frame columns 'LON' and 'LAT'")}
  else{
    coords <- data.frame(x = x$LON, y = x$LAT)
    codata <- as.data.frame(terra::extract(raster, coords))
    codata$ClimID <- seq.int(nrow(codata))
    codata <- codata[-1]
    x$ClimID <- seq.int(nrow(x))
    names(codata)[names(codata) != "ClimID"] <- variable
    x <- merge(x, codata, by.x = "ClimID", by.y = "ClimID", all.x = TRUE)
    x$ClimID <- NULL
    return(x)
  }
}




