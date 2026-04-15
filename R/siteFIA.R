#' Summarize tree surveys into a site-level dataframe
#'
#' Summarizes plot characteristics with species and calculates new importance values (IV)
#'
#' @param x The data frame with sites and species to be summarized.
#' @param sites The column of the data frame that differentiates sites.
#' @return A site-level summary as a data frame.
#' @family treeFIA
#' @examples
#' # Example usage of the function
#' sites <- siteFIA(plotIV, ID)
#' @export




siteFIA <- function(x, sites){
  output <- x %>%
    dplyr::group_by({{sites}}) %>%
    dplyr::summarise(County = mean(COUNTYCD),
            Density = sum(n),
            BA = sum(BA),
            Diversity = n(),
            Latitude = mean(LAT),
            Longitude = mean(LON),
            Genus.Diversity = length(unique(Genus))
  )
  #  dplyr::mutate(BA_percent = round(BA / sum(BA, na.rm = TRUE)*100,3), #add BA% column
  #                Density_percent = round((Density / sum(Density, na.rm = TRUE))*100, 5), #add density % column
  #                Frequency_percent = round((Frequency / sum(Frequency, na.rm = TRUE))*100, 5), # add frequency % column
  #                IV = round((BA_percent+Frequency_percent+Density_percent)/3,3), # calculate new IV
  #                avg.BA = round(BA/Density, 3))

}
