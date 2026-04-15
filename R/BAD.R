#' Basal Area Discrepency (BAD) values for a given level.
#'
#' Calculate disproportionate basal areas using Simpson's Diversity. Ranges from 0 (basal area relatively even across level) to 1 (basal area highly disproportionate across level).
#'
#' @param x The data frame to be used for calculations. Must be at the species level.
#' @param level The hierarchical level to calculate BAD values. Often is site, but can vary based on question (genus, etc.).
#' @return A summary with BAD values for each site.
#' @examples
#' # Example usage of the function
#' BAD(plotIV, Site)
#' @export


BAD <- function(x, level){
  ## for basal area discrepency (BAD) at each plot (site)
  x$site.BA.proportion <- (x$BA /x$SUM.BA)
  if(!("BA" %in% colnames(x))){stop("Data must contain 'BA' and 'SUM.BA' columns.")}
  x$site.sq <- (x$site.BA.proportion)^2
  plotsumm <- x %>%
    dplyr::group_by({{level}}) %>%
    dplyr::summarize(BAD = sum(site.sq), .groups = "keep")
  return(plotsumm)
}

