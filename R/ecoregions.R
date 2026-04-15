#' US EPA Ecoregion names and codes
#'
#' A helper function that provides the codes and names of EPA Ecoregions at a given level. Best used as reference for 'ecoshapes()'.
#'
#' @param level description
#' @return description
#' @examples
#' # example code
#'
#' @export


ecoregions <- function(level = 3){
  arber::econamesl3
    if(level == 3){
    out<- data.frame(
        row.names = econamesl3$NA_L3CODE,
        Level_3_Region <- econamesl3$US_L3NAME,
        States <- econamesl3$STATE_NAME,
        Level_2_Region <- econamesl3$NA_L2NAME,
        Level_1_Region <- econamesl3$NA_L1NAME)
    out <- out %>%
      group_by()
  }
  if(level == 2){
    out <- data.frame(
        Level_2_Code <- econamesl3$NA_L2CODE,
        Level_2_Region <- econamesl3$NA_L2NAME,
        States <- econamesl3$STATE_NAME,
        Level_1_Region <- econamesl3$NA_L1NAME)
  }
  if(level == 1){
    out <- data.frame(
        Level_1_Code <- econamesl3$NA_L1CODE,
        Level_1_Region <- econamesl3$NA_L1NAME,
        States <- econamesl3$STATE_NAME)
  }
  print(out)
  return(out)
}
