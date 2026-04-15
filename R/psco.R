#' Probability of Species Co-Occurring
#'
#' Calculates the probability of two species to be in a given plot. Value based
#'  on US FIA data, or a supplied data frame.
#'
#' @param sp1 The first species for co-occurrence.
#' @param sp2 The second species for co-occurrence.
#' @param data The data frame for the co-occurence proability to calculate from. If left blank, data will calculate from USFS FIA data.
#' @return Along with the probability, a list is supplied containing information for both species.
#' @examples
#' # Example usage of the function
#' tadi.nyac <- pcso("Taxodium_distichum", "Nyssa_aquatica")
#' @export





psco <- function(sp1, sp2, data = NULL){
  spp <- c(sp1, sp2)
  both <- arber::sites_with(c(sp1, sp2), exact = T, x = data)
  both <- both %>%
    dplyr::group_by(ID, Species) %>%
    dplyr::filter(Species %in% spp)  %>%
    dplyr::ungroup(Species) %>%
    dplyr::summarise(IVboth = sum(IV))
  N.both <- length(both$IVboth)
  IV.both <- sum(both$IVboth)

  sp1df <- arber::sites_with(sp1, x = data)
  sp1df <- sp1df %>%
    dplyr::group_by(ID) %>%
    dplyr::filter(Species == sp1)
  sp1df$IV[is.na(sp1df$IV)] <- 0
  N.sp1 <- length(sp1df$IV)
  IV.sp1 <- sum(sp1df$IV)

  sp2df <- arber::sites_with(sp2, x = data)
  sp2df <- sp2df %>%
    dplyr::group_by(ID) %>%
    dplyr::filter(Species == sp2)
  sp2df$IV[is.na(sp2df$IV)] <- 0
  N.sp2 <- length(sp2df$IV)
  IV.sp2 <- sum(sp2df$IV)

  dataframes <- list(both, sp1df, sp2df)
  names(dataframes) <- c("both", "sp1", "sp2")

  prob <- ((IV.both)/(IV.sp1 + IV.sp2)) + (N.both/(N.sp1 + N.sp2))
  output <- list(dataframes, N.both, IV.both, N.sp1, IV.sp1, N.sp2, IV.sp2, prob)
  names(output) <-c("data",  "n_both", "IV_both", "N_sp1", "IV_sp1", "N_sp2","IV_sp2", "prob")
  message(paste0(sp1,"/",sp2," occurence probability = ", prob))
  return(output)
}
