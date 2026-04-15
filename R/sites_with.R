#' Get site FIA plot information where given species are present
#'
#' Use USFS FIA style data to get information on sites where a species (or multiple species) are present.
#'
#' @param present The species (as "Genus_specific"), or multiple with c(), used to choose the sites.
#' @param absent The species (as "Genus_specific"), or multiple with c(), to exclude in site selection.
#' @param x  Optional. The data to be filtered from. If left blank, will filter from the entire FIA database.
#' @param exact boolean. FALSE by default. If TRUE, will only return sites with all of the listed species present.
#' @return Dataframe with species level information at plots where the chosen species are present.
#' @family treeFIA
#' @examples
#' # Example usage of the function
#' cypress_sites <- sites_with("Taxodium_distichum")
#' cypress_tupelo <- sites_with(c("Taxodium_distichum","Nyssa_aquatica"), mydata)
#' cypress_and_tupelo <- sites_with(c("Taxodium_distichum","Nyssa_aquatica"), mydata, exact = TRUE)
#' @export



sites_with <- function(present,
                       absent = NULL,
                       data   = NULL,
                       exact  = FALSE) {
# choose data
  if (is.null(data)) {
    data("treeIVdata", package = "arber", envir = environment())
    df <- treeIVdata
  } else {
    df <- data
  }


# filter
  options(dplyr.summarize.inform = F)
  if (!exact) {                     # “any of the present species”
    outdf <- df %>%
      dplyr::group_by(ID) %>%
      dplyr::filter(Species %in% present &
               (is.null(absent) | !Species %in% absent)) %>%
      dplyr::ungroup()

  } else {                         # “exactly all of the present species”
    outdf <- df %>%
      dplyr::group_by(ID) %>%
      dplyr::filter(all(present %in% Species) &
               (is.null(absent) | !any(absent %in% Species))) %>%
      dplyr::ungroup()
  }
  outid <- as.vector(unique(outdf$ID))
  output$sites_with <- outid
  output$sites_without <- df$ID[!df$ID %in% outid]
  output$site_data <- df[df$ID %in% outid,]

  return(output)
}
