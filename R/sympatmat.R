#' Sympatry Matrices for Tree Species
#'
#' Create and plot matrices of relative sympatry
#'
#' @param species.list  A vector of species to include in the matrix.
#' @param method Currently one of 'proportional' (calculated based on proportion of shared plots) or 'basal' (calculated by basal area in shared plots)
#' @param summary Boolean, False by default. If true will provide a matrix of half the length with summaries of the species-species sympatries out of 2.
#' @param data The data to be used, including columns 'Species', 'ID', 'BA'
#' @return A list of length 1 (or 2 with summary plot) and plots the correlation matrix with 'corrplot'. Matrix is read with by the amount of plots with species n (column) present out of total plots with species m (row).
#'
#' @export
#'
#'




sympatmat <- function(species.list,
                      method = c("proportion","basal"),
                      data = NULL){
   spp <- as.vector(species.list)
   n <- length(spp)

   mat <- matrix(nrow = n,
                ncol = n,
                dimnames = list(spp,spp))

   for(i in seq_along(spp)){
     idx <- spp[i]
     tspp <- spp[!spp %in% idx]

     if(method == "proprtion")for(j in seq_along(tspp)){
       jdx <- tspp[j]
       s = (length(unique(arber::sites_with(present = c(idx, jdx), exact = T, data = data)$site_data$ID))/
              length(unique(arber::sites_with(present = idx, data = data)$site_data$ID)))
       mat[{{idx}},{{jdx}}] <- s
     }

     if(method == "basal")for(j in seq_along(tspp)){
       jdx <- tspp[j]
       together <- as.data.frame(sites_with(present = c(idx, jdx), exact = T, data = data)$site_data)
       tog_just2 = together[together$Species %in% c(idx, jdx), ]
       ba1_together <- sum(together[together$Species == idx, "BA"])
       ba2_together <- sum(together[together$Species == jdx, "BA"])
       tog_just2$prop_BA <- tog_just2$BA/tog_just2$SUM.BA
       isites = sites_with(present = idx, data = data)$site_data
       jsites = sites_with(present = jdx, data = data)$site_data
       ba1 = sum(isites[isites$Species == idx, "BA"])
       ba2 = sum(isites[isites$Species == jdx, "BA"])
       s = (ba1_together + ba2_together)/(ba1+ba2)
       mat[{{idx}},{{jdx}}] <- s
     }

   }

   if(summary){
     mat2 <- matrix(nrow = length(rownames(mat)),
                    ncol = length(rownames(mat)),
                    dimnames = list(rownames(mat),rownames(mat)))
     for(i in seq_along(rownames(mat))){
       sp <- rownames(mat)[i]
       others <- rownames(mat)[!rownames(mat) %in% sp]

       for(n in others){
         mat2[sp, n] <- (mat[sp,n] + mat[n,sp] / 2)
       }
     }
   }


   for(n in rownames(mat)) mat[{{n}},{{n}}] <- 1
   corrplot::corrplot(mat, method = "square", is.corr = F)
   if(summary) mat = c(mat, mat2)
   return(mat)
}
