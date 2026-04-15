#' Co-Occurring Species
#'
#' Calculates what species occur with a given species, and how often.
#'  Also creates a matrix of how often the co-occurring species occur with eachother.
#'
#'
#'
#'





# species co-occurrences ####
cocurs <- function(species, data = NULL, diag = FALSE, colors = NULL,
                   silent = FALSE, method = "square"){
  t <- arber::sites_with(species, x = data)
  tsite <- unique(t$ID)
  if(length(tsite) < 1){stop("There are no sites with this species in the data.")}
  tspp <- unique(t$Species)
  if(!("Species" %in% names(t))){stop("Column with species names
                                      must be labeled `Species`")}
  rspp <- tspp[!tspp %in% species]


## code given # Initialize an empty data frame to store results
results_df <- data.frame(i = character(), j = character(), prob = numeric())

results_list <- vector("list", length(tspp) * length(tspp) / 2)
k <- 1

cat("Calculating species co-occurrence probabilities...\n")

m <- choose(length(tspp), 2) + length(tspp)
if(silent) pb <- txtProgressBar(min = 0, max = m, initial = 0, style = 3) #


# Loop through each species `i`
for (i_idx in seq_along(tspp)) {
  i <- tspp[i_idx]


  for (j_idx in seq_len(i_idx)) {
    j <- tspp[j_idx]

    # evaluate psco properly
    if(silent){e <- suppressMessages(psco(i, j))
    } else {e <- psco(i, j)}

    results_list[[k]] <- data.frame(
      i = i,
      j = j,
      prob = e$prob
    )
    if(silent)setTxtProgressBar(pb, k)
    k <- k + 1
  }

}
if(silent == T){close(pb)}
cat("\n")
results_df <- do.call(rbind, results_list)


## convert to matrix in fill in values #####
n <- length(tspp)

mat <- matrix(
  NA_real_,
  nrow = n,
  ncol = n,
  dimnames = list(tspp, tspp)
)

for (r in seq_len(nrow(results_df))) {
  i <- results_df$i[r]
  j <- results_df$j[r]
  p <- results_df$prob[r]

  mat[i, j] <- p
  mat[j, i] <- p   # <- mirror
}

#redf_wide <- results_df %>%
#    tidyr::pivot_wider(names_from = i,
#                values_from = prob) %>%
#    tibble::column_to_rownames(var = "j") %>%
#    dplyr::select(!!!tspp)




#ordf_wide <- redf_wide[tspp,]

## ordering based on species that are most likely to occur with 'species'
sp.order <- data.frame(spp = row.names(mat),
                       pb = mat[, species])
sp.order <- sp.order[sp.order$spp != species, ]
sp.ordered <- sp.order[order(-sp.order$pb),]
spp.orders <- sp.ordered[, "spp"]
sorder <- c(species, spp.orders)

orderedframe <- mat[sorder,sorder]




ordmat <- as.matrix(orderedframe)


cuscol <- colorRampPalette(c("#fec44f", "#fe9929",
                           "#ec7014", "#cc4c02", "#993404", "#662506"))(10)

if(is.null(colors)){colors = cuscol}


#q2 <- quantile(ordmat)["50%"]
#rp <- max(ordmat) + q2

#for(k in rownames(ordmat)){      ## legacy
 # ordmat[k,k] <- rp}

corrplot::corrplot(ordmat, method = method, type = "lower", is.corr = F, col = colors,
         diag = diag)

out <- list(sorder, ordmat)
names(out) <- c("species","matrix")

return(out)


}










