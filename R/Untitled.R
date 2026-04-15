


cocurs <- function(species, data = NULL){


  species <- "Carya_myristiciformis"
  data <- treeFIA(filter.by = "state", "Arkansas")

  t <- arber::sites_with(species, x = data)
  tsite <- unique(t$ID)
  tspp <- unique(t$Species)
  rspp <- tspp[!tspp %in% species]







  ## code given # Initialize an empty data frame to store results
  results_df <- data.frame(i = character(), j = character(), prob = numeric())

  # Loop through each species
  for (idx in seq_along(tspp)) {
    i <- tspp[idx]

    # Remove `i` from `tspp` to get `dspp`
    dspp <- tspp[tspp != i]

    # Loop through each species for species `i`
    for (j_idx in seq_along(dspp)) {
      j <- dspp[j_idx]
      e <- psco(i, j)
      prob_value <- e$prob

      # Append the result to the data frame
      results_df <- rbind(results_df, data.frame(i = i, j = j, prob = prob_value))
    }
  }
  print(results_df)
  redf_wide <- results_df %>%
    tidyr::pivot_wider(names_from = i,
                       values_from = prob) %>%
    tibble::column_to_rownames(var = "j") %>%
    dplyr::select(!!!tspp)
  ordf_wide <- redf_wide[tspp,]
  print(ordf_wide)

  ordmat <- as.matrix(ordf_wide)
  ordmat[is.na(ordmat)] <- 0

  cuscol <- colorRampPalette(c( "#662506",  "#993404", "#cc4c02", "#ec7014",
                                "#fe9929",  "#fec44f", "#8c510a"))(10)


  q2 <- quantile(ordmat)["50%"]
  rp <- max(ordmat) + q2

  for(k in rownames(ordmat)){
    ordmat[k,k] <- rp}

  corrplot::corrplot(ordmat, method = "square", type = "lower", is.corr = F, col = cuscol,
                     diag = F)
  }


