







##### Print summary format ####
print.summary.randnova <- function(x) {
  get_stars <- function(p) {
    ifelse(is.na(p), "",
           ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.1,   ".", "")))))
  }

  tab <- x[[1]]
  ps = tab[,3]
  starss <- get_stars(ps)

  # Combine p-values + stars
  tab[,4] <- starss
  c(colnames(tab)[1:3], "") -> colnames(tab)

  # Replace NA with blank JUST for display
  tab[is.na(tab)] <- ""

  cat("Randomized ANOVA Table\n")
  print(tab, quote = FALSE)

  if(length(x) > 1){
    cat("\n\nPairwise Comparison Table\n")
    two <- x[[2]]
    ps <- two[,5]
    two[,6] <- get_stars(ps)
    c(colnames(two)[1:5], "") -> colnames(two)
    # Replace NA with blank for display
    two[is.na(two)] <- ""

    print(two, quote = F)
  }
}
