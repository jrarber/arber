#' Test the Distribution of Data
#'
#' Visually the distribution of data in a vector or a data frame.
#'
#' @param data The data for the distribution. If a dataframe is supplied, only numeric columns will be displayed.
#' @returns Provides visual graphs of the data and summary statistical output, including mean and standard deviation.
#'
#' @export








dist_test <- function(data){

# check if it is a dataframe
  if(class(data) %in% c("data.frame", "list")){
    df <- data
    df <- df[, vapply(df, function(x) all(is.na(x) | is.finite(as.numeric(x))), logical(1))]

    cols <- names(df)
    for(i in cols) {
      invisible(readline(prompt = "Press [Enter] to continue…"))
      x <- df[[i]]
      m <- mean(x, na.rm = T)
      std <- sd(x, na.rm = T)
      hist(x, density = 10, prob = TRUE, main = paste("Histogram of", i))
      curve(dnorm(x, mean = m, sd = std), col = "darkblue",
            lwd = 2, add = TRUE)

    }
  } else {
    #if(method == "density"){
      x <- data
      mu <- mean(x, na.rm = T)
      std <- sd(x, na.rm = T)
      hist(x, freq = FALSE, density = 10, main = paste("Histogram of",
                                                       deparse(substitute(data))))
      curve(dnorm(x, mean = mu, sd = std), add = T, col = "darkblue", lwd = 2)
    #} else if(method == "qq"){

    #}

   # no
  }
}
#, method = "hist", predict = T

#  if(method == "boxplot") {
 #   vols <- names(WetSiteEnvr)[5:length(names(WetSiteEnvr))]
#    for(i in vols){boxplot(WetSiteEnvr[[i]] ~ WetSiteEnvr$cluster,
#                         main = i,
#                         xlab = "Cluster",
#                         ylab = i,)
#      points(WetClusterEnvr[[i]] ~ WetClusterEnvr$cluster, col = "red", pch = 18, cex = 1.5)
#      abline(a = mean(WetClusterEnvr[[i]]), b = 0, col = "blue")
#      invisible(readline(prompt = "Press [Enter] to continue…"))
#    }
#  }

