








fwe <- function(comparisons, alpha){
  gamm = 1 - alpha
  error = (1 - (gamm^comparisons))*100
  cat("Overall likelihood of a Type I error =", error, "%\n")
}
fwe(6, 0.05)
