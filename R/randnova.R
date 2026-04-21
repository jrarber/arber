#' Randomization Analysis of Variance
#'
#'
#'
#'
#'



randnova <- function(formula, data = NULL,
                     iterations = 9999,
                     post.hoc = FALSE,
                     plot = TRUE,
                     all.plots = FALSE,
                     alpha = 0.05,
                     post.hoc.test = "mean.difference"){
  ## generate the observed data set
  mf <- model.frame(formula, data)
  y <- mf[[1]]
  x <- mf[[2]]

  x = as.character(x)
  y = as.numeric(y)

  obs.data <- data.frame(x = x, y = y)

  prim.mod <- aov(y ~ x, data = obs.data)
  obs_F <- summary(prim.mod)[[1]][1,4]


  null_f_distribution <- numeric(iterations)

  null_group_distributions <- list()

  differences <- list()
  for(i in seq_len(iterations)){
    cat("Run number", i, "\r")
    # Shuffle treatment
    shuffled_labels <- sample(x) # NO replacement

    # Run ANOVA on the shuffled data and grab the F values
    s_anova <- summary(aov(y ~ shuffled_labels))
    null_f_distribution[[i]] <- s_anova[[1]]["shuffled_labels", "F value"]

    # Create a data frame of our new randomized group values
    n.df.raw <- data.frame(x = shuffled_labels, y = y)

    # Order that randomized data frame by treatment alphabetically.
    ## this ensure that all of the group values are always in the same order
    n.df <- n.df.raw[order(n.df.raw$x),]

    if(post.hoc){
      n.df.uni <- unique(n.df$x)
      group.diff <- matrix(nrow = length(n.df.uni), ncol = length(n.df.uni),
                           dimnames = list(unique(x), unique(x)))
      for(n in n.df.uni){
        rem.uni <- n.df.uni[n.df.uni != n]
        for(r in rem.uni){
          group.diff[n, r] <- (mean(n.df[n.df$x == n, 2], na.rm = T) -
                                 mean(n.df[n.df$x == r, 2], na.rm = T))
        }
        differences[[i]] <- group.diff
      }
    } # END bootstrapping post-hoc


    # Create a list to store group distributions
    null.distr <- list()

    for(g in seq_len(length(unique(n.df$x)))){
      group = unique(n.df$x)[g]
      group.values <- n.df[n.df$x == group, 2]
      null.distr[[g]] <- group.values
    }
    # Name the group distribution values
    names(null.distr) <- unique(n.df$x)

    # Pull distributions together to a data frame
    null.distr <- do.call(cbind, null.distr)

    # Store simulated values for this run
    null_group_distributions[[i]] <- null.distr

  } ## END THE SIMULATIONS

  cat("\n\nRandomization of F statistics complete\n")

  # Strap together simulated group distributions
  group_rand_distributions <- do.call(rbind, null_group_distributions)


  if(post.hoc){
    pairwise <- as.data.frame(combn(unique(x), 2))
    paired.list <- vector()
    pairwise.observed <- list()
    pairwise.vectors <- list()
    if(post.hoc.test == "mean.difference"){
      for(comp in seq_len(length(colnames(pairwise)))){
        p1 <- pairwise[1, comp]
        p2 <- pairwise[2, comp]

        values <- sapply(differences, function(m) m[p2, p1])

        paired.list[comp] <- paste0(p1, ".", p2)
        pairwise.vectors[[comp]] <- values

        pairwise.observed[[comp]] <- mean(as.numeric(obs.data[obs.data$x == p1, 2]), na.rm = T) -
          mean(as.numeric(obs.data[obs.data$x == p2, 2]), na.rm = T)
      }
    }
    names(pairwise.vectors) <- paired.list
    names(pairwise.observed) <- paired.list
    pair.df <- as.data.frame(pairwise.vectors)
    obs.differences <- as.data.frame(pairwise.observed)

    critical.values <- list()
    for(cols in seq_len(length(names(pair.df)))){
      Left.pos <- (length(pair.df[,cols]) + 1) * alpha
      order.col <- sort(pair.df[, cols])
      Left.tailed <- order.col[Left.pos]
      Right.pos <- (length(pair.df[,cols]) + 1) * (1-alpha)
      Right.tailed <- order.col[Right.pos]
      Two.pos <- (length(pair.df[,cols]) + 1) * (alpha/2)
      Two.tailed <- order.col[Two.pos]
      critical.values[[cols]] <- c(Left.tailed, Right.tailed, Two.tailed)
    }
    critical.values <- do.call(cbind, critical.values)
    rownames(critical.values) <- c("Left.tailed", "Right.tailed", "Two.tailed")
    colnames(critical.values) <- names(pair.df)


    post.hoc.info <- list(comparisons = paired.list,
                          obs.difference.values = obs.differences,
                          sim.difference.values = pair.df,
                          critical.values = critical.values)
  } # END post-hoc calculations / summary


  p.value <- ((length(null_f_distribution[null_f_distribution >= obs_F])+1)/
                (iterations+1))

  cat("\nObserved F-value:", obs_F, "\nProb(≥):", p.value)

  if(plot){
    maxF = max(null_f_distribution) + (sd(null_f_distribution)/8)
    if(obs_F > max(null_f_distribution)) maxF = obs_F + (sd(null_f_distribution)/8)
    hist(null_f_distribution, breaks = 20,
                 xlim = c(min(null_f_distribution), maxF), xlab = "Simulated F Values")
    abline(v = obs_F, col = "red")
    cat("\n")
    if(all.plots){
      for(comps in names(pair.df)){
      invisible(readline(prompt = "Press [Enter] to continue…"))
      hist(pair.df[,comps], main = comps)

      abline(v = obs.differences[comps], col = "red")
      }
    }
  }


  if(!post.hoc){
    output <- list(Observed.model = prim.mod,
                 simulated.F.statistics = null_f_distribution,
                 simiulated.group.values = group_rand_distributions,
                 probability = p.value)
  } else {
    output <- list(Observed.model = prim.mod,
                   simulated.F.statistics = null_f_distribution,
                   simulated.group.values = group_rand_distributions,
                   probability = p.value,
                   post.hoc = post.hoc.info)
  }
  class(output) <- "randnova"
  return(output)


}

