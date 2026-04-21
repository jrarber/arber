










#### Summary format ####
summary.randnova <- function(object){
  table <- data.frame(N = c(sum(summary(object$Observed.model)[[1]][,1]) + 1,
                            (length(object$simulated.F.statistics))),
                      F_Value = c(summary(object$Observed.model)[[1]][1,4],
                                  mean(object$simulated.F.statistics)),
                      Prob = c(object$probability, NA),
                      row.names = c("Observations", "Iterations"))
  names(table) <- c("N", "F Value", "Prob(>F)")
  if("post.hoc" %in% names(object)){
    comparisons <- list()
    for(numb in seq_along(object$post.hoc$comparisons)){
      comps <- object$post.hoc$comparisons
      name <- comps[numb]
      distr.vals <- sort(object$post.hoc$sim.difference.values[,name])
      obs.diff <- object$post.hoc$obs.difference.values[[numb]]
      crit.vals <- object$post.hoc$critical.values

      comparisons[[numb]] <- data.frame(Left.tailed = crit.vals[1, name],
                                        Right.tailed = crit.vals[2, name],
                                        Two.tailed = crit.vals[3, name],
                                        Observed = obs.diff,
                                        Probs = (length(distr.vals[distr.vals >= obs.diff]) + 1) /
                                          (length(distr.vals) + 1),
                                        row.names = name)
    }
    mean.difference = do.call(rbind, comparisons)
    names(mean.difference) <- c("Left.tailed", "Right.tailed", "Two.tailed",
                                "Observed", "Prob(>Obs)")
    out <- list(table, mean.difference)
    names(out) <- c("ANOVA","pairwise")
  } else if(!"post.hoc" %in% names(object)){
    out = list(table); "ANOVA" -> names(out)}
  class(out) <- "summary.randnova"
  return(out)
}
