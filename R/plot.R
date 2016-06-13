


autoplot.inla <- function(x){

  # Combine all marginals
  allMarginals <- lapply(seq_len(length(x$marginals.fixed)), 
                    function(p) data.frame(x$marginals.fixed[[p]], var = names(x$marginals.fixed)[p]))
  allMarginals <- do.call(rbind, allMarginals)

  # Plot
  ggplot(allMarginals, aes(x, y)) + 
    facet_wrap('var') +
    geom_line() 
    
}
