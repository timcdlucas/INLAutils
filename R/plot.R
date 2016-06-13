


autoplot.inla <- function(x){

  x$all.hyper$fixed

  plots <- list()
  for(p in seq_len(length(x$all.hyper$fixed))){
    plots[[p]] <- 
