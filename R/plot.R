
#'
#'
#'
#'
#'@examples
#'data(Epil)
#'my.center = function(x) (x - mean(x))

#'## make centered covariates
#'Epil$CTrt    = my.center(Epil$Trt)
#'Epil$ClBase4 = my.center(log(Epil$Base/4))
#'Epil$CV4     = my.center(Epil$V4)
#'Epil$ClAge   = my.center(log(Epil$Age))
#'Epil$CBT     = my.center(Epil$Trt*Epil$ClBase4)

#'##Define the model
#'formula = y ~ ClBase4 + CTrt + CBT+ ClAge + CV4 +
#          f(Ind, model="iid") + f(rand,model="iid")

#'result = inla(formula,family="poisson", data = Epil)
#'summary(result)
#'plot(result)
#'autoplot(result)

autoplot.inla <- function(x, which = c(1, 2)){

  plots <- list()
  

  # Plot marginals for fixed effects
  if(1 %in% which){
    # Combine all marginals
    allMarginals <- lapply(seq_len(length(x$marginals.fixed)), 
                      function(p) data.frame(x$marginals.fixed[[p]], var = names(x$marginals.fixed)[p]))
    allMarginals <- do.call(rbind, allMarginals)

    # Plot
    p <- ggplot(allMarginals, aes(x, y)) + 
           facet_wrap('var') +
           geom_line() 
    plots$fixed.marginals <- p
  }

  # Plot marginals for hyperparameters
  if(2 %in% which){

    allMarginals <- lapply(seq_len(length(x$marginals.hyperpar)), 
                      function(p) data.frame(x$marginals.hyperpar[[p]], var = names(x$marginals.hyperpar)[p]))
    allMarginals <- do.call(rbind, allMarginals)


    # Plot
    p <- ggplot(allMarginals, aes(x, y)) + 
           facet_wrap('var') +
           geom_line() 
    plots$hyper.marginals <- p
  }
    
  new('ggmultiplot', plots = plots)
}
