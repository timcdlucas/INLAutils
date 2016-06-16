
#' Autoplot method for INLA objects
#'
#' Replicate the plots produced by \link{\code{plot}} using ggplot.
#'
#'@param x An inla object
#'@param which Vector of integers selecting which plots (1 -- 4) are wanted.
#'
#'@export
#'
#'
#'@examples
#'  data(Epil)
#'  my.center = function(x) (x - mean(x))
#'  ## make centered covariates
#'  Epil$CTrt    = my.center(Epil$Trt)
#'  Epil$ClBase4 = my.center(log(Epil$Base/4))
#'  Epil$CV4     = my.center(Epil$V4)
#'  Epil$ClAge   = my.center(log(Epil$Age))
#'  Epil$CBT     = my.center(Epil$Trt*Epil$ClBase4)
#'  ##Define the model
#'  formula = y ~ ClBase4 + CTrt + CBT+ ClAge + CV4 +
#'           f(Ind, model="iid") + f(rand,model="iid")
#'  result = inla(formula,family="poisson", data = Epil)
#'  summary(result)
#'  plot(result)
#'
#'  p <- autoplot(result)
#'
#'  p
#'
#'  p + theme_bw()
#'
#'  p[2] <- p[2] + ggtitle('Hyper parameters')
#'  p

autoplot.inla <- function(x, which = 1:3){

  assert_that(is.numeric(which))

  if(!all(which %in% 1:4)){
    stop('which should only contain integers in 1:3')
  }

  # Create empty list.
  plots <- list()

  # Check that the plots requested are possible
  if(length(x$marginals.fixed) == 0){
    warning('Plot 1 selected in which, but not fixed effects to plot marginals for.')
    which <- which[which != 1]
  }


  # Plot marginals for fixed effects
  if(1 %in% which){


    # Combine all marginals
    allMarginals <- lapply(seq_len(length(x$marginals.fixed)), 
                      function(p) data.frame(x$marginals.fixed[[p]], var = names(x$marginals.fixed)[p]))
    allMarginals <- do.call(rbind, allMarginals)

    # Plot
    p <- ggplot2::ggplot(allMarginals, aes(x, y)) + 
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
    p <- ggplot2::ggplot(allMarginals, aes(x, y)) + 
           facet_wrap('var', scales = 'free_x') +
           geom_line() 
    plots$hyper.marginals <- p
  }

  if(3 %in% which){
    allSummary <- lapply(seq_len(length(x$summary.random)), 
                      function(p) data.frame(x$summary.random[[p]], var = names(x$summary.random)[p]))
    allSummary <- do.call(rbind, allSummary)

    # plot
    p <- ggplot2::ggplot(allSummary, aes(x = as.numeric(ID), y = mean)) +
           facet_wrap('var', scales = 'free_x') +
           geom_line() +
           geom_line(aes(y = X0.025quant), linetype = 2) +
           geom_line(aes(y = X0.975quant), linetype = 2)
    plots$random.effects.line <- p
  }

  if(4 %in% which){

    allMarginals = list()
    for(i in seq_len(length(x$marginals.random))){
      allMarginals[[i]] <- lapply(seq_len(length(x$marginals.random[[i]])), 
                             function(p) data.frame(x$marginals.random[[i]][[p]], 
                                                    ID = as.character(p), 
                                                    var = names(x$marginals.random)[i]))
      allMarginals[[i]] <- do.call(rbind, allMarginals[[i]])
    }
    combMarginals <- do.call(rbind, allMarginals)
    

    # Plot
    p <- ggplot2::ggplot(combMarginals, aes(ID, y = x)) + 
           facet_wrap('var', scales = 'free_x') +
           geom_boxplot(outlier.size = 0.01) 
    plots$random.effects.boxplots <- p
  }
    
  new('ggmultiplot', plots = plots)
}
