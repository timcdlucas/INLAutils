
#' Autoplot method for INLA objects
#'
#' Replicate the plots produced by \code{\link{plot}} using ggplot.
#'
#'@param object An inla object
#'@param which Vector of integers selecting which plots (1 -- 4) are wanted.
#'
#'@export
#'
#'@details
#'  The plot types are
#' \enumerate{
#'   \item Marginal plot for fixed effects
#'   \item Marginal plot for hyper parameters
#'   \item Marginal line plot for random effects
#'   \item Marginal boxplot plot for random effects (more informative then 3, but gets big with lots of levels
#'   \item
#' }
#'
#'@examples
#'  library(INLA)
#'  library(ggplot2)
#'  data(Epil)
#'  ##Define the model
#'  formula = y ~ Trt + Age + V4 +
#'           f(Ind, model="iid") + f(rand,model="iid")
#'  result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))
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
#'


autoplot.inla <- function(object, which = c(1:3, 5)){

  assert_that(is.numeric(which))

  if(!all(which %in% 1:5)){
    stop('which should only contain integers in 1:5')
  }

  # Check that the plots requested are possible
  if(length(object$marginals.fixed) == 0 & 1 %in% which){
    warning('Plot 1 selected in which, but no fixed effects to plot marginals for.')
    warning('Plot 1 will not be plotted.')
    which <- which[which != 1]
  }

  if(5 %in% which & !object$.args$control.predictor$compute){
    warning('Plot 5 selected but this can only be plotted if `control.predictor = list(compute = TRUE)` is passed to `inla`.
             \nPlot 5 will not be plotted.')
    which <- which[which != 5]
  }

  # Create empty list.
  plots <- list()


  # Plot marginals for fixed effects
  if(1 %in% which){
    plots$fixed.marginals <- plot_fixed_marginals(object)
  }

  # Plot marginals for hyperparameters
  if(2 %in% which){
    plots$hyper.marginals <- plot_hyper_marginals(object)
  }

  # plot random effects
  if(3 %in% which){
        plots$random.effects.line <- plot_random_effects(object, type = 'line')
  }

  if(4 %in% which){
    plots$random.effects.boxplots <- plot_random_effects(object, type = 'boxplot')
  }
   
  # plot predicted data 
  if(5 %in% which){
    plots$marginal.fitted <- plot_marginals_fitted(object)
  }

  new('ggmultiplot', plots = plots, nrow = 2)
}





# ------------------------------------------------------------------------------ #
# individual plot types. Export? Probably.
# ------------------------------------------------------------------------------ #



#' Individual plot functions for INLA objects
#'
#' Replicate the individual plots produced by \code{\link{plot}} using ggplot.
#'
#'@param x An inla object
#'@param type Which type of plot? 'boxplot' or 'line'
#'
#'@export
#'@name plot_random_effects
#'
#'
#'@examples
#'  library(INLA)
#'  data(Epil)
#'  ##Define the model
#'  formula = y ~ Trt + Age + V4 +
#'           f(Ind, model="iid") + f(rand,model="iid")
#'  result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))
#'
#'  plot_random_effects(result)
#'  plot_random_effects(result, type = 'boxplot')
#'  plot_fixed_marginals(result)
#'  plot_hyper_marginals(result)


plot_random_effects <- function(x, type = 'line'){

  assert_that(type %in% c('line', 'boxplot'))
  if(type == 'line'){
    allSummary <- lapply(seq_len(length(x$summary.random)), 
                      function(p) data.frame(x$summary.random[[p]], var = names(x$summary.random)[p]))
    allSummary <- do.call(rbind, allSummary)

    # plot
    p <- ggplot2::ggplot(allSummary, aes(x = as.numeric(factor(ID)), y = mean)) +
           facet_wrap('var', scales = 'free', ncol = 1) +
           geom_line() +
           xlab('ID') +
           geom_ribbon(aes(ymin = `X0.025quant`, ymax = `X0.975quant`), alpha = 0.3)
           #geom_line(aes(y = X0.025quant), linetype = 2) +
           #geom_line(aes(y = X0.975quant), linetype = 2)
  }

  if(type == 'boxplot'){

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
           facet_wrap('var', scales = 'free', ncol = 1) +
           geom_boxplot(outlier.size = 0.0, outlier.colour = '#FFFFFF00') 

  }
  return(p)
}




#'@name plot_fixed_marginals
#'@rdname plot_random_effects
#'@export

plot_fixed_marginals <- function(x){
  # Combine all marginals
  allMarginals <- lapply(seq_len(length(x$marginals.fixed)), 
                    function(p) data.frame(x$marginals.fixed[[p]], var = names(x$marginals.fixed)[p]))
  allMarginals <- do.call(rbind, allMarginals)

  # Plot
  p <- ggplot2::ggplot(allMarginals, aes(x, y)) + 
         facet_wrap('var', scales = 'free_y') +
         geom_line() 
}


#'@name plot_hyper_marginals
#'@rdname plot_random_effects
#'@export

plot_hyper_marginals <- function(x){

  allMarginals <- lapply(seq_len(length(x$marginals.hyperpar)), 
                    function(p) data.frame(x$marginals.hyperpar[[p]], var = names(x$marginals.hyperpar)[p]))
  allMarginals <- do.call(rbind, allMarginals)


  # Plot
  p <- ggplot2::ggplot(allMarginals, aes(x, y)) + 
         facet_wrap('var', scales = 'free') +
         geom_line() 
}




#'@name plot_marginals_fitted
#'@rdname plot_random_effects
#'@export

plot_marginals_fitted <- function(x){

  #assert_that(type %in% c('line'))
  #if(type == 'line'){
    d1 <- cbind(ID = 1:NROW(x$summary.linear.predictor), x$summary.linear.predictor[, -7], plot = 'Linear Predictor')
    d2 <- cbind(ID = 1:NROW(x$summary.fitted.values), x$summary.fitted.values, plot = 'Fitted Values')
    
    d <- rbind(d1, d2)

    p <- ggplot2::ggplot(d, aes(x = ID, y = mean)) +
           facet_wrap('plot', scales = 'free', ncol = 1) +
           geom_line() +
           geom_ribbon(aes(ymin = `0.025quant`, ymax = `0.975quant`), alpha = 0.3)
           #geom_line(aes(y = `0.025quant`), linetype = 2) +
           #geom_line(aes(y = `0.975quant`), linetype = 2)
  #}

#  if(type == 'boxplot'){

#    allMarginals = list()
#    for(i in seq_len(length(x$marginals.fitted.values))){
#      allMarginals[[i]] <- lapply(seq_len(length(x$marginals.fitted.values[[i]])), 
#                             function(p) data.frame(x$marginals.fitted.values[[i]][[p]], 
#                                                    ID = as.character(p)))
#      allMarginals[[i]] <- do.call(rbind, allMarginals[[i]])
#    }
#    combMarginals <- do.call(rbind, allMarginals)
#    names(combMarginals) <- c('x', 'ID')

#    # Plot
#    p <- ggplot2::ggplot(combMarginals, aes(ID, y = x)) + 
#           geom_boxplot(outlier.size = 0.01) 

#  }
  return(p)
}











