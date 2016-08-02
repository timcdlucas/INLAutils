
#' Autoplot method for INLA objects
#'
#' Replicate the plots produced by \code{\link{plot}} using ggplot.
#'
#'@param object An inla object.
#'@param which Vector of integers selecting which plots (1 -- 4) are wanted.
#'@param priors Logical, plot priors as well.
#'@param CI Plot credible intervals. TRUE for 95\% CI or a numeric in [0, 1]
#'@param ... other arguments passed to methods
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
#'  data(Epil)
#'  ##Define the model
#'  formula = y ~ Trt + Age + V4 +
#'           f(Ind, model="iid") + f(rand,model="iid")
#'  result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))
#'  plot(result)
#'
#'  p <- autoplot(result)
#'  p
#'
#'  # Change the theme
#'  p + theme_bw()
#'
#'  # Add a title to a subplot
#'  p[2] <- p[2] + ggtitle('Hyper parameters')
#'  p
#'
#'  # Switch plot of fixed effects posteriors to not rescale x axis
#'  #   If variables are on the same scale, this may provide a useful comparison
#'  p[1] <- p[1] + facet_wrap('var', scale = 'free_y')
#'  p
#'


autoplot.inla <- function(object, which = c(1:3, 5), priors = FALSE, CI = FALSE, ...){

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

  if(length(object$summary.random) == 0 & 3 %in% which){
    warning('Plot 3 selected in which, but no random effects to plot marginals for.')
    warning('Plot 3 will not be plotted.')
    which <- which[which != 3]
  }

  if(length(object$summary.random) == 0 & 4 %in% which){
    warning('Plot 4 selected in which, but no random effects to plot marginals for.')
    warning('Plot 4 will not be plotted.')
    which <- which[which != 4]
  }


  # Create empty list.
  plots <- list()


  # Plot marginals for fixed effects
  if(1 %in% which){
    plots$fixed.marginals <- plot_fixed_marginals(object, priors, CI)
  }

  # Plot marginals for hyperparameters
  if(2 %in% which){
    plots$hyper.marginals <- plot_hyper_marginals(object, CI)
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

  methods::new('ggmultiplot', plots = plots, nrow = 2)
}





# ------------------------------------------------------------------------------ #
# individual plot types. 
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
  if(length(x$summary.random) == 0) stop('No random effects to plot')
  if(type == 'line'){
    allSummary <- lapply(seq_len(length(x$summary.random)), 
                      function(p) data.frame(x$summary.random[[p]], var = names(x$summary.random)[p]))
    allSummary <- do.call(rbind, allSummary)
    allSummary$ID <- as.numeric(factor(allSummary$ID))

    # plot
    p <- ggplot2::ggplot(allSummary, ggplot2::aes_string(x = 'ID', y = 'mean')) +
           ggplot2::facet_wrap('var', scales = 'free', ncol = 1) +
           ggplot2::geom_line() +
           ggplot2::xlab('ID') +
           ggplot2::geom_ribbon(ggplot2::aes_string(ymin = '`X0.025quant`', ymax = '`X0.975quant`'), alpha = 0.3)
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
    p <- ggplot2::ggplot(combMarginals, ggplot2::aes_string(x = 'ID', y = 'x')) + 
           ggplot2::facet_wrap('var', scales = 'free', ncol = 1) +
           ggplot2::geom_boxplot(outlier.size = 0.0, outlier.colour = '#FFFFFF00') 

  }
  return(p)
}




#'@name plot_fixed_marginals
#'@param CI Plot credible intervals. TRUE for 95\% CI or a numeric in [0, 1]
#'@param priors Logical, plot priors as well.
#'@rdname plot_random_effects
#'@export

plot_fixed_marginals <- function(x, priors = FALSE, CI = FALSE){
  # Combine all marginals
  allMarginals <- lapply(seq_len(length(x$marginals.fixed)), 
                    function(p) data.frame(x$marginals.fixed[[p]], var = names(x$marginals.fixed)[p]))
  allMarginals <- do.call(rbind, allMarginals)

  # Plot
  p <- ggplot2::ggplot(allMarginals, ggplot2::aes_string('x', 'y')) + 
         ggplot2::facet_wrap('var', scales = 'free') +
         ggplot2::geom_line() 

  if(priors){
    # empty dataframe for priors
    priorParams <- extractPriors(x)
    evalPriors <- evalPriors(x, allMarginals, priorParams)

    p <- p + ggplot2::geom_line(data = evalPriors, colour = '#2078C0')
  }

  
  # If CI is just true, use 95% as default
  if(identical(CI, TRUE)){
    CI <- 0.95
  }
  # Unless CI is false, loop through fixed effects, find quantiles, find y values for those quantiles
  if(CI){
    CIs <- list()
    for(i in seq_len(length(x$marginals.fixed))){
      CIs[[i]] <- data.frame(x = c(INLA::inla.qmarginal((1 - CI) / 2, x$marginals.fixed[[i]]), 
                                          INLA::inla.qmarginal(1 - (1 - CI) / 2, x$marginals.fixed[[i]])),
                             var = names(x$marginals.fixed)[i],
                             y0 = 0
                            )
      CIs[[i]]$y <- INLA::inla.dmarginal(CIs[[i]]$x, x$marginals.fixed[[i]])
      
    }
    CIs <- do.call(rbind, CIs)
  
    # Add to plot
    p <- p + ggplot2::geom_segment(data = CIs, ggplot2::aes_string(x = 'x', xend = 'x', y = 'y', yend = 'y0'), colour = 'darkgrey')
    
  }

  return(p)
}


#'@name plot_hyper_marginals
#'@rdname plot_random_effects
#'@export

plot_hyper_marginals <- function(x, CI = FALSE){

  allMarginals <- lapply(seq_len(length(x$marginals.hyperpar)), 
                    function(p) data.frame(x$marginals.hyperpar[[p]], var = names(x$marginals.hyperpar)[p]))
  allMarginals <- do.call(rbind, allMarginals)


  # Plot
  p <- ggplot2::ggplot(allMarginals, ggplot2::aes_string('x', 'y')) + 
         ggplot2::facet_wrap('var', scales = 'free') +
         ggplot2::geom_line() 

  
  
  # If CI is just true, use 95% as default
  if(identical(CI, TRUE)){
    CI <- 0.95
  }
  # Unless CI is false, loop through fixed effects, find quantiles, find y values for those quantiles
  if(CI){
    CIs <- list()
    for(i in seq_len(length(x$marginals.hyper))){
      CIs[[i]] <- data.frame(x = c(INLA::inla.qmarginal((1 - CI) / 2, x$marginals.hyper[[i]]), 
                                          INLA::inla.qmarginal(1 - (1 - CI) / 2, x$marginals.hyper[[i]])),
                             var = names(x$marginals.hyper)[i],
                             y0 = 0
                            )
      CIs[[i]]$y <- INLA::inla.dmarginal(CIs[[i]]$x, x$marginals.hyper[[i]])
      
    }
    CIs <- do.call(rbind, CIs)
  
    # Add to plot
    p <- p + ggplot2::geom_segment(data = CIs, ggplot2::aes_string(x = 'x', xend = 'x', y = 'y', yend = 'y0'), colour = 'darkgrey')
    
  }

  return(p)
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

    p <- ggplot2::ggplot(d, ggplot2::aes_string(x = 'ID', y = 'mean')) +
           ggplot2::facet_wrap('plot', scales = 'free', ncol = 1) +
           ggplot2::geom_line() +
           ggplot2::geom_ribbon(ggplot2::aes_string(ymin = '`0.025quant`', ymax = '`0.975quant`'), alpha = 0.3)

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




# Get the priors out of the inla object and into a dta.frame
extractPriors <- function(x){
  priors <- data.frame(var = x$names.fixed, mean = NA, prec = NA)
  row.names(priors) <- x$names.fixed
  priors['(Intercept)', 2:3] <- c(x$.args$control.fixed$mean.intercept, x$.args$control.fixed$prec.intercept)

  # find and combine prior means    
  if(length(x$.args$control.fixed$mean) == 1){
    priors$mean[!priors$var == '(Intercept)'] <- x$.args$control.fixed$mean
  } else if(length(x$.args$control.fixed$mean) == length(x$names.fixed) - 1) {
    priors$mean[names(x$.args$control.fixed$mean)] <- unlist(x$.args$control.fixed$mean)
  } else {
    priors$mean[!priors$var == '(Intercept)'] <- x$.args$control.fixed$mean$default
    # Take mean values that are not defulat
    nondef <- unlist(x$.args$control.fixed$mean)[names(x$.args$control.fixed$mean) != 'default']
    priors[names(nondef), 'mean'] <- x$.args$control.fixed$mean[[1]]
  }

  # find and combine prior prec
  if(length(x$.args$control.fixed$prec) == 1){
    priors$prec[!priors$var == '(Intercept)'] <- x$.args$control.fixed$prec
  } else if(length(x$.args$control.fixed$prec) == length(x$names.fixed) - 1) {
    priors$prec[names(x$.args$control.fixed$prec)] <- unlist(x$.args$control.fixed$prec)
  } else {
    priors$prec[!priors$var == '(Intercept)'] <- x$.args$control.fixed$prec$default
    # Take mean values that are not defulat
    nondef <- unlist(x$.args$control.fixed$prec)[names(x$.args$control.fixed$prec) != 'default']
    priors[names(nondef), 'mean'] <- x$.args$control.fixed$prec[[1]]
  }


  
  return(priors)
}

# Return priors evaluated at points on x to be plotted.
#   Doesn't account for free_x very well?
evalPriors <- function(x, allMarginals, priors){
    priorsEval <- data.frame(x = rep(seq(min(allMarginals$x), 
                                         max(allMarginals$x), 
                                         length.out = 1000),
                                     by = length(x$names.fixed)),
                             var = rep(x$names.fixed, each = 1000),
                             mean = rep(priors$mean, each = 1000),
                             prec = rep(priors$prec, each = 1000))
    priorsEval$sd <- 1/sqrt(priorsEval$prec)
    priorsEval$sd[is.infinite(priorsEval$sd)] <- 1e100

    priorsEval$y <- with(priorsEval, dnorm(x, mean, sd))
    return(priorsEval)
}





# Get the priors out of the inla object and into a dta.frame
extractHyperPriors <- function(x){
  priors <- data.frame(var = x$names.fixed, mean = NA, prec = NA)
  row.names(priors) <- x$names.fixed
  priors['(Intercept)', 2:3] <- c(x$.args$control.fixed$mean.intercept, x$.args$control.fixed$prec.intercept)

  # find and combine prior means    
  if(length(x$.args$control.fixed$mean) == 1){
    priors$mean[!priors$var == '(Intercept)'] <- x$.args$control.fixed$mean
  } else if(length(x$.args$control.fixed$mean) == length(x$names.fixed) - 1) {
    priors$mean[names(x$.args$control.fixed$mean)] <- unlist(x$.args$control.fixed$mean)
  } else {
    priors$mean[!priors$var == '(Intercept)'] <- x$.args$control.fixed$mean$default
    # Take mean values that are not defulat
    nondef <- unlist(x$.args$control.fixed$mean)[names(x$.args$control.fixed$mean) != 'default']
    priors[names(nondef), 'mean'] <- x$.args$control.fixed$mean[[1]]
  }

  # find and combine prior prec
  if(length(x$.args$control.fixed$prec) == 1){
    priors$prec[!priors$var == '(Intercept)'] <- x$.args$control.fixed$prec
  } else if(length(x$.args$control.fixed$prec) == length(x$names.fixed) - 1) {
    priors$prec[names(x$.args$control.fixed$prec)] <- unlist(x$.args$control.fixed$prec)
  } else {
    priors$prec[!priors$var == '(Intercept)'] <- x$.args$control.fixed$prec$default
    # Take mean values that are not defulat
    nondef <- unlist(x$.args$control.fixed$prec)[names(x$.args$control.fixed$prec) != 'default']
    priors[names(nondef), 'mean'] <- x$.args$control.fixed$prec[[1]]
  }


  
  return(priors)
}



