


#' Plot residuals of observed vs predicted values for INLA model
#'
#'
#'@param inla.model An inla object
#'@param observed The observed values
#'
#'@export
#'
#'@examples
#'\dontrun{
#'  library(INLA)
#'  data(Epil)
#'  observed <- Epil[1:30, 'y']
#'  Epil <- rbind(Epil, Epil[1:30, ])
#'  Epil[1:30, 'y'] <- NA
#'  ## make centered covariates
#'  formula = y ~ Trt + Age + V4 +
#'           f(Ind, model="iid") + f(rand,model="iid")
#'  result = inla(formula, family="poisson", data = Epil, 
#'                control.predictor = list(compute = TRUE, link = 1))
#'  plot_inla_residuals(result, observed)
#'  
#'
#'  data(Seeds)
#'  l <- nrow(Seeds)
#'  Seeds <- rbind(Seeds, Seeds)
#'  Seeds$r[1:l] <- NA
#'
#'
#'  formula = r ~ x1 * x2 + f(plate, model = "iid")
#'  mod.seeds = inla(formula, data=Seeds, family = "binomial", Ntrials = n, 
#'                   control.predictor = list(compute = TRUE, link = 1))
#'  plot_inla_residuals(mod.seeds, na.omit(Seeds$r / Seeds$n))
#'  }



plot_inla_residuals <- function(inla.model, observed){
  
  if(is.null(inla.model$marginals.fitted.values)) stop('No fitted values to plot')
  if(any(is.na(inla.model$misc$linkfunctions$link))){ 
    warning('Fitted values from the INLA model may have been returned on the linear, rather than link scale. Use `control.predictor = list(link = 1)` to make sure all fitted values are on the natural scale.')
  }

  predicted.p.value <- c()
  n <- length(observed)
  for(i in (1:n)){
    predicted.p.value[i] <- INLA::inla.pmarginal(q = observed[i], marginal = inla.model$marginals.fitted.values[[i]])
  }

  # Set up plot area
  par(mfrow = c(1,2))
  # Plot histogram of predicted p values
  hist(predicted.p.value,main = "", xlab = "Posterior predictive p-value")

  # Plot observed vs residuals 
  plot(x = inla.model$summary.fitted.values$mean[1:length(observed)], y = observed,
    xaxs="i", yaxs="i", ylab = "Observed", xlab = "Fitted", 
    xlim=c(min(observed, na.rm = TRUE), max(observed, na.rm = TRUE)), 
    ylim=c(min(observed, na.rm = TRUE), max(observed, na.rm = TRUE)))
  abline(a = 0, b = 1)
}



#' Plot residuals of observed vs predicted values for INLA model using ggplot2
#'
#'
#'@param inla.model An inla object
#'@param observed The observed values
#'@param CI Add credible intervals to the fitted values?
#'@param binwidth The size of the bins used for the histogram. If NULL ggplot guesses for you.
#'
#'@importFrom ggplot2 aes_string
#'@importFrom ggplot2 ggplot
#'@export
#'
#'@examples
#'\dontrun{
#'  library(INLA)
#'  data(Epil)
#'  observed <- Epil[1:30, 'y']
#'  Epil <- rbind(Epil, Epil[1:30, ])
#'  Epil[1:30, 'y'] <- NA
#'  ## make centered covariates
#'  formula = y ~ Trt + Age + V4 +
#'           f(Ind, model="iid") + f(rand,model="iid")
#'  result = inla(formula, family="poisson", data = Epil, 
#'                control.predictor = list(compute = TRUE, link = 1))
#'  p <- ggplot_inla_residuals(result, observed)
#'  
#'  # Subplots can be altered afterwards
#'  p[[1]] <- p[[1]] + theme_grey()
#'  cowplot::plot_grid(plotlist = p)
#'
#'  data(Seeds)
#'  l <- nrow(Seeds)
#'  Seeds <- rbind(Seeds, Seeds)
#'  Seeds$r[1:l] <- NA
#'
#'
#'  formula = r ~ x1 * x2 + f(plate, model = "iid")
#'  mod.seeds = inla(formula, data=Seeds, family = "binomial", Ntrials = n, 
#'                   control.predictor = list(compute = TRUE, link = 1))
#'  ggplot_inla_residuals(mod.seeds, na.omit(Seeds$r / Seeds$n))
#'}

ggplot_inla_residuals <- function(inla.model, observed, CI = FALSE, binwidth = NULL){
  
  if(is.null(inla.model$marginals.fitted.values)) stop('No fitted values to plot')
  if(any(is.na(inla.model$misc$linkfunctions$link))){ 
    warning('Fitted values from the INLA model may have been returned on the linear, rather than link scale. Use `control.predictor = list(link = 1)` to make sure all fitted values are on the natural scale.')
  }
  
  predicted.p.value <- c()
  n <- length(observed)
  for(i in (1:n)){
    predicted.p.value[i] <- INLA::inla.pmarginal(q = observed[i], marginal = inla.model$marginals.fitted.values[[i]])
  }

  df <- data.frame(predicted = inla.model$summary.fitted.values$mean[1:length(observed)],
                   observed = observed,
                   lower = inla.model$summary.fitted.values$`0.025quant`[1:length(observed)],
                   upper = inla.model$summary.fitted.values$`0.975quant`[1:length(observed)],
                   p.value = predicted.p.value)

  min <- min(df[, c('lower', 'observed')])
  max <- max(df[, c('upper', 'observed')])

  plots <- list()

  plots[[1]] <- ggplot2::ggplot(df, ggplot2::aes_string(x = 'predicted.p.value')) + 
                  ggplot2::geom_histogram(binwidth = binwidth)

  plots[[2]] <- ggplot2::ggplot(df, ggplot2::aes_string(x = 'predicted', y = 'observed')) +
                  ggplot2::geom_point() +
                  ggplot2::geom_abline(slope = 1, intercept = 0) +
                  ggplot2::labs(y = "Observed", x = "Fitted") +
                  ggplot2::lims(x = c(min, max), y = c(min, max))
  if(CI) plots[[2]] <- plots[[2]] + 
                         ggplot2::geom_segment(ggplot2::aes_string(x = 'lower', 
                                                          xend = 'upper', 
                                                          yend = 'observed'),
                                               alpha = 0.4) 
  
  print(cowplot::plot_grid(plotlist = plots))
  return(invisible(plots))
}




#' Plot residuals against covariate values for INLA model using ggplot2
#'
#'
#'@param inla.model An inla object
#'@param observed The observed values
#'@param CI plot credible intervals for each residual
#'@param se Plot a ribbon showing the standard error of the smoother.
#'@param method What method should be used for the smoother. 
#'  Defaults to loess unless data is large. 
#'  Other options include 'gam', 'loess', 'lm'.
#'  See \code{\link[ggplot2]{geom_smooth}} for details.
#'@export
#'
#'@examples
#'\dontrun{
#'  library(INLA)
#'  data(Epil)
#'  observed <- Epil[1:30, 'y']
#'  Epil <- rbind(Epil, Epil[1:30, ])
#'  Epil[1:30, 'y'] <- NA
#'  ## make centered covariates
#'  formula = y ~ Trt + Age + V4 +
#'           f(Ind, model="iid") + f(rand,model="iid")
#'  result = inla(formula, family="poisson", data = Epil, 
#'                control.predictor = list(compute = TRUE, link = 1))
#'  ggplot_inla_residuals2(result, observed)
#'  
#'
#'  data(Seeds)
#'  l <- nrow(Seeds)
#'  Seeds <- rbind(Seeds, Seeds)
#'  Seeds$r[1:l] <- NA
#'
#'
#'  formula = r ~ x1 * x2 + f(plate, model = "iid")
#'  mod.seeds = inla(formula, data=Seeds, family = "binomial", Ntrials = n, 
#'                   control.predictor = list(compute = TRUE, link = 1))
#'  ggplot_inla_residuals2(mod.seeds, na.omit(Seeds$r / Seeds$n), method = 'lm')
#'  }


ggplot_inla_residuals2 <- function(inla.model, observed, CI = FALSE, se = TRUE, method = 'auto'){
  
  if(is.null(inla.model$marginals.fitted.values)) stop('No fitted values to plot')
  if(any(is.na(inla.model$misc$linkfunctions$link))){ 
    warning('Fitted values from the INLA model may have been returned on the linear, rather than link scale. Use `control.predictor = list(link = 1)` to make sure all fitted values are on the natural scale.')
  }
  
  df <- data.frame(predicted = inla.model$summary.fitted.values$mean[1:length(observed)],
                   lower = inla.model$summary.fitted.values$`0.025quant`[1:length(observed)],
                   upper = inla.model$summary.fitted.values$`0.975quant`[1:length(observed)],
                   observed = observed)
  
  df$residual <- df$predicted - df$observed
  df$standardResidual <- df$residual / stats::sd(df$residual)
  df$standardUpper <- (df$upper - df$observed) / stats::sd(df$residual)
  df$standardLower <- (df$lower - df$observed) / stats::sd(df$residual)
  

  plot <- ggplot2::ggplot(df, ggplot2::aes_string(x = 'predicted', y = 'standardResidual')) +
            ggplot2::geom_point() +
            ggplot2::labs(y = "Standardised Residual", x = "Fitted") +
            ggplot2::geom_smooth(se = se, method = method) +
            ggplot2::geom_hline(yintercept = 0, linetype = 2, col = 'red')
  if(CI) plot <- plot + ggplot2::geom_segment(ggplot2::aes_string(y = 'standardLower', 
                                                         yend = 'standardUpper', 
                                                        xend = 'predicted'),
                                              alpha = 0.4) 
    
  plot
  return(plot)
}
