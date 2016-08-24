


#' Plot residuals of observed vs predicted values for INLA model
#'
#'
#'@param inla.model An inla object
#'@param observed The observed values
#'
#'@export
#'
#'@examples
#'  library(INLA)
#'  data(Epil)
#'  observed <- Epil[1:30, 'y']
#'  Epil <- rbind(Epil, Epil[1:30, ])
#'  Epil[1:30, 'y'] <- NA
#'  ## make centered covariates
#'  formula = y ~ Trt + Age + V4 +
#'           f(Ind, model="iid") + f(rand,model="iid")
#'  result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))
#'  plot_inla_residuals(result, observed)


plot_inla_residuals <- function(inla.model, observed){
  predicted.p.value <- c()
  n <- length(observed)
  for(i in (1:n)){
    predicted.p.value[i] <- inla.pmarginal(q = observed[i], marginal = inla.model$marginals.fitted.values[[i]])
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
#'@param binwidth The size of the bins used for the histogram. If NULL ggplot guesses for you.
#'
#'@export
#'
#'@examples
#'  library(INLA)
#'  data(Epil)
#'  observed <- Epil[1:30, 'y']
#'  Epil <- rbind(Epil, Epil[1:30, ])
#'  Epil[1:30, 'y'] <- NA
#'  ## make centered covariates
#'  formula = y ~ Trt + Age + V4 +
#'           f(Ind, model="iid") + f(rand,model="iid")
#'  result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))
#'  ggplot_inla_residuals(result, observed)


ggplot_inla_residuals <- function(inla.model, observed, binwidth = NULL){
  predicted.p.value <- c()
  n <- length(observed)
  for(i in (1:n)){
    predicted.p.value[i] <- inla.pmarginal(q = observed[i], marginal = inla.model$marginals.fitted.values[[i]])
  }

  df <- data.frame(predicted = inla.model$summary.fitted.values$mean[1:length(observed)],
                   observed = observed,
                   p.value = predicted.p.value)

  min <- min(df[, c('predicted', 'observed')])
  max <- max(df[, c('predicted', 'observed')])

  plots <- list()

  plots[[1]] <- ggplot2::ggplot(df, ggplot2::aes_string(x = 'predicted.p.value')) + 
                  ggplot2::geom_histogram(binwidth = binwidth)

  plots[[2]] <- ggplot2::ggplot(df, ggplot2::aes_string(x = 'predicted', y = 'observed')) +
                  ggplot2::geom_point() +
                  ggplot2::geom_abline(slope = 1, intercept = 0) +
                  ggplot2::labs(y = "Observed", x = "Fitted") +
                  ggplot2::lims(x = c(min, max), y = c(min, max))


  methods::new('ggmultiplot', plots = plots)

}




#' Plot residuals against covariate values for INLA model using ggplot2
#'
#'
#'@param inla.model An inla object
#'@param observed The observed values
#'
#'@export
#'
#'@examples
#'  library(INLA)
#'  data(Epil)
#'  observed <- Epil[1:30, 'y']
#'  Epil <- rbind(Epil, Epil[1:30, ])
#'  Epil[1:30, 'y'] <- NA
#'  ## make centered covariates
#'  formula = y ~ Trt + Age + V4 +
#'           f(Ind, model="iid") + f(rand,model="iid")
#'  result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))
#'  ggplot_inla_residuals(result, observed)


ggplot_inla_residuals2 <- function(inla.model, observed, binwidth = NULL){
  
  df <- data.frame(predicted = inla.model$summary.fitted.values$mean[1:length(observed)],
                   observed = observed)
  
  df$residual <- df$predicted - df$observed
  df$standardResidual <- df$residual / sd(df$residual)
  
  min <- min(df[, c('predicted', 'observed')])
  max <- max(df[, c('predicted', 'observed')])
  

  plot <- ggplot2::ggplot(df, ggplot2::aes_string(x = 'predicted', y = 'standardResidual')) +
    ggplot2::geom_point() +
    ggplot2::labs(y = "Standardised Residual", x = "Fitted") +
    ggplot2::geom_smooth() +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, col = 'red')
  plot
  return(plot)
}
