


#' Plot residuals of observed vs predicted values for INLA model
#'
#'
#'@param inla.model An inla object
#'@param observed The observed values
#'
#'@export
#'
#'@examples
#'  data(Epil)
#'  observed <- Epil[1:30, 'y']
#'  Epil <- rbind(Epil, Epil[1:30, ])
#'  Epil[1:30, 'y'] <- NA
#'  ## make centered covariates
#'  formula = y ~ Trt + Age + V4 +
#'           f(Ind, model="iid") + f(rand,model="iid")
#'  result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))
#'  plot.inla.residuals(result, observed)


plot.inla.residuals <- function(inla.model, observed){
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
#'
#'@export
#'
#'@examples
#'  data(Epil)
#'  observed <- Epil[1:30, 'y']
#'  Epil <- rbind(Epil, Epil[1:30, ])
#'  Epil[1:30, 'y'] <- NA
#'  ## make centered covariates
#'  formula = y ~ Trt + Age + V4 +
#'           f(Ind, model="iid") + f(rand,model="iid")
#'  result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))
#'  ggplot.inla.residuals(result, observed)


ggplot.inla.residuals <- function(inla.model, observed, binwidth = NULL){
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

  plots[[1]] <- ggplot(df, aes(x = predicted.p.value)) + 
                  geom_histogram(binwidth = binwidth)

  plots[[2]] <- ggplot(df, aes(x = predicted, y = observed)) +
                  geom_point() +
                  geom_abline(slope = 1, intercept = 0) +
                  labs(y = "Observed", x = "Fitted") +
                  lims(x = c(min, max), y = c(min, max))


  new('ggmultiplot', plots = plots)

}
