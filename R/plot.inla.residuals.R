


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
  plot(inla.model$summary.fitted.values$mean[1:length(observed)], xaxs="i", yaxs="i", 
    observed, ylab="Observed", xlab="Fitted", 
    xlim=c(min(observed, na.rm = TRUE), max(observed, na.rm = TRUE)), 
    ylim=c(min(observed, na.rm = TRUE), max(observed, na.rm = TRUE)))
  abline(a = 0, b = 1)
}
