#' A function to plot outputs of a spatial leave-one-out cross-validation in R using INLA.
#' 
#' This function plots the output of a spatial leave-one-out cross-validation (SLOO-CV) of one or several models running on INLA.
#' @param df dataframe (output of inla.sloo), which include the "Observed_response", "Predictions", "Residuals", RMSE, MAE (optional), and DS(optional)
#' @param alpha Numeric value (0,...,1) giving the threshold for computing confidence intervals (1-alpha) of rmse and mae estimation
#' @param mae If TRUE, compute the mean absolute error (mae) and the root mean square error (rmse). If FALSE, compute the rmse only.
#' @param ds If TRUE, compute the Dawid-Sebastiani score (ds). If FALSE, does not compute ds.
#' @param family Character string or list of Character string giving the family or families, respectively of the response in INLA format 
#' @param ntrials Numeric value (1,2,...) setting the number of trials for a Binomial family
#' @param sqroot If TRUE, compute the square root of the observed and predicted values to generate the rmse and/or mae. 
#' 
#' @export
#' @name slooplot.fun
#' @examples 
#' \dontrun{
#'  # SLOO function with one model formula (Bernoulli)
#'  df<-data.frame(Residuals=runif(10, 0.0, 1.0),RMSE=runif(10, 0.0, 2.0),MAE=runif(10, 0.0, 2.0),
#'  Observed_response= sample(c(0,1), replace=TRUE, size=10),Predictions=runif(10, 0.0, 1.0))
#'  alpha = 0.05
#'  family = bernoulli
#'  slooplot1<-slooplot.fun(df=df, alpha=0.05,mae=TRUE,ds=FALSE,family='bernoulli',sqroot=FALSE)
#'  }

slooplot.fun <- function(alpha = alpha, df = df, mae = mae, ds = ds, family = family, ntrials = ntrials, sqroot = sqroot) {
  # compute confidence intervals for rmse and mae confidence intervals
  rmse.LCI <- sqrt(length(df$Residuals)/stats::qchisq(1 - alpha/2, df = length(df$Residuals))) * df$RMSE
  rmse.HCI <- sqrt(length(df$Residuals)/stats::qchisq(alpha/2, df = length(df$Residuals))) * df$RMSE
  if (mae == TRUE) {
    mae.LCI <- sqrt(length(df$Residuals)/stats::qchisq(1 - alpha/2, df = length(df$Residuals))) * df$MAE
    mae.HCI <- sqrt(length(df$Residuals)/stats::qchisq(alpha/2, df = length(df$Residuals))) * df$MAE
  }
  if (family == "binomial") {
    df$Predictions <- df$Predictions*ntrials
  }
  
  if (sqroot == TRUE) {
    plotx <- sqrt(df$Observed_response)
    ploty <- sqrt(df$Predictions)
    xlabtext <- paste0("Sqrt.observed response", " ", "(", family, ")")
    ylabtext <- "Sqrt.prediction"
  } else {
    plotx <- df$Observed_response
    ploty <- df$Predictions
    xlabtext <- paste0("Observed response", " ", "(", family, ")")
    ylabtext <- "Prediction"
  }
  if (family == "gamma") {
    slooplot <- plot(plotx, ploty, pch = "+", xlab = xlabtext, ylab = ylabtext, pty = "s", asp = 1)
    abline(a = 0, b = 1, col = "grey")
    graphics::mtext(paste0("rmse mean sqrt", " ", "(", 1 - alpha, "% CI):", " ", round(df$RMSE, 2), " ", "(", round(rmse.LCI, 2), 
                           ";", round(rmse.HCI, 2), ")"), side = 3, adj = 0, cex = 0.8)
    if (mae == TRUE) {
      graphics::mtext(paste0(" ", "mae mean sqrt", " ", "(", 1 - alpha, "% CI):", " ", round(df$MAE, 2), " ", "(", round(mae.LCI, 
                                                                                                                         2), ";", round(mae.HCI, 2), ")"), side = 3, adj = 0, cex = 0.8, line = 0.8)
    }
    if (ds == TRUE) {
      graphics::mtext(paste0("dawid-sebastiani score:", " ", " ", " ", " ", " ", round(df$DS, 2)), side = 3, adj = 0, cex = 0.8, 
                      line = 1.6)
    }
    cat(slooplot)
    
  } else if (family == "poisson") {
    slooplot <- plot(plotx, ploty, pch = "+", xlab = xlabtext, ylab = ylabtext, pty = "s", asp = 1)
    abline(a = 0, b = 1, col = "grey")
    graphics::mtext(paste0("rmse mean", " ", "(", 1 - alpha, "% CI):", " ", round(df$RMSE, 2), " ", "(", round(rmse.LCI, 2), ";", 
                           round(rmse.HCI, 2), ")"), side = 3, adj = 0, cex = 0.8)
    if (mae == TRUE) {
      graphics::mtext(paste0(" ", "mae mean", " ", "(", 1 - alpha, "% CI):", " ", round(df$MAE, 2), " ", "(", round(mae.LCI, 2), 
                             ";", round(mae.HCI, 2), ")"), side = 3, adj = 0, cex = 0.8, line = 0.8)
    }
    if (ds == TRUE) {
      graphics::mtext(paste0("dawid-sebastiani score:", " ", " ", " ", " ", " ", round(df$DS, 2)), side = 3, adj = 0, cex = 0.8, 
                      line = 1.6)
    }
    cat(slooplot)
    
  } else if (family == "bernoulli") {
    
    yjitter <- jitter(plotx, factor = 0.01)
    slooplot <- plot(yjitter, ploty, pch = "+", xlab = xlabtext, ylab = ylabtext, xaxt = "n", pty = "s", asp = 1)
    # abline(a = 0, b = 1, col = 'grey')
    graphics::axis(1, at = c(0, 1), labels = c(0, 1))
    graphics::mtext(paste0("rmse mean", " ", "(", 1 - alpha, "% CI):", " ", round(df$RMSE, 2), " ", "(", round(rmse.LCI, 2), ";", 
                           round(rmse.HCI, 2), ")"), side = 3, adj = 0, cex = 0.8)
    if (mae == TRUE) {
      graphics::mtext(paste0(" ", "mae mean", " ", "(", 1 - alpha, "% CI):", " ", round(df$MAE, 2), " ", "(", round(mae.LCI, 2), 
                             ";", round(mae.HCI, 2), ")"), side = 3, adj = 0, cex = 0.8, line = 0.8)
    }
    if (ds == TRUE) {
      graphics::mtext(paste0("dawid-sebastiani score:", " ", " ", " ", " ", " ", round(df$DS, 2)), side = 3, adj = 0, cex = 0.8, 
                      line = 1.6)
    }
    cat(slooplot)
    
  } else if (family == "binomial") {
    
    slooplot <- plot(plotx, ploty, pch = "+", xlab = xlabtext, ylab = ylabtext, pty = "s", asp = 1)
    abline(a = 0, b = 1, col = "grey")
    graphics::axis(1, at = c(0, 1), labels = c(0, 1))
    graphics::mtext(paste0("rmse mean", " ", "(", 1 - alpha, "% CI):", " ", round(df$RMSE, 2), " ", "(", round(rmse.LCI, 2), ";", 
                           round(rmse.HCI, 2), ")"), side = 3, adj = 0, cex = 0.8)
    if (mae == TRUE) {
      graphics::mtext(paste0(" ", "mae mean", " ", "(", 1 - alpha, "% CI):", " ", round(df$MAE, 2), " ", "(", round(mae.LCI, 2), 
                             ";", round(mae.HCI, 2), ")"), side = 3, adj = 0, cex = 0.8, line = 0.8)
    }
    if (ds == TRUE) {
      graphics::mtext(paste0("dawid-sebastiani score:", " ", " ", " ", " ", " ", round(df$DS, 2)), side = 3, adj = 0, cex = 0.8, 
                      line = 1.6)
    }
    cat(slooplot)
    
  } else {
    slooplot <- plot(plotx, ploty, pch = "+", xlab = xlabtext, ylab = ylabtext, pty = "s", asp = 1)
    abline(a = 0, b = 1, col = "grey")
    graphics::mtext(paste0("rmse mean", " ", "(", 1 - alpha, "% CI):", " ", round(df$RMSE, 2), " ", "(", round(rmse.LCI, 2), ";", 
                           round(rmse.HCI, 2), ")"), side = 3, adj = 0, cex = 0.8)
    if (mae == TRUE) {
      graphics::mtext(paste0(" ", "mae mean", " ", "(", 1 - alpha, "% CI):", " ", round(df$MAE, 2), " ", "(", round(mae.LCI, 2), 
                             ";", round(mae.HCI, 2), ")"), side = 3, adj = 0, cex = 0.8, line = 0.8)
    }
    if (ds == TRUE) {
      graphics::mtext(paste0("dawid-sebastiani score:", " ", " ", " ", " ", " ", round(df$DS, 2)), side = 3, adj = 0, cex = 0.8, 
                      line = 1.6)
    }
    cat(slooplot)
  }
}

  