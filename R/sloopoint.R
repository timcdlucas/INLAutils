#' A function to plot the left-out points during the spatial leave-one-out cross-validation in R using INLA.
#' 
#' This function plots the left-out point(s) of a spatial leave-one-out cross-validation (SLOO-CV) of one or several models running on INLA.
#' @param points = cbind of longitude and latitude of the observed points (full sample)
#' @param test = cbind of longitude and latitude of the test point(s)
#' @param rad = Numeric value giving the radius for the spatial buffer around left-out point's location
#' 
#' @name sloopoint
#' 
#' @examples 
#' \dontrun{
#'  # sloopoint function
#'  dataframe<-data.frame(long=runif(100, -40.0, 40.0),lat=runif(100, -40.0, 40.0))
#'  test<-dataframe[1:10,]
#'  rad = 1
#'  
#'  # run the function
#'  sloopoint(points = cbind(dataframe$long, dataframe$lat), test = cbind(test$long, test$lat), 
#'  rad = rad)
#'  }

sloopoint <- function(points = points, test = test, rad = rad) {
    pchname <- as.character(seq(1:nrow(test)))
    plot(points, pch = 3, xlab = "longitude", ylab = "latitude")
    points(test, pch = 19, col = "red", cex = 3.5, bg = "black")
    graphics::text(test, labels = pchname, cex = 0.75, col = "white")
    graphics::symbols(x = test[, 1], y = test[, 2], circles = rep(rad, length(test[, 1])), fg = "red", add = TRUE, inches = FALSE)
    graphics::legend("topright", legend = c("Observed locations", paste0("Predicted location(s) and removed disc(s)\nwith iteration number at centroid:{1,...,", 
        length(test[, 1]), "}")), pch = c(3, 19), col = c("black", "red"), cex = 0.75, text.col = c("black", "red"))
}
