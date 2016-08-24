
#'These are plot methods for a number of inla objects that do not have plot methods in the INLA package
#'
#'@param x Object to be printed
#'@param y not used
#'@param ... other methods passed to plot.data.frame
#'@name plot.inla.mesh.segment
#'@export

plot.inla.mesh.segment <- function(x, y, ...){
  plot(x$loc, type = 'l', ...)
}
