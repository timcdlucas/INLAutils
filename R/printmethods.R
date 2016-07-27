
#'These are print methods for a number of inla objects that do not have print methods in the INLA package
#'
#'@param x Object to be printed
#'@name print.inla.data.stack
#'@rdname print.inla.data.stack
#'@export
print.inla.data.stack <- function(x){
  cat('inla.data.stack object\n')
  cat(paste0('Data: ', x$data$nrow, ' observations. Variables: ', x$data$names))
  cat(paste0('. Effects: ', paste(unlist(x$effects$names), collapse = ', '), '.\n'))
  cat('\n')
  cat(paste('Containing data from stacks:', paste(names(x$data$index), collapse = ', '), '\n'))
  

}





#'@name print.inla.mesh
#'@rdname print.inla.data.stack
#'@export
print.inla.mesh <- function(x){
  print(summary(x))
}



#'@name print.inla.mesh.projector
#'@rdname print.inla.data.stack
#'@export
print.inla.mesh.projector <- function(x){
  cat('inla.mesh.projector object\n\n')
  cat(paste0('xdim: ', length(x$x), '  ydim: ', length(x$y), '\n'))
  cat(paste0('x range: ', min(x$x), ' to ', max(x$x), '\n'))
  cat(paste0('y range: ', min(x$y), ' to ', max(x$y), '\n'))
}
