
#'These are print methods for a number of inla objects that do not have print methods in the INLA package
#'
#'@param x Object to be printed
#'@param ... Further arguments passed to or from other methods.
#'@name print.inla.data.stack
#'@rdname print.inla.data.stack
#'@export
print.inla.data.stack <- function(x, ...){
  cat('inla.data.stack object\n')
  cat(paste0('Data: ', x$data$nrow, ' observations. Variables: ', x$data$names))
  cat(paste0('. Effects: ', paste(unlist(x$effects$names), collapse = ', '), '.\n'))
  cat('\n')
  cat(paste('Containing data from stacks:', paste(names(x$data$index), collapse = ', '), '\n'))
  

}





#'@name print.inla.mesh
#'@rdname print.inla.data.stack
#'@export
print.inla.mesh <- function(x, ...){
  print(summary(x))
}



#'@name print.inla.mesh.projector
#'@rdname print.inla.data.stack
#'@export
print.inla.mesh.projector <- function(x, ...){
  cat('inla.mesh.projector object\n\n')
  cat(paste0('xdim: ', length(x$x), '  ydim: ', length(x$y), '\n'))
  cat(paste0('x range: ', min(x$x), ' to ', max(x$x), '\n'))
  cat(paste0('y range: ', min(x$y), ' to ', max(x$y), '\n'))
}


#'@name print.inla.mesh.segment
#'@rdname print.inla.data.stack
#'@export
print.inla.mesh.segment <- function(x, ...){
  cat(paste('inla.mesh.segment object with', nrow(x$loc), 'points.\n'))
  if(is.null(x$grp)){ 
    grplength <- 1
  } else {
    grplength <- length(x$grp)
  }
  cat(paste('Number of groups:', grplength, '\n'))
  extent <- c(range(x$loc[, 1]), range(x$loc[, 2]))
  cat(paste('xlim:   ', sprintf('%.3f', extent[1]), sprintf('%.3f', extent[2]), '\n'))
  cat(paste('ylim:   ', sprintf('%.3f', extent[3]), sprintf('%.3f', extent[4]), '\n'))
}


