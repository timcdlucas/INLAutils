

#' An autoplot (ggplot2) method for INLA mesh objects.
#'
#'@param mesh An inla.mesh object
#'@export
#'@examples
#' m = 100
#' points = matrix(runif(m*2),m,2)
#' mesh = inla.mesh.create.helper(
#'   points=points,
#'   cutoff=0.05,
#'   offset=c(0.1,0.4),
#'   max.edge=c(0.05,0.5) )
#' 
#' autoplot(mesh)
#' 
autoplot.inla.mesh <- function(mesh){
  
  d <- data.frame(x = mesh$loc[, 1], y = mesh$loc[, 2])

  idx = rbind(mesh$graph$tv[, 1:2, drop = FALSE], 
              mesh$graph$tv[, 2:3, drop = FALSE], 
              mesh$graph$tv[, c(3, 1), drop = FALSE])
  segments <- data.frame(cbind(mesh$loc[idx[, 1], 1:2], mesh$loc[idx[, 2], 1:2]))
  names(segments) <- c('x1', 'y1', 'x2', 'y2')

  
  ggplot(d, aes(x, y)) +
    geom_point() +
    geom_segment(data = segments, aes(x = x1, y = y1, xend = x2, yend = y2))



        
}


#        if (draw.segments) {
#            if (!is.null(mesh$segm$bnd)) 
#                lines(mesh$segm$bnd, mesh$loc, lwd = lwd + 1, 
#                  ...)
#            if (!is.null(mesh$segm$int)) 
#                lines(mesh$segm$int, mesh$loc, lwd = lwd + 1, 
#                  ...)
#        }

