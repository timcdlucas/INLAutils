

#' An autoplot (ggplot2) method for INLA mesh objects.
#'
#'@param object An inla.mesh object
#'@param col Colour for data points
#'@param lwd Line width
#'@param linecol The colour for the mesh edges
#'@param size size Size of data points
#'@param ... Other arguments passed to specific methods
#'@export
#'@examples
#' library(INLA)
#' m = 100
#' points = matrix(runif(m*2),m,2)
#' mesh = inla.mesh.create.helper(
#'   points=points,
#'   cutoff=0.05,
#'   offset=c(0.1,0.4),
#'   max.edge=c(0.05,0.5) )
#' 
#' autoplot(mesh)
#' p <- autoplot(mesh)
#' 
#' # As a ggplot2 object, the plot can be altered.
#' p + theme_dark()
#' 
#' # The size and colour of all objects can be controlled.
#' #   The order of the values is 
#' #   1: data points
#' #   2: edges
#' #   3: outer domain
#' #   4: inner domain
#' #   5: non data vertices
#' p + scale_colour_manual(values = c('red', 'grey', 'darkblue', 'steelblue', 'yellow'))
#' 
#' # The name of the variable that defines the different objects is 'type'
#' p + facet_grid(. ~ type)
#' 
#' # Plot projections with ggalt
#' \dontrun{
#' library(ggalt)
#' p + ggalt::coord_proj("+proj=wintri")
#' }

autoplot.inla.mesh <- function(object, ..., col = 'blue', lwd = 0.5, linecol = 'darkgrey', size = 1.2){
  
  mesh <- object
  # extract point data
  d <- data.frame(x = mesh$loc[, 1], y = mesh$loc[, 2], type = 'evertices')
  levels(d$type) <- c('evertices', 'adata')
  d[mesh$idx$loc, 'type'] <- 'adata'
  # extract lines data. 
  # mesh$graph$tv column 1, 2, 3 are points in triangles.
  # Therefore need 1 to 2, 2 to 3 and 3 to 1.
  idx = rbind(mesh$graph$tv[, 1:2, drop = FALSE], 
              mesh$graph$tv[, 2:3, drop = FALSE], 
              mesh$graph$tv[, c(3, 1), drop = FALSE])
  segments <- data.frame(mesh$loc[idx[, 1], 1:2], mesh$loc[idx[, 2], 1:2], type = 'bsegments')

  innerouter <- rbind(
                  data.frame(mesh$loc[mesh$segm$bnd$idx[, 1], 1:2],
                             mesh$loc[mesh$segm$bnd$idx[, 2], 1:2],
                             type = 'cbinding'),
                  data.frame(mesh$loc[mesh$segm$int$idx[, 1], 1:2],
                             mesh$loc[mesh$segm$int$idx[, 2], 1:2],
                             type = 'dinternal'))
  names(segments) <- c('x1', 'y1', 'x2', 'y2', 'type')
  names(innerouter) <- c('x1', 'y1', 'x2', 'y2', 'type')

  segments <- rbind(segments, innerouter)

  
  p <- ggplot2::ggplot(data = d, 
                      ggplot2::aes_string('x', 'y', 
                          colour = 'type', 
                          size = 'type')) +
                 ggplot2::geom_segment(data = segments, 
                   ggplot2::aes_string(x = 'x1', y = 'y1', xend = 'x2', yend = 'y2')) +
                 ggplot2::geom_point() +
                 ggplot2::theme_minimal() +
                 ggplot2::theme(legend.position = 'none')
#stroke
  p <- p +
         ggplot2::scale_colour_manual(values = c(col, linecol, 'black', 'black', 'black')) +
         ggplot2::scale_size_manual(values = c(size, lwd, 1.3, 1.3, 0)) 
  p
}


