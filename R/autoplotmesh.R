

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
autoplot.inla.mesh <- function(mesh, col = 'blue', lwd = 0.5, size = 0){
  
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

  
  p <- ggplot(data = d, 
              aes(x, y, 
                  colour = type, 
                  size = type, 
                  shape = type, 
                  alpha = type, 
                  linetype = type, 
                  fill = type)) +
         geom_segment(data = segments, 
           aes(x = x1, y = y1, xend = x2, yend = y2)) +
         geom_point() +
         theme_minimal() +
         theme(legend.position = 'none')
#stroke
  p <- p +
         scale_colour_manual(values = c(col, 'darkgrey', 'black', 'black', 'black')) +
         scale_linetype_manual(values = rep(1, 5)) +
         scale_size_manual(values = c(size, lwd, 1.3, 1.3, 0)) +
         scale_shape_manual(values = rep(16, 5)) +
         scale_alpha_manual(values = rep(1, 5)) +
         scale_fill_manual(values = rep('red', 5))

  p
}


