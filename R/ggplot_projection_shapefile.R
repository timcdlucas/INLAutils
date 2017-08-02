
#' Plot a raster (either a raster layer or a inla projection matrix) with a shape file
#' 
#'@param raster Either a RasterLayer or a matrix 
#'  (e.g. from \code{\link[INLA]{inla.mesh.project}})
#'@param projector If raster is a matrix, a projector object 
#'  (from \code{\link[INLA]{inla.mesh.projector}}) is also needed.
#'@param spatialpolygons A \code{\link[sp]{SpatialPolygonsDataFrame}} object.
#'@param mesh An inla.mesh object to be plotted. Still rough.
#'@param shapecol Colour for the shape file outlines.
#'@export
#'@importFrom reshape2 melt
#'@importFrom grDevices colorRampPalette
#'@importFrom RColorBrewer brewer.pal
#'@importFrom raster raster
#'@importFrom methods is
#'@examples
#'\dontrun{
#' set.seed(2)
#' library(INLA)
#' 
#' # Create inla projector
#' n <- 20
#' loc <- matrix(runif(n*2), n, 2)
#' mesh <- inla.mesh.create(loc, refine=list(max.edge=0.05))
#' projector <- inla.mesh.projector(mesh)
#' 
#' field <- cos(mesh$loc[,1]*2*pi*3)*sin(mesh$loc[,2]*2*pi*7)
#' projection <- inla.mesh.project(projector, field)
#' 
#' # And the shape file
#' crds <- loc[chull(loc), ]
#' SPls <- SpatialPolygons(list(Polygons(list(Polygon(crds)), ID = 'a')))
#' 
#' ggplot_projection_shapefile(projection, projector, SPls) + theme_minimal()
#' 
#' 
#' # Alternatively plot a raster
#' library(raster)
#' raster <- raster(projection)
#' extent(raster) <- c(range(projector$x), range(projector$y))
#' ggplot_projection_shapefile(raster, spatialpolygons = SPls)
#' }




ggplot_projection_shapefile <- function(raster = NULL,
                                        projector = NULL, 
                                        spatialpolygons = NULL, 
                                        mesh = NULL,
                                        shapecol = 'white'){
  
  if(!is(raster, 'RasterLayer') & !is(raster, 'matrix') & !is.null(raster)){
    stop('raster must be a raster layer or a matrix.')
  }
  
  # confirm that if raster, projector not needed. If projection, projector IS needed.
  if(is(raster, 'RasterLayer') & !is.null(projector)){
    warning('projector object not neede if plotting a raster')
  }
  if(is(raster, 'matrix') & is.null(projector)){
    stop('If raster is a matrix (i.e. from inla.mesh.project), the projector object is also needed.') 
  }
  
  if(all(is.null(raster), is.null(spatialpolygons), is.null(mesh))){
    stop('At least one of raster, spatialpolygons or mesh needed')
  }
  
  # Check mesh is correct class
  if(!is.null(mesh) & !is(mesh, 'inla.mesh')){
    stop('mesh musth either be and INLA mesh or NULL')
  }
  
  # setup colour palette
  cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(6, 'Blues'))(1000)
  

  

  plot <- ggplot()
  if(!is.null(raster)){
      
    if(is(raster, 'matrix')){ # Convert matrix + projector to raster.
      raster <- raster::raster(t(raster)[nrow(raster) : 1, ])
      raster::extent(raster) <- c(range(projector$x), range(projector$y))
    } 
    
    raster.df <- ggplot2::fortify(raster)
    names(raster.df) <- c('long', 'lat', 'value')
    
    plot <- plot + 
              ggplot2::geom_raster(data = raster.df, 
                                   aes_string('long', 'lat', fill = 'value'))
  }
  
  if(!is.null(spatialpolygons)){
    shape.df <- ggplot2::fortify(spatialpolygons)   
    
    plot <- plot + 
              ggplot2::geom_path(data = shape.df, 
                                aes_string('long', 'lat', group = 'group'), 
                                colour = shapecol)
  }   

  
  if(!is.null(mesh)){
    d <- data.frame(x = mesh$loc[, 1], y = mesh$loc[, 2], type = 'evertices')
    levels(d$type) <- c('evertices', 'adata')
    d[mesh$idx$loc, 'type'] <- 'adata'
    idx = rbind(mesh$graph$tv[, 1:2, drop = FALSE], 
                mesh$graph$tv[, 2:3, drop = FALSE], 
                mesh$graph$tv[, c(3, 1), drop = FALSE])
    segments <- data.frame(mesh$loc[idx[, 1], 1:2], mesh$loc[idx[, 2], 1:2], type = 'bsegments')
    
    innerouter <- data.frame(mesh$loc[mesh$segm$bnd$idx[, 1], 1:2],
                             mesh$loc[mesh$segm$bnd$idx[, 2], 1:2],
                             type = 'cbinding', stringsAsFactors = FALSE)
    if(nrow(mesh$segm$int$idx) > 0){
      innerouter <- rbind(innerouter,
                          data.frame(mesh$loc[mesh$segm$int$idx[, 1], 1:2],
                                     mesh$loc[mesh$segm$int$idx[, 2], 1:2],
                                     type = 'dinternal'))
    } else {
      #innerouter <- rbind(innerouter,
      #                    NA)
      #innerouter[nrow(innerouter), 5] <- 'dinternal'
      innerouter$type = factor(innerouter$type, levels = c('dinternal', 'cbinding'))
    }
    
    names(segments) <- c('x1', 'y1', 'x2', 'y2', 'type')
    names(innerouter) <- c('x1', 'y1', 'x2', 'y2', 'type')
    
    segments <- rbind(segments, innerouter)
    
    # Now add to plot
    plot <- plot +
      ggplot2::geom_segment(data = segments, 
                            ggplot2::aes_string(x = 'x1', y = 'y1', xend = 'x2', yend = 'y2')) +
      ggplot2::geom_point(data = d, 
                          ggplot2::aes_string('x', 'y', 
                                              colour = 'type', 
                                              size = 'type')) +
      ggplot2::theme(legend.position = 'none') + 
      ggplot2::scale_colour_manual(values = c('red', 'red', 'black', 'black', 'black')) +
      ggplot2::scale_size_manual(values = c(1, 1.5, 1.3, 1.3, 0)) 
    
  }

  plot
  
  
}



