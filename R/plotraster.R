
#' Plot a raster (either a raster layer or a inla projection matrix) with a shape file
#' 
#'@param raster Either a RasterLayer or a matrix 
#'  (e.g. from \code{\link[INLA]{inla.mesh.project}})
#'@param projector If raster is a matrix, a projector object 
#'  (from \code{\link[INLA]{inla.mesh.projector}}) is also needed.
#'@param spatialpolygons A \code{\link[sp]{SpatialPolygonsDataFrame}} object.
#'@param mesh Not yet implemented. And additional mesh object to plot as well.
#'@export
#'@importFrom reshape2 melt
#'@importFrom grDevices colorRampPalette
#'@importFrom RColorBrewer brewer.pal
#'@importFrom raster raster
#'@importFrom methods is
#'@examples
#' set.seed(2)
#' library(INLA)
#' 
#' # Create inla project
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
#' df <- data.frame(value=1, row.names='a')
#' my_shapefile <- SpatialPolygonsDataFrame(SPls, df)
#' 
#' 
#' ggplot_projection_shapefile(projection, projector, my_shapefile) + theme_minimal()
#' 
#' 
#' # Alternatively plot a raster
#' library(raster)
#' raster <- raster(projection)
#' extent(raster) <- c(range(projector$x), range(projector$y))
#' ggplot_projection_shapefile(raster, spatialpolygons = my_shapefile)
#' # p.s. I'm not sure why this is different to the above raster.
#' 




ggplot_projection_shapefile <- function(raster = NULL,
                                        projector = NULL, 
                                        spatialpolygons, 
                                        mesh = NULL){
  
  if(!is(raster, 'RasterLayer') & !is(raster, 'matrix')){
    stop('raster must be a raster layer or a matrix.')
  }
  
  # confirm that if raster, projector not needed. If projection, projector IS needed.
  if(is(raster, 'RasterLayer') & !is.null(projector)){
    warning('projector object not neede if plotting a raster')
  }
  if(is(raster, 'matrix') & is.null(projector)){
    stop('If raster is a matrix (i.e. from inla.mesh.project), the projector object is also needed.') 
  }
  
  
  # setup colour palette
  cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(6, 'Blues'))(1000)
  
  if(is(raster, 'matrix')){
    raster.df <- reshape2::melt(raster)
    raster.df$long <- projector$x[raster.df$Var1]
    raster.df$lat <- projector$y[raster.df$Var2]
  } else if(is(raster, 'RasterLayer')){
    raster.df <- data.frame(raster::rasterToPoints(raster))
    names(raster.df) <- c('long', 'lat', 'value')
    
  }
    
  shape.df <- ggplot2::fortify(spatialpolygons)   
  
  ggplot() + 
    ggplot2::geom_raster(data = raster.df, 
                         aes_string('long', 'lat', fill = 'value')) +
    ggplot2::geom_path(data = shape.df, 
                       aes_string('long', 'lat', group = 'group'), 
                       colour = 'white')
  
}



