
#' Parallel extraction of means of rasters by shape file.
#' 
#' Parallelisation is performed across rasters, not shapes. 
#' So this function is only useful if you are extracting 
#' data from many raster layers.
#' As the overhead for parallel computation in windows is high
#' it only makes sense to parallelise in this way.
#' To run, you must first register a cluster (see examples).
#'
#' 
#' @param raster A raster brick or stack
#' @param shape A shape file
#' @param fun The function used to aggregate the pixel data. If NULL, raw pixel data is returned.
#' @param ... Other arguments to raster::extract (see \code{\link[raster]{extract]})
#'
#' @export
#' @seealso \code{\link[raster]{extract]}
#' @examples 
#' 
#' # Create raster
#' r <- raster(ncol=36, nrow=18)
#' r[] <- 1:ncell(r)
#' 
#' # Create polygon
#' cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
#' cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
#' polys <- spPolygons(cds1, cds2)
#' 
#' #plot(r)
#' #plot(polys, add=TRUE)
#' 
#' # Standard raster::extract
#' v <- extract(r, polys)
#' v <- extract(r, polys, fun = mean, df = TRUE)
#' 
#' # Parallel extract
#' #  Register the cluster first (use doMC on linux)
#' library(doSNOW)  
#' cl <- makeCluster(2) #change the 2 to your number of CPU cores  
#' registerDoSNOW(cl)  

#' v2 <- parallelExtract(r, polys, fun = mean)
#' stopCluster(cl)  

parallelExtract <- function(raster, shape, fun = mean, ...){
  
  
  
  values <- foreach(i = 1:nlayers(raster)) %dopar% {  
    raster::extract(raster[[i]], shape, fun = fun, na.rm = TRUE, ...)
  } 
  if(!is.null(fun)){
    df <- data.frame(do.call(cbind, values))
    names(df) <- names(raster)
    return(df)
  } else {
    return(values)
  }

}



#extract(raster, shape, df = TRUE, fun = mean, na.rm = TRUE)


