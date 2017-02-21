context('Tests for INLA sdm functions.')




test_that('All INLAsdm options return at least models.', {

  set.seed(3)
  coords <- data.frame(long = c(rnorm(70), rnorm(30, 3)), lat = rnorm(100))
  
  PA <- rep(c(0, 1), each = 50)
  
  
  x <- data.frame(x1 = rnorm(100), # no relationship
                  x2 = c(rnorm(70), rnorm(30, 2))) # positive relationship
  
  
  ggplot(cbind(x, PA), aes(x1, PA)) + 
    geom_point() +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'))
  
  
  ggplot(cbind(x, PA), aes(x2, PA)) + 
    geom_point() +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'))
  
  
  
  
  res <- 50
  
  xrange <- range(coords$long)
  xrange <- c(floor(xrange[1]), ceiling(xrange[2]))
  yrange <- range(coords$lat)
  yrange <- c(floor(yrange[1]), ceiling(yrange[2]))
  
  xcells <- res * (xrange[2] - xrange[1])
  ycells <- res * (yrange[2] - yrange[1])
  
  
  raster <- raster::raster(matrix(NA, ncol = ycells, nrow = xcells), xmn = xrange[1], xmx = xrange[2], ymn = yrange[1], ymx = yrange[2])
  
  x1 <- raster::rasterize(coords, raster, x$x1)
  x1[is.na(x1)] <- rnorm(sum(is.na(raster::getValues(x1))))
  
  x2 <- raster::rasterize(coords, raster, x$x2)
  x2[is.na(x2)] <- rnorm(sum(is.na(raster::getValues(x2))))
  
  predictors <- raster::stack(x1, x2)
  
  
  # non spatial
  
    # CV
  
  
  
  # spatial
  
    # CV
  
  # Step non spatial
  
    # CV
  
  # step spatial
    
    # CV

})
