context('Tests for INLA sdm functions')



set.seed(6)

# Create locations, presence absence points and covariates with spatial and environmental relationships
coords <- data.frame(long = c(rnorm(70), rnorm(30, 3)), lat = rnorm(100))
PA <- rep(c(0, 1), each = 50)
x <- data.frame(x1 = rnorm(100), # no relationship
                x2 = c(rnorm(70), rnorm(30, 5))) # positive relationship

# Have a look
ggplot(cbind(x, PA), aes(x1, PA)) + 
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'))


ggplot(cbind(x, PA), aes(x2, PA)) + 
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'))


ggplot(cbind(coords, PA), aes(long, lat, colour = PA)) + geom_point()


# Set raster resolution
res <- 50

# Create raster limits
xrange <- range(coords$long)
xrange <- c(floor(xrange[1]), ceiling(xrange[2]))
yrange <- range(coords$lat)
yrange <- c(floor(yrange[1]), ceiling(yrange[2]))

# Calculate number of cells
xcells <- res * (xrange[2] - xrange[1])
ycells <- res * (yrange[2] - yrange[1])

# Create an empty raster of correct dims
#print(c(ycells, xcells, xrange, yrange))
suppressWarnings(
  raster <- raster::raster(matrix(NA, ncol = ycells, nrow = xcells), xmn = xrange[1], xmx = xrange[2], ymn = yrange[1], ymx = yrange[2])
)
# Add dataframe data to rasters, then fill gaps with random data.
x1 <- raster::rasterize(coords, raster, x$x1)
x1[is.na(x1)] <- rnorm(sum(is.na(raster::getValues(x1))))

x2 <- raster::rasterize(coords, raster, x$x2)
x2[is.na(x2)] <- rnorm(sum(is.na(raster::getValues(x2))))

# Stack rasters
predictors <- raster::stack(x1, x2)

# Pull together coordinates and PA data into SpatialPointsDataFrame
dataframe = sp::SpatialPointsDataFrame(coords = coords, data = data.frame(y = PA))


test_that('All basic INLAsdm options return at least reasonable', {

  
  # non spatial
  model_nospace_nocv <- inlaSDM(dataframe, predictors, spatial = FALSE, cross_validation = FALSE)

  expect_true(class(model_nospace_nocv) == 'inlaSDM')
  expect_true(class(model_nospace_nocv[[2]][[1]]) == 'inla')
  expect_true(length(model_nospace_nocv[[2]][[1]]$summary.random) == 0)
  # Check layer two is positive (and 0 is not in 95% CI)
  expect_true(all(model_nospace_nocv[[2]][[1]]$summary.fixed['layer.2', c('mean', '0.025quant')] > 0))

    # CV
    if(FALSE){ # add back in later
      model_nospace_nocv <- inlaSDM(dataframe, predictors, spatial = FALSE, cross_validation = TRUE, cv_folds = 2)
  
      expect_true(class(model_nospace_nocv) == 'inlaSDM')
      expect_true(class(model_nospace_nocv[[2]][[1]]) == 'inla')
      expect_true(length(model_nospace_nocv[[2]][[1]]$summary.random) == 0)
      # Check layer two is positive (and 0 is not in 95% CI)
      expect_true(all(model_nospace_nocv[[2]][[1]]$summary.fixed['layer.2', c('mean', '0.025quant')] > 0))
    }
  
  # spatial
  
  model_space_nocv <- inlaSDM(dataframe, predictors, spatial = TRUE, cross_validation = FALSE, meshvals = list(co = 1.2))

  expect_true(class(model_space_nocv) == 'inlaSDM')
  expect_true(class(model_space_nocv[[2]][[1]]) == 'inla')
  expect_true(length(model_space_nocv[[2]][[1]]$summary.random) != 0)
  # Check layer two is positive (and 0 is not in 95% CI)
  expect_true(all(model_space_nocv[[2]][[1]]$summary.fixed['layer.2', c('mean', '0.025quant')] > 0))

  
    # CV
  
  # Step non spatial
  
    # CV
  
  # step spatial
    
    # CV

  

  
})


test_that('INLAsdm invariant works', { 
  
})



test_that('meshvals invariant works', { 
  # Complete example
  
  
  
  # incomplete example
  
  
})



test_that('INLAsdm include works', { 

  # Check we remove some columns  
  model_nospace_nocv <- inlaSDM(dataframe, predictors, spatial = FALSE, cross_validation = FALSE, include = 1)

  expect_true(class(model_nospace_nocv) == 'inlaSDM')
  expect_true(class(model_nospace_nocv[[2]][[1]]) == 'inla')
  expect_true(row.names(model_nospace_nocv[[2]][[1]]$summary.fixed)[2] == 'layer.1')
  expect_true(nrow(model_nospace_nocv[[2]][[1]]$summary.fixed) == 2)
  
  # Check we get the correct columns
  model_nospace_nocv <- inlaSDM(dataframe, predictors, spatial = FALSE, cross_validation = FALSE, include = 2)

  expect_true(class(model_nospace_nocv) == 'inlaSDM')
  expect_true(class(model_nospace_nocv[[2]][[1]]) == 'inla')
  expect_true(row.names(model_nospace_nocv[[2]][[1]]$summary.fixed)[2] == 'layer.2')
  expect_true(nrow(model_nospace_nocv[[2]][[1]]$summary.fixed) == 2)
  
  
  # Check this works in spatial as well. There's some code duplication.
  
  
  model_nospace_nocv <- inlaSDM(dataframe, predictors, spatial = TRUE, cross_validation = FALSE, include = 2, meshvals = list(co = 1.2))

  expect_true(class(model_nospace_nocv) == 'inlaSDM')
  expect_true(class(model_nospace_nocv[[2]][[1]]) == 'inla')
  expect_true(row.names(model_nospace_nocv[[2]][[1]]$summary.fixed)[2] == 'layer.2')
  expect_true(nrow(model_nospace_nocv[[2]][[1]]$summary.fixed) == 2)


})


# removed this argument
# 
# test_that('INLAsdm y value works', { 
#   
#   set.seed(3)
#   
#   # Create locations, presence absence points and covariates
#   coords <- data.frame(long = c(rnorm(70), rnorm(30, 3)), lat = rnorm(100))
#   PA <- rep(c(0, 1), each = 50)
#   x <- data.frame(x1 = rnorm(100), # no relationship
#                   x2 = c(rnorm(70), rnorm(30, 2))) # positive relationship
#   
#   
#   
#   # Set raster resolution
#   res <- 50
#   
#   # Create raster limits
#   xrange <- range(coords$long)
#   xrange <- c(floor(xrange[1]), ceiling(xrange[2]))
#   yrange <- range(coords$lat)
#   yrange <- c(floor(yrange[1]), ceiling(yrange[2]))
#   
#   # Calculate number of cells
#   xcells <- res * (xrange[2] - xrange[1])
#   ycells <- res * (yrange[2] - yrange[1])
#   
#   # Create an empty raster of correct dims
#   raster <- raster::raster(matrix(NA, ncol = ycells, nrow = xcells), xmn = xrange[1], xmx = xrange[2], ymn = yrange[1], ymx = yrange[2])
#   
#   # Add dataframe data to rasters, then fill gaps with random data.
#   x1 <- raster::rasterize(coords, raster, x$x1)
#   x1[is.na(x1)] <- rnorm(sum(is.na(raster::getValues(x1))))
#   
#   x2 <- raster::rasterize(coords, raster, x$x2)
#   x2[is.na(x2)] <- rnorm(sum(is.na(raster::getValues(x2))))
#   
#   # Stack rasters
#   predictors <- raster::stack(x1, x2)
#   
#   # Pull together coordinates and PA data into SpatialPointsDataFrame
#   dataframe = sp::SpatialPointsDataFrame(coords = coords, data = data.frame(PA = PA))
#   
#   
#   # non spatial
#   model_nospace_nocv <- inlaSDM(dataframe, predictors, spatial = FALSE, cross_validation = FALSE, y = 'PA')
#   
#   expect_true(class(model_nospace_nocv) == 'inlaSDM')
#   expect_true(class(model_nospace_nocv[[2]][[1]]) == 'inla')
#   expect_true(length(model_nospace_nocv[[2]][[1]]$summary.random) == 0)
#   # Check layer two is positive (and 0 is not in 95% CI)
#   expect_true(all(model_nospace_nocv[[2]][[1]]$summary.fixed['layer.2', c('mean', '0.025quant')] > 0))
# 
# })


