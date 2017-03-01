context('Test stepINLA function')



test_that('Basic usage works', {
  
  
  library(raster)
  library(INLA)
  library(INLAutils)
  
  set.seed(10)
  
  coords <- data.frame(long = c(rnorm(70), rnorm(30, 3)), lat = rnorm(100))
  
  PA <- rep(c(0, 1), each = 50)
  
  ggplot(cbind(coords, PA), aes(long, lat, colour = factor(PA))) +
    geom_point()
  
  
  x <- data.frame(x1 = rnorm(100), # no relationship
                  x2 = c(rnorm(70), rnorm(30, 2))) # positive relationship
  
  
  ggplot(cbind(x, PA), aes(x1, PA)) + 
    geom_point() +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'))
  
  
  ggplot(cbind(x, PA), aes(x2, PA)) + 
    geom_point() +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'))
  
  
  
  
  # Create rasters of x1 and x2
  
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
  
  
  
  dataframe = sp::SpatialPointsDataFrame(coords = coords, data = data.frame(y = PA))
  invariant = "0 + Intercept"
  include = 1:raster::nlayers(predictors)
  cross_validation=FALSE
  spatial = TRUE
  num.threads=1
  meshvals=list(minME=max(raster::res(predictors)) * 10, maxME=max(raster::res(predictors)) * 100, co=0, minOS = -0.1, maxOS = -0.3)
  
  
  
  # Deal with names of response column.
  assert_that(ncol(dataframe) == 1)
  y <- 'y'
  names(dataframe) <- 'y'
  
  
  # Make meshvals be complete
  meshvals_complete = list(minME = max(raster::res(predictors)) * 10, 
                           maxME = max(raster::res(predictors)) * 100, 
                           co = 0, 
                           minOS = -0.1,
                           maxOS = -0.3)
  # Check that all values in meshvals arg are correct
  assert_that(all(names(meshvals) %in% names(meshvals_complete)))
  
  
  meshvals_complete[names(meshvals)] <- meshvals
  
  
  
  cv_folds <- 1
  
  # Empty list for all inla models
  models <- list()
  # Empty data frame for summaries
  model_res <- as.data.frame(matrix(NA, ncol = 4, nrow = cv_folds))
  names(model_res) <- c('replicate', 'AUC', 'WAIC', 'comp_time')
  # Empty list for meshes
  meshes <- list()
  
  
  
  
  ###add training data and testing data (NAs) together
  sss <- as.data.frame(raster::extract(predictors[[include]], dataf1))
  # If there's only 1 predictor, we end up with funny names.
  if(NCOL(sss) == 1) colnames(sss) <- names(predictors[[include]])
  
  sss <- cbind(sss, dataf1@data[, y])
  names(sss)[NCOL(sss)] <- y
  
  # Why?
  # sss <- sss[complete.cases(sss), ]
  dataf1@data <- sss
  
  # Organise the data
    ##make mesh
    mesh5 <- inla.mesh.2d(loc = sp::coordinates(dataf1), 
                          max.edge = c(meshvals_complete$minME, meshvals_complete$maxME), 
                          cutoff = meshvals_complete$co, 
                          offset = c(meshvals_complete$minOS, meshvals_complete$maxOS))
    
    ####The SPDE model is defined 
    spde <- inla.spde2.matern(mesh5, alpha=2)
    
    ###projector matrix for known
    A <- inla.spde.make.A(mesh5, loc = sp::coordinates(dataf1))
    
    ###make index for spatial field
    s.index<-inla.spde.make.index(name="spatial.field",n.spde=spde$n.spde)
    
    ##and the stack data is defined to include effects IMPORTANT they have same names as columns so the formula below will work
    #if(bin1==FALSE){dataf1$Presence<-dataf1$p; fam1=fam2}
    
    # Assumes invariant has `Intercept`
    stk.est <- inla.stack(data = list(y = dataf1$y),
                          A = list(A, 1), 
                          effects = list(c(s.index,list(Intercept = 1)),
                                         list(dataf1@data[, names(dataf1) != y, drop = FALSE])),
                          tag = 'est')
    
    ###projector matrix for known
    A.val <- inla.spde.make.A(mesh5, loc = sp::coordinates(dataf1))
    
    ##and the stack data is defined to include effects IMPORTANT they have same names as columns so the formula below will work
    # Todo What is this for!
    # Guessing it's supposed to be the test data? But not defined while taking into account the cross_validation groups above
    # 
    # stk.val <- inla.stack(data=list(y=NA),
    #                       A=list(A.val,1), 
    #                       effects=list(c(s.index, list(Intercept=1)), 
    #                                    list(dataf1@data[, names(dataf1) != y])), tag='val')
    # 
    # join.stack<-inla.stack(stk.est,stk.val)
    ntt <- rep(1, nrow(dataf1))
    
    # # What is predict1?
    # if(!predict1==TRUE){
    #   join.stack<-stk.est
    #   ntt=dataf1$trials
    # }
    
    
    stepINLA.out <- stepINLA(fam1 = "binomial", 
                         dataf1,
                         in_stack = stk.est,
                         spde = spde,
                         invariant = "0 + Intercept", # +  f(i, model=spde)",
                         direction = 'backwards',
                         include = 1:2,
                         y = y,
                         y2 = y,
                         powerl = 1,
                         inter = 1,
                         thresh = 2,
                         Ntrials = NULL,
                         num.threads = num.threads)
    
    
  
  
})



test_that('Spatial and nonspatial works steping through fixed effects only', {
  
})




test_that('Spatial and nonspatial works steping through fixed effects and spatial effect', {
  
})


test_that('Feature engineering works', {
  # Hopefully this will include refectored functions.
  
})
