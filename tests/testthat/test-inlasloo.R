context('Test inlasloo')


test_that('Basic sloo usage works', {
  
  skip_if_not_installed('INLA')
  
  library(INLA)
  
  # generate a dataframe and INLA output for the function
  set.seed(10)
  coords <- data.frame(long = c(rnorm(70), rnorm(30, 3)), lat = rnorm(100))
  x <- data.frame(x1 = rnorm(100), x2 = rnorm(100))# x1 no relat., x2 pos. relat.
  y <- x$x2 * 2 + rnorm(100)
  dataf1 <- sp::SpatialPointsDataFrame(coords = coords, data = data.frame(y = y, x))
  mesh <- INLA::inla.mesh.2d(loc = sp::coordinates(dataf1), max.edge = c(3, 3),cutoff = 1.3)
  spde <- INLA::inla.spde2.matern(mesh, alpha=2)#SPDE model is defined
  A <- INLA::inla.spde.make.A(mesh, loc = sp::coordinates(dataf1))#projector matrix
  dataframe <- data.frame(dataf1) #generate dataframe with response and covariate
  modform<-stats::as.formula(paste('y ~ -1+ x1 + x2 + y.intercept + f(spatial.field, model=spde)'))
  stk <- INLA::inla.stack(data = list(y=dataframe$y), 
                          A = list(A, 1),
                          effects = list(list(spatial.field=1:spde$n.spde),
                                         list(y.intercept = rep(1, length(dataframe$y)),
                                              covariate = dataframe[c(-1)])), 
                          tag='est')
 
  # parameters for the SLOO process
  rad <- 0.6 # define the radius of the spatial buffer surrounding the removed point. Make sure it isn't bigger than 25% of the study area (Le Rest 2014)
  modform <- y ~ -1+ y.intercept + x1 + x2 + f(spatial.field, model=spde)
  alpha <- 0.05 # rmse and mae confidence intervals (1-alpha)
  
  expect_error(
  # run the function
    cv <- inlasloo(dataframe = dataframe, 
                   long = 'long', lat = 'lat',
                   y = 'y', ss = 3, 
                   rad = rad, modform = modform,
                   mesh = mesh, family = 'normal',
                   mae = TRUE), 
    NA)
  
  
  # With multiple models
  modform2 <- y ~ -1+ y.intercept + x1 + x2
  
  expect_error(
    cv <- inlasloo(dataframe = dataframe, 
                   long = 'long', lat = 'lat',
                   y = 'y', ss = 3, 
                   rad = rad, modform = list(modform, modform2),
                   mesh = mesh, family = 'normal',
                   mae = TRUE), 
    NA)
  
  
})


test_that('x y column names do not break things.', {
  
  skip_if_not_installed('INLA')
  
  library(INLA)
  
  library(sp)
  data(meuse)

  coords <- meuse[, c('x', 'y')] %>% scale
  dataf1 <- sp::SpatialPointsDataFrame(coords = coords, data = meuse[, -c(1:2)])
  
  mesh <- inla.mesh.2d(loc = sp::coordinates(dataf1), max.edge = c(0.2, 0.5), cutoff = 0.1)
  spde <- inla.spde2.matern(mesh, alpha=2) # SPDE model is defined
  A <- inla.spde.make.A(mesh, loc = sp::coordinates(dataf1)) # projector matrix
  dataframe <- data.frame(dataf1) # generate dataframe with response and covariate
  modform <- cadmium ~ -1 + y.intercept + ffreq + om + soil + lime + f(spatial.field, model = spde)
  modform2 <- cadmium ~ -1 + y.intercept + ffreq + om + soil + lime
  
  # make index for spatial field
  s.index <- inla.spde.make.index(name="spatial.field", n.spde=spde$n.spde)
  
  ## Prepare the data
  stk <- inla.stack(data=list(cadmium=dataframe$cadmium),
                    A=list(A,1), 
                    effects=list(c(s.index,list(y.intercept=1)),
                                 list(dataframe[, 7:10])),
                    tag='est')
  
  # parameters for the SLOO process
  ss <- 3 # sample size to process (number of SLOO runs)
  # define the radius of the spatial buffer surrounding the removed point. 
  rad <- 0.5
  # Make sure it isn't bigger than 25% of the study area (see Le Rest et al.(2014))
  alpha <- 0.05 # rmse and mae confidence intervals (1-alpha)
  
  expect_error(
    # run the function to compare both models
    cv <- inlasloo(dataframe = dataframe, 
                   long = 'x', lat = 'y',
                   y = 'cadmium', ss = ss, 
                   rad = rad, 
                   modform = list(modform, modform2),
                   mesh = mesh, family = 'normal',
                   int.strategy = 'inla',
                   mae = TRUE)
    , NA)
  
})
