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




test_that('Other likelihoods', {
  
  skip_if_not_installed('INLA')
  
  library(INLA)
  
  # generate a dataframe and INLA output for the function
  set.seed(10)
  coords <- data.frame(long = c(rnorm(70), rnorm(30, 3)), lat = rnorm(100))
  x <- data.frame(x1 = rnorm(100), x2 = rnorm(100))# x1 no relat., x2 pos. relat.
  y <- as.numeric((x$x2 * 2 + rnorm(100)) < 0)
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
                   mesh = mesh, family = 'bernoulli',
                   mae = TRUE), 
    NA)


  expect_error(
  # run the function
    cv <- inlasloo(dataframe = dataframe, 
                   long = 'long', lat = 'lat',
                   y = 'y', ss = 3, 
                   ntrials = sample(1:2, length(y), replace = TRUE),
                   rad = rad, modform = modform,
                   mesh = mesh, family = 'binomial',
                   mae = TRUE), 
    NA)
  
})




test_that('xy names work', {
  
  skip_if_not_installed('INLA')
  
  library(INLA)
  
  # generate a dataframe and INLA output for the function
  set.seed(10)
  coords <- data.frame(x = c(rnorm(70), rnorm(30, 3)), y = rnorm(100))
  x <- data.frame(x1 = rnorm(100), x2 = rnorm(100))# x1 no relat., x2 pos. relat.
  y1 <- x$x2 * 2 + rnorm(100)
  dataf1 <- sp::SpatialPointsDataFrame(coords = coords, data = data.frame(y1 = y1, x))
  mesh <- INLA::inla.mesh.2d(loc = sp::coordinates(dataf1), max.edge = c(3, 3),cutoff = 1.3)
  spde <- INLA::inla.spde2.matern(mesh, alpha=2)#SPDE model is defined
  A <- INLA::inla.spde.make.A(mesh, loc = sp::coordinates(dataf1))#projector matrix
  dataframe <- data.frame(dataf1) #generate dataframe with response and covariate
  modform<-stats::as.formula(paste('y1 ~ -1+ x1 + x2 + y.intercept + f(spatial.field, model=spde)'))
  stk <- INLA::inla.stack(data = list(y1=dataframe$y1), 
                          A = list(A, 1),
                          effects = list(list(spatial.field=1:spde$n.spde),
                                         list(y.intercept = rep(1, length(dataframe$y1)),
                                              covariate = dataframe[c(-1)])), 
                          tag='est')
 
  # parameters for the SLOO process
  rad <- 0.6 # define the radius of the spatial buffer surrounding the removed point. Make sure it isn't bigger than 25% of the study area (Le Rest 2014)
  modform <- y1 ~ -1+ y.intercept + x1 + x2 + f(spatial.field, model=spde)
  alpha <- 0.05 # rmse and mae confidence intervals (1-alpha)
  
  expect_error(
  # run the function
    cv <- inlasloo(dataframe = dataframe, 
                   long = 'x', lat = 'y',
                   y = 'y1', ss = 3, 
                   rad = rad, modform = modform,
                   mesh = mesh, family = 'normal',
                   ds = TRUE,
                   mae = TRUE), 
    NA)
  
  
  # With multiple models
  modform2 <- y1 ~ -1+ y.intercept + x1 + x2
  
  expect_error(
    cv <- inlasloo(dataframe = dataframe, 
                   long = 'x', lat = 'y',
                   y = 'y1', ss = 3, 
                   rad = rad, modform = list(modform, modform2),
                   mesh = mesh, family = 'normal',
                   ds = TRUE,
                   mae = TRUE), 
    NA)
  
  
})



test_that('slooplot works.', {

  df<-data.frame(Residuals=runif(10, 0.0, 1.0),RMSE=runif(10, 0.0, 2.0),MAE=runif(10, 0.0, 2.0),
                 Observed_response= sample(c(0,1), replace=TRUE, size=10),
                 Predictions=runif(10, 0.0, 1.0), DS = runif(10, 0.0, 1.0))
  alpha = 0.05

  expect_error(
    slooplot1<-slooplot.fun(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='bernoulli',sqroot=FALSE)
    , NA)


  expect_error(
    slooplot1<-slooplot.fun(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='normal',sqroot=FALSE)
    , NA)

  expect_error(
    slooplot1<-slooplot.fun(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='gamma',sqroot=FALSE)
    , NA)

  expect_error(
    slooplot1<-slooplot.fun(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='normal',sqroot=TRUE)
    , NA)

  expect_error(
    slooplot1<-slooplot.fun(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='binomial',sqroot=FALSE, ntrial = rep(10, 10))
    , NA)

  expect_error(
    slooplot1<-slooplot.fun(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='poisson',sqroot=FALSE)
    , NA)


  expect_error(
    slooplot1<-slooplot.fun(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='poisson',sqroot=FALSE)
    , NA)





  # This other function that is basically a clone. Guess we should fix this.

  expect_error(
    slooplot1<-slooplot(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='bernoulli',sqroot=FALSE)
    , NA)


  expect_error(
    slooplot1<-slooplot(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='normal',sqroot=FALSE)
    , NA)

  expect_error(
    slooplot1<-slooplot(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='gamma',sqroot=FALSE)
    , NA)

  expect_error(
    slooplot1<-slooplot(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='normal',sqroot=TRUE)
    , NA)

  expect_error(
    slooplot1<-slooplot(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='binomial',sqroot=FALSE, ntrials = c(10, 10))
    , NA)

  expect_error(
    slooplot1<-slooplot(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='poisson',sqroot=FALSE)
    , NA)


  expect_error(
    slooplot1<-slooplot(df=df, alpha=0.05,mae=TRUE,ds=TRUE,family='poisson',sqroot=FALSE)
    , NA)






})
