context('Test INLAstep function')



test_that('Basic usage works', {
  

  set.seed(10)
  
  coords <- data.frame(long = c(rnorm(70), rnorm(30, 3)), lat = rnorm(100))
  
  PA <- rep(c(0, 1), each = 50)
  
  if(FALSE){
    ggplot(cbind(coords, PA), aes(long, lat, colour = factor(PA))) +
      geom_point()
  }
  
  x <- data.frame(x1 = rnorm(100), # no relationship
                  x2 = c(rnorm(70), rnorm(30, 2))) # positive relationship
  
  if(FALSE){
    
    ggplot(cbind(x, PA), aes(x1, PA)) + 
      geom_point() +
      geom_smooth(method = 'glm', method.args = list(family = 'binomial'))
    
    
    ggplot(cbind(x, PA), aes(x2, PA)) + 
      geom_point() +
      geom_smooth(method = 'glm', method.args = list(family = 'binomial'))
  }
  
  
  
  # Create rasters of x1 and x2
  
  
  dataf1 = sp::SpatialPointsDataFrame(coords = coords, data = data.frame(y = PA, x))
  invariant = "0 + Intercept"

  
  # Organise the data
  ##make mesh

  mesh5 <- INLA::inla.mesh.2d(loc = sp::coordinates(dataf1), 
                        max.edge = c(3, 3), 
                        cutoff = 1.3)
  
  ####The SPDE model is defined 
  spde <- INLA::inla.spde2.matern(mesh5, alpha=2)
  
  ###projector matrix for known
  A <- INLA::inla.spde.make.A(mesh5, loc = sp::coordinates(dataf1))
  
  ###make index for spatial field
  s.index <- INLA::inla.spde.make.index(name="spatial.field",n.spde=spde$n.spde)
  
  # Assumes invariant has `Intercept`
  stk.est <- INLA::inla.stack(data = list(y = dataf1$y),
                        A = list(A, 1), 
                        effects = list(c(s.index, list(Intercept = 1)),
                                       list(dataf1@data[, names(dataf1) != 'y', drop = FALSE])),
                        tag = 'est')

  dataframe <- dataf1@data 

  # spatial
  expect_error(
    suppressMessages(
      INLAstep.out <- INLAstep(fam1 = "binomial", 
                           dataframe,
                           in_stack = stk.est,
                           spde = spde,
                           invariant = "0 + Intercept + f(spatial.field, model=spde)",
                           direction = 'backwards',
                           include = 2:3,
                           y = 'y',
                           y2 = 'y',
                           powerl = 1,
                           inter = 1,
                           thresh = 2)
    ),
    NA
  )
  
  expect_true(is(INLAstep.out$best_model, 'inla'))
  expect_true(length(INLAstep.out$best_model$summary.random) == 1)
  
  
  
  # nonspatial
  expect_error(
    suppressMessages(
      INLAstep.out2 <- INLAstep(fam1 = "binomial", 
                               dataframe,
                               in_stack = stk.est,
                               invariant = "0 + Intercept",
                               direction = 'backwards',
                               include = 2:3,
                               y = 'y',
                               y2 = 'y',
                               powerl = 1,
                               inter = 1,
                               thresh = 2)
    ),
    NA
  )
  
  expect_true(is(INLAstep.out2$best_model, 'inla'))
  expect_true(length(INLAstep.out2$best_model$summary.random) == 0)
  
  
})


test_that('Some INLA example datasets work', {
  
  data(Epil)
  stack <- inla.stack(data = list(y = Epil$y),
                      A = list(1),
                      effects = list(data.frame(Intercept = 1, Epil[3:5])))
                      
  result <- INLAstep(fam1 = "poisson", 
                     Epil,
                     in_stack = stack,
                     invariant = "0 + Intercept",
                     direction = 'backwards',
                     include = 3:5,
                     y = 'y',
                     y2 = 'y',
                     powerl = 1,
                     inter = 1,
                     thresh = 2)
  
})


test_that('Spatial and nonspatial works steping through fixed effects only', {

})




test_that('Spatial and nonspatial works steping through fixed effects and spatial effect', {
  
})


test_that('Feature engineering works', {
  # Hopefully this will include refectored functions.
  
})



test_that('Forwards and backwards works', {
  # Hopefully this will include refectored functions.
  
})
