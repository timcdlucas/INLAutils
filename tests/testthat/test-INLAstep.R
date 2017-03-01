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
  
  # Assumes invariant has `Intercept`
  stk.est <- inla.stack(data = list(y = dataf1$y),
                        A = list(A, 1), 
                        effects = list(c(s.index,list(Intercept = 1)),
                                       list(dataf1@data[, names(dataf1) != y, drop = FALSE])),
                        tag = 'est')
  


  expect_error(
    suppressMessages(
      INLAstep.out <- INLAstep(fam1 = "binomial", 
                           dataf1,
                           in_stack = stk.est,
                           spde = spde,
                           invariant = "0 + Intercept +  f(i, model=spde)",
                           direction = 'backwards',
                           include = 2:3,
                           y = y,
                           y2 = y,
                           powerl = 1,
                           inter = 1,
                           thresh = 2)
    ),
    NA
  )
  

  
})



test_that('Spatial and nonspatial works steping through fixed effects only', {
  
})




test_that('Spatial and nonspatial works steping through fixed effects and spatial effect', {
  
})


test_that('Feature engineering works', {
  # Hopefully this will include refectored functions.
  
})
