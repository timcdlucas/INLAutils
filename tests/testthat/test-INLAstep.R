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
  
  
  # best_formula and best_model should match
  varsInFormula <- sapply(INLAstep.out$best_model$names.fixed, 
                          function(x) grepl(x, as.character(INLAstep.out$best_formula)[3]))
  expect_true(all(varsInFormula))
  
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
  
  # best_formula and best_model should match
  varsInFormula <- sapply(INLAstep.out2$best_model$names.fixed, 
                          function(x) grepl(x, as.character(INLAstep.out2$best_formula)[3]))
  expect_true(all(varsInFormula))
  
  
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
  
  # best_formula and best_model should match
  varsInFormula <- sapply(result$best_model$names.fixed, function(x) grepl(x, as.character(result$best_formula)[3]))
  expect_true(all(varsInFormula))
  
  
})





test_that('Spatial and nonspatial works steping through fixed effects and spatial effect', {
  
})


test_that('Feature engineering works. expandExplanatoryVars', {
  # Hopefully this will include refectored functions.
  expl <- c('a', 'b')
  newexpl1 <- expandExplanatoryVars(expl, 1, 1)
  
  expect_equal(expl, newexpl1)
  
  
  newexpl2 <- expandExplanatoryVars(expl, inter = 2, powerl = 1)
  
  expect_equal(newexpl2, c('a', 'b', 'a:b'))
  
  newexpl3 <- expandExplanatoryVars(expl, inter = 1, powerl = 2)
  
  expect_equal(newexpl3, c('a', 'b', 'I(a^2)',  'I(b^2)'))
  
  
  expl2 <- letters[1:3]
  longexpl <- expandExplanatoryVars(expl2, inter = 3, powerl = 1)
  expect_equal(length(longexpl), 3 + 3 + 1)
  
  longexpl2 <- expandExplanatoryVars(expl, inter = 1, powerl = 3)
  expect_equal(length(longexpl2), 2 * 3)
  longexpl2 <- expandExplanatoryVars(expl, inter = 1, powerl = 4)
  expect_equal(length(longexpl2), 2 * 4)
  
  
})



test_that('Feature engineering works. Whole models.', {
  # Hopefully this will include refectored functions.
  
  
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
                     powerl = 3,
                     inter = 2,
                     thresh = 2)
  # best_formula and best_model should match
  varsInFormula <- sapply(result$best_model$names.fixed, function(x) grepl(x, as.character(result$best_formula)[3]))
  expect_true(all(varsInFormula))
  
  
  result1 <- INLAstep(fam1 = "poisson", 
                      Epil,
                      in_stack = stack,
                      invariant = "0 + Intercept",
                      direction = 'forwads',
                      include = 3:5,
                      y = 'y',
                      y2 = 'y',
                      powerl = 1,
                      inter = 1,
                      thresh = 2)
  
  # best_formula and best_model should match
  varsInFormula <- sapply(result1$best_model$names.fixed, function(x) grepl(x, as.character(result1$best_formula)[3]))
  expect_true(all(varsInFormula))
  
  # Not sure good ways of testing this
  # Given the set seed, and having looked at the answer, I know that the final model
  #   has a formula that is longer than 0 + Intercept + 3 variables.
  
  nvars <- length(strsplit(as.character(result$best_formula)[3], '+', fixed = TRUE)[[1]])
  expect_true(nvars > 2 + 3)
  
  expect_true(is(result$best_model, 'inla'))
  expect_true(is(result, 'INLAstep'))
  
  
})



test_that('Forwards and backwards works', {

  data(Epil)
  stack <- inla.stack(data = list(y = Epil$y),
                      A = list(1),
                      effects = list(data.frame(Intercept = 1, Epil[3:5])))
  
  
  result1 <- INLAstep(fam1 = "poisson", 
                     Epil,
                     in_stack = stack,
                     invariant = "0 + Intercept",
                     direction = 'forwards',
                     include = 3:5,
                     y = 'y',
                     y2 = 'y',
                     powerl = 1,
                     inter = 1,
                     thresh = 2)
  varsInFormula <- sapply(result1$best_model$names.fixed, function(x) grepl(x, as.character(result1$best_formula)[3]))
  expect_true(all(varsInFormula))
  
  
  # Try and make a dataset where a variable WILL get added.
  Epil2 <- Epil
  Epil2$Base <- Epil$y + rnorm(nrow(Epil), sd = 0.01)
  Epil2$Age <- Epil$y + rnorm(nrow(Epil), sd = 0.01)
  
  stack2 <- inla.stack(data = list(y = Epil2$y),
                      A = list(1),
                      effects = list(data.frame(Intercept = 1, Epil2[3:5])))
  
  result2 <- INLAstep(fam1 = "poisson", 
                      Epil2,
                      in_stack = stack2,
                      invariant = "0 + Intercept",
                      direction = 'forwads',
                      include = 3:5,
                      y = 'y',
                      y2 = 'y',
                      powerl = 1,
                      inter = 1,
                      thresh = 0.01)
  
  # test that best_formula and best_model match.
  varsInFormula <- sapply(result2$best_model$names.fixed, function(x) grepl(x, as.character(result2$best_formula)[3]))
  expect_true(all(varsInFormula))
  
  expect_true(all(c('Base', 'Age') %in% result2$best_model$names.fixed))
  
  
  
  
  # Try and make a dataset where no variables retained
  Epil3 <- Epil
  Epil2$Base <- rnorm(nrow(Epil), sd = 0.01)
  Epil2$Age <- rnorm(nrow(Epil), sd = 0.01)
  Epil2$V4 <- rnorm(nrow(Epil), sd = 0.01)
  
  
  stack2 <- inla.stack(data = list(y = Epil2$y),
                       A = list(1),
                       effects = list(data.frame(Intercept = 1, Epil2[3:5])))
  
  result2 <- INLAstep(fam1 = "poisson", 
                      Epil2,
                      in_stack = stack2,
                      invariant = "0 + Intercept",
                      direction = 'forwads',
                      include = 3:5,
                      y = 'y',
                      y2 = 'y',
                      powerl = 1,
                      inter = 1,
                      thresh = 0.01)
  
  # test that best_formula and best_model match.
  varsInFormula <- sapply(result2$best_model$names.fixed, function(x) grepl(x, as.character(result2$best_formula)[3]))
  expect_true(all(varsInFormula))
  
  expect_true(all(!c('Base', 'Age', 'V4') %in% result2$best_model$names.fixed))
  
})
