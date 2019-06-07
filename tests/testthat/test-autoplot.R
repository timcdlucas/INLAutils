context('Basic tests that autoplot produces plots without errors')

test_that('autoplot does not fail when it should not.', {
  
  skip_if_not_installed('INLA')
  library(INLA)
  data(Epil)
  ##Define the model
  formula = y ~ Trt + Age + V4 +
           f(Ind, model="iid") + f(rand,model="iid")
  result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))

  # Test that figure builds
  expect_error(p <- autoplot(result, which = 1:5), NA)
  expect_error(p, NA)

  expect_error(autoplot(result, which = 1:4, CI = TRUE), NA)
  expect_error(autoplot(result, which = 1:4, priors = TRUE), NA)
  expect_error(autoplot(result, which = 1, priors = TRUE, CI = TRUE), NA)
  
  # test theme is modifiable
  expect_error(p2 <- p + theme_bw(), NA)

  expect_error(p2, NA)
  expect_false(identical(p, p2))

  # Test that you can change a single figure
  p3 <- p
  expect_error(p3[[2]] <- p3[[2]] + ggtitle('Hyper parameters'), NA)
  expect_false(identical(p, p3))


})



test_that('Check which arg more carefully.', {
  
  skip_if_not_installed('INLA')
  
  data <- data.frame(y = rpois(100, 10), x1 = rnorm(100))
  
  data$x2 <- sin(data$y / 2) + rnorm(100, sd = 0.1)
  
  #ggplot(data, aes(y, x2)) + geom_point()
  
  model <- inla(y ~ x1 + x2, data = data, family = 'poisson')
  
  expect_error(suppressWarnings(autoplot(model, which = c(1, 2))), NA)
  
})



test_that('Plot prior different cases', {

  skip_if_not_installed('INLA')
  
  library(sp)
  data(meuse)
  
  meuse <- cbind(meuse, y.intercept = 1)
  
  modform <- cadmium ~ -1 + y.intercept + elev + dist + om
  
  meuse_model <- inla(modform, data = meuse)
  
  expect_error(autoplot(meuse_model, priors = TRUE), NA)
  
  
  modform2 <- cadmium ~ elev + dist + om
  
  meuse_model2 <- inla(modform2, data = meuse)
  expect_error(autoplot(meuse_model2, priors = TRUE), NA)
  
  
})
