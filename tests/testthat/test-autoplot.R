context('Basic tests that autoplot produces plots without errors')

test_that('autoplot does not fail when it should not.', {
  library(INLA)
  data(Epil)
  ##Define the model
  formula = y ~ Trt + Age + V4 +
           f(Ind, model="iid") + f(rand,model="iid")
  result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))

  # Test that figure builds
  expect_error(p <- autoplot(result), NA)
  expect_error(p, NA)

  # test theme is modifiable
  expect_error(p2 <- p + theme_bw(), NA)

  expect_error(p2, NA)
  expect_false(identical(p, p2))

  # Test that you can change a single figure
  p3 <- p
  expect_error(p3[2] <- p3[2] + ggtitle('Hyper parameters'), NA)
  expect_false(identical(p, p3))


})
