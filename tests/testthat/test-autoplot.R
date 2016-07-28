context('Basic tests that autoplot produces plots without errors.')

test_that('autoplot does not fail when it should not.', {
  library(INLA)
  data(Epil)
  ##Define the model
  formula = y ~ Trt + Age + V4 +
           f(Ind, model="iid") + f(rand,model="iid")
  result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))

  p <- autoplot(result)
  p + theme_bw()

  p[2] <- p[2] + ggtitle('Hyper parameters')
  expect_error(p, NA)

})
