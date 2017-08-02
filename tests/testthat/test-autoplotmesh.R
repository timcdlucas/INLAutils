context('Test that autoplot works on mesh objects')


test_that('Mesh plot can be built and altered', {

  skip_if_not_installed('INLA')
  
  # code from https://www.math.ntnu.no/inla/r-inla.org/tutorials/spde/inla-spde-howto.pdf
  n=200 
  coo=matrix(runif(2*n), n)
  k<-10   
  s2rf<-0.7
  ## RF params.
  R<-s2rf*exp(-k*as.matrix(dist(coo)))

  s<-drop(rnorm(n)%*%chol(R))

  x <- runif(n)
  beta <- 1:2
  s2e <- 0.3
  lin.pred <- beta[1]+beta[2]*x+s
  y <- lin.pred+rnorm(n,0,sqrt(s2e))

  mesh<-inla.mesh.2d(coo, 
          ## provide locations or domain
          max.edge=c(1/k,2/k), 
          ## mandatory
          cutoff=0.1/k) 


  expect_error(pl <- autoplot(mesh), NA)
  expect_error(pl <- autoplot(mesh, col = 'red', lwd = 2, size = 2), NA)

  pl2 <- pl + scale_colour_manual(values = c(1, 2, 3, 4, 5))

  expect_false(identical(pl2, pl))

})
