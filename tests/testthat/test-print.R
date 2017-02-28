context('Simple tests to make sure no print methods cause errors')


# Can't easily test much beyond error/not error


test_that('Print methods do not give errors', {

n=50 
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
## good to have >0

A <- inla.spde.make.A(mesh=mesh,loc=coo)

spde<-inla.spde2.matern(mesh=mesh,alpha=1.5)

stk.e <- inla.stack(tag='est',## tag
           data=list(y=y),## response
           A=list(A,1),## two projector matrix
           effects=list(## two elements:
           s=1:spde$n.spde,## RF index
           data.frame(b0=1,x=x)))

formula <- y~0+b0+x+## fixed part
            f(s,model=spde)## RF term
res<-inla(formula,data=inla.stack.data(stk.e),control.predictor=list(A=inla.stack.A(stk.e)))


gproj<-inla.mesh.projector(mesh,xlim=0:1,ylim=0:1,dims=c(300,300))
g.mean<-inla.mesh.project(gproj, res$summary.random$s$mean)

z <- capture.output({
  # Now test print all objects
  expect_error(print(stk.e), NA)
  expect_error(print(mesh), NA)
  expect_error(print(gproj), NA)
})

})
