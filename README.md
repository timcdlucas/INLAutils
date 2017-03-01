INLAutils
==========

[![Build Status](https://travis-ci.org/timcdlucas/INLAutils.svg)](https://travis-ci.org/timcdlucas/INLAutils)
[![codecov.io](https://codecov.io/github/timcdlucas/INLAutils/coverage.svg?branch=master)](https://codecov.io/github/timcdlucas/INLAutils?branch=master)


A package containing utility functions for the `R-INLA` package.

There's a fair bit of overlap with [inlabru](www.github.com/fbachl/inlabru).


Installation
-------------

To install, first install `INLA`.


```r
install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")
```

then install `INLAutils`


```r
library(devtools)
install_github('timcdlucas/INLAutils')

library(INLA)
library(INLAutils)
```





Overview
--------




### Plotting


I find the the `plot` function in `INLA` annoying and I like `ggplot2`.
So `INLAutils` provides an `autoplot` method for INLA objects.


```r
      data(Epil)
      ##Define the model
      formula = y ~ Trt + Age + V4 +
               f(Ind, model="iid") + f(rand,model="iid")
      result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))
     
      autoplot(result)
```

![plot of chunk autoplot](figure/autoplot-1.png)


There is also an autoplot method for INLA SPDE meshes.


```r
    m = 100
    points = matrix(runif(m * 2), m, 2)
    mesh = inla.mesh.create.helper(
      points = points,
      cutoff = 0.05,
      offset = c(0.1, 0.4),
      max.edge = c(0.05, 0.5))
    
    autoplot(mesh)
```

![plot of chunk autoplot_mesh](figure/autoplot_mesh-1.png)


There are also functions for plotting more diagnostic plots.


```r
 data(Epil)
 observed <- Epil[1:30, 'y']
 Epil <- rbind(Epil, Epil[1:30, ])
 Epil[1:30, 'y'] <- NA
 ## make centered covariates
 formula = y ~ Trt + Age + V4 +
          f(Ind, model="iid") + f(rand,model="iid")
 result = inla(formula, family="poisson", data = Epil,
               control.predictor = list(compute = TRUE, link = 1))
 ggplot_inla_residuals(result, observed, binwidth = 0.1)
```

![plot of chunk plot_residuals](figure/plot_residuals-1.png)

```r
 ggplot_inla_residuals2(result, observed, se = FALSE)
```

```
## `geom_smooth()` using method = 'loess'
```

![plot of chunk plot_residuals](figure/plot_residuals-2.png)


### Analysis

There are some helper functions for general analyses.


`INLAstep` runs stepwise variable selection with INLA.


```r
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
  
  result$best_formula
```

```
## y ~ 0 + Intercept + Base + Age + V4
## <environment: 0x000000002e79f078>
```

```r
  autoplot(result$best_model, which = 1)
```

![plot of chunk INLAstep](figure/INLAstep-1.png)



`makeGAM` helps create a function object for fitting GAMs with INLA.


```r
 data(Epil)
 formula <- makeGAM('Age', invariant = '', linear = c('Age', 'Trt', 'V4'), returnstring = FALSE)
 formula
```

```
## y ~ +Age + Trt + V4 + f(inla.group(Age), model = "rw2")
## <environment: 0x0000000020663a88>
```

```r
 result = inla(formula, family="poisson", data = Epil)
```





### Domain specific applications



To do list
----------

* `inla.sdm`
* ggplot2 version of `plot.inla.tremesh`
* Make good `plot` and `ggplot` functions for plotting the Gaussian Random Field with value and uncertainty.
* `stepINLA`

