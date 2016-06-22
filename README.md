INLAutils
==========

[![Build Status](https://travis-ci.org/timcdlucas/INLAutils.svg)](https://travis-ci.org/timcdlucas/INLAutils)
[![codecov.io](https://codecov.io/github/timcdlucas/INLAutils/coverage.svg?branch=master)](https://codecov.io/github/timcdlucas/INLAutils?branch=master)


A package containing utility functions for the `R-INLA` package.




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



```
## Loading required package: sp
```

```
## Loading required package: Matrix
```

```
## Loading required package: splines
```

```
## This is INLA 0.0-1463562937, dated 2016-05-18 (11:01:03+0200).
## See www.r-inla.org/contact-us for how to get help.
```


Overview
--------




### Plotting


I find the the `plot` function in `INLA` annoying and I like `ggplot2`.
So `INLAutils` provides an `autoplot` function for inla objects.


```r
      data(Epil)
      ##Define the model
      formula = y ~ Trt + Age + V4 +
               f(Ind, model="iid") + f(rand,model="iid")
      result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE))
     
      autoplot(result)
```

![plot of chunk autoplot](figure/autoplot-1.png)
