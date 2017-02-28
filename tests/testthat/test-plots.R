context('Test all plots (just no errors)')



test_that('ggplot_projection_shapefile works', {
   set.seed(2)
   n <- 20
   loc <- matrix(runif(n*2), n, 2)
   mesh <- INLA::inla.mesh.create(loc, refine = list(max.edge=0.05))
   projector <- INLA::inla.mesh.projector(mesh)
   field <- cos(mesh$loc[,1] * 2 * pi * 3) * sin(mesh$loc[, 2] * 2 * pi * 7)
   projection <- INLA::inla.mesh.project(projector, field)
   # And the shape file
   crds <- loc[grDevices::chull(loc), ]
   SPls <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(crds)), ID = 'a')))
   expect_error(ggplot_projection_shapefile(projection, projector, SPls), NA)
   
   # Alternatively plot a raster
   library(raster)
   #t(m)[,nrow(m):1]
   raster <- raster(t(projection)[nrow(projection) : 1, ])
   extent(raster) <- c(range(projector$x), range(projector$y))
   
   expect_error(ggplot_projection_shapefile(raster, spatialpolygons = SPls), NA)
   
   
   # Check you get the same results using a raster or matrix + projector
   p1 <- ggplot2::ggplot_build(ggplot_projection_shapefile(raster, spatialpolygons = SPls))
   p2 <- ggplot2::ggplot_build(ggplot_projection_shapefile(projection, projector, SPls))
   
   expect_equal(p1$data, p2$data)

   

   
   # Try all combinations: rs, sm, mr, mrs, m, r, s
   # plot mesh and raster but no shapes
   expect_error(ggplot_projection_shapefile(raster, spatialpolygons = SPls), NA)
   expect_error(ggplot_projection_shapefile(spatialpolygons = SPls, mesh = mesh, shapecol = 'blue'), NA)
   expect_error(ggplot_projection_shapefile(raster, mesh = mesh), NA)
   expect_error(ggplot_projection_shapefile(raster, spatialpolygons = SPls, mesh = mesh), NA)
   expect_error(ggplot_projection_shapefile(mesh = mesh), NA)
   expect_error(ggplot_projection_shapefile(raster), NA)
   expect_error(ggplot_projection_shapefile(spatialpolygons = SPls), NA)
   
   
   
})


test_that('plot_inla residuals works', {
  
  library(INLA)
  data(Epil)
  observed <- Epil[1:30, 'y']
  
  Epil <- rbind(Epil, Epil[1:30, ])
  Epil[1:30, 'y'] <- NA
  

  ## make centered covariates
  formula = y ~ Trt + Age + V4 +
           f(Ind, model="iid") + f(rand,model="iid")
  result = inla(formula, family="poisson", data = Epil, control.predictor = list(compute = TRUE, link = 1))
  
  
  
  
  expect_error(plot_inla_residuals(result, observed), NA)
  expect_error(ggplot_inla_residuals(result, observed), NA)
  expect_error(ggplot_inla_residuals2(result, observed), NA)
  
  
  
  
  
  data(Seeds)
  l <- nrow(Seeds)
  Seeds <- rbind(Seeds, Seeds)
  Seeds$r[1:l] <- NA
  

  formula = r ~ x1 * x2 + f(plate, model = "iid")
  mod.seeds = inla(formula, data=Seeds, family = "binomial", Ntrials = n, 
                   control.predictor = list(compute = TRUE, link = 1))
  
  
  
  
  ## improved estimation of the hyperparameters
  
  expect_error(plot_inla_residuals(mod.seeds, na.omit(Seeds$r / Seeds$n)), NA)
  expect_error(ggplot_inla_residuals(mod.seeds, na.omit(Seeds$r / Seeds$n)), NA)
  expect_error(ggplot_inla_residuals2(mod.seeds, na.omit(Seeds$r / Seeds$n)), NA)
  
  
})



