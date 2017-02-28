context('Test all plots (just no errors)')



test_that('ggplot_projection_shapefile works', {
   set.seed(2)
   n <- 20
   loc <- matrix(runif(n*2), n, 2)
   mesh <- inla.mesh.create(loc, refine = list(max.edge=0.05))
   projector <- inla.mesh.projector(mesh)
   field <- cos(mesh$loc[,1] * 2 * pi * 3) * sin(mesh$loc[, 2] * 2 * pi * 7)
   projection <- inla.mesh.project(projector, field)
   # And the shape file
   crds <- loc[chull(loc), ]
   SPls <- SpatialPolygons(list(Polygons(list(Polygon(crds)), ID = 'a')))
   expect_error(ggplot_projection_shapefile(projection, projector, SPls), NA)
   
   # Alternatively plot a raster
   library(raster)
   #t(m)[,nrow(m):1]
   raster <- raster(t(projection)[nrow(projection) : 1, ])
   extent(raster) <- c(range(projector$x), range(projector$y))
   
   expect_error(ggplot_projection_shapefile(raster, spatialpolygons = SPls), NA)
   
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
