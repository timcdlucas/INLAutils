context('Test parallelRasterExtract')




library(raster)
# Create raster stack
r <- raster(ncol=36, nrow=18)
r[] <- 1:ncell(r)
r <- stack(r, r)

# Create polygon
cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
polys <- spPolygons(cds1, cds2)

#plot(r)
#plot(polys, add=TRUE)

test_that('parallelExtract gives same results as extract', {
  
  
  # Standard raster::extract
  v1 <- extract(r, polys)
  v2 <- extract(r, polys, fun = mean, df = TRUE)
  
  # Parallel extract
  #  Register the cluster first (use doMC on linux)
  suppressWarnings({
    v3 <- parallelExtract(r, polys, fun = NULL)
    v4 <- parallelExtract(r, polys, fun = mean)
  })
  
  expect_equal(data.frame(do.call(rbind, v1)), v3[, 3:4])
  
  expect_equal(v2[, 2:3], v4[, 2:3])
  
  
})



test_that('parallelExtract works for SpatialPolygons and SpatialPolygonDataFrames', {
  
  suppressWarnings({
      
    v3 <- parallelExtract(r, polys, fun = NULL)
    
    polys.df <- SpatialPolygonsDataFrame(Sr = polys, data = data.frame(names = c('a', 'b')))
    v5 <- parallelExtract(r, polys.df, fun = NULL, id = 'names')
    
    # check it works with multiple columsns
    polys.df2 <- SpatialPolygonsDataFrame(Sr = polys, data = data.frame(x = c('xx', 'zz'), names = c('a', 'b')))
    v6 <- parallelExtract(r, polys.df2, fun = NULL, id = 'names')
    
  })
  
  expect_equal(v3[, -1], v5[, -1])
  expect_equal(v5, v6)
  
  
  suppressWarnings({
      
    v3 <- parallelExtract(r, polys, fun = mean)
    
    polys.df <- SpatialPolygonsDataFrame(Sr = polys, data = data.frame(names = c('a', 'b')))
    v5 <- parallelExtract(r, polys.df, fun = mean, id = 'names')
    
    # check it works with multiple columsns
    polys.df2 <- SpatialPolygonsDataFrame(Sr = polys, data = data.frame(x = c('xx', 'zz'), names = c('a', 'b')))
    v6 <- parallelExtract(r, polys.df2, fun = mean, id = 'names')
  })
  
  expect_equal(v3[, -1], v5[, -1])
  expect_equal(v5, v6)
  
  
})



