
#create k-fold function that can accept user defined groups? 
#Need to do stepwise within? I guess to robust check parameters - how to combine output.
#Let it take presences/y and then absence makes it binomial
#kfold just presences if not an SDM
#doesnt need spatial or not
#spit out sum CPO as well as DIC and wAIC, pred-obs data, standardised residuals
#how to deal with invariant??

#' Fit INLA species distribution models.
#' 
#'@param dataframe A \code{\link[sp]{SpatialPointsDataFrame}} containing the presence absence values
#'@param predictors Raster of predictors (covariates)
#'@param include Vector of integers describing which covariates to include in the model
#'@param step Logical indicating whether to run stepwise elimination of variables.
#'@param invariant Character indicating the parts of the model formula that should not change despite stepwise selection (e.g. the intercept).
#'@param cross_validation Run cross validation?
#'@param cv_folds How many folds should the data be split into?
#'@param meshvals List giving details for the creation of the INLA mesh (see details and \code{\link[INLA]{inla.mesh.2d}})
#'@param spatial Run INLA with a spatial term.
#'@param num.threads How many threads should be used for parallel computation.
#'
#'@details For now invariant MUST include 'Intercept'.
#'
#'meshvals takes a list of up to five named values:
#'  \describe{
#'  \item{inner.max.edge}{Maximum triangle length for inner domain.}
#'  \item{outer.max.edge}{Maximum triangle length for outer domain.}
#'  \item{cutoff}{Minumum allowed distance between mesh nodes.}
#'  \item{inner.offset}{Extension distance beyond points.}
#'  \item{outer.offset}{Additional extension distance with larger triangles (with max length outer.max.edge)}
#'  }
#'  
#'  Note that negative values for the offsets are in absolute units by default. Negative values give the 
#'    extension distance relative to the diameter of the coordinates range (i.e. -0.1 will create an extension 10\% the 
#'    that 10\% the diameter of the points).
#'  
#'These values are explained in more detail in \code{\link[INLA]{inla.mesh.2d}}.
#'
#'
#'@export
#'@examples
#'\dontrun{
#'library(INLA)
#'set.seed(6)
#'
#'# Create locations, presence absence points and covariates 
#'#   with spatial and environmental relationships
#'coords <- data.frame(long = c(rnorm(70), rnorm(30, 3)), lat = rnorm(100))
#'PA <- rep(c(0, 1), each = 50)
#'x <- data.frame(x1 = rnorm(100), # no relationship
#'                x2 = c(rnorm(70), rnorm(30, 5))) # positive relationship
#'
#'# Have a look
#'\dontrun{
#'ggplot(cbind(x, PA), aes(x1, PA)) + 
#'  geom_point() +
#'  geom_smooth(method = 'glm', method.args = list(family = 'binomial'))
#'
#'
#'ggplot(cbind(x, PA), aes(x2, PA)) + 
#'  geom_point() +
#'  geom_smooth(method = 'glm', method.args = list(family = 'binomial'))
#'
#'
#'ggplot(cbind(coords, PA), aes(long, lat, colour = PA)) + geom_point()
#'}
#'
#'# Set raster resolution
#'res <- 50
#'
#'# Create raster limits
#'xrange <- range(coords$long)
#'xrange <- c(floor(xrange[1]), ceiling(xrange[2]))
#'yrange <- range(coords$lat)
#'yrange <- c(floor(yrange[1]), ceiling(yrange[2]))
#'
#'# Calculate number of cells
#'xcells <- res * (xrange[2] - xrange[1])
#'ycells <- res * (yrange[2] - yrange[1])
#'
#'# Create an empty raster of correct dims
#'suppressWarnings(
#'  raster <- raster::raster(matrix(NA, ncol = ycells, nrow = xcells), 
#'    xmn = xrange[1], xmx = xrange[2], ymn = yrange[1], ymx = yrange[2])
#')
#'# Add dataframe data to rasters, then fill gaps with random data.
#'x1 <- raster::rasterize(coords, raster, x$x1)
#'x1[is.na(x1)] <- rnorm(sum(is.na(raster::getValues(x1))))
#'
#'x2 <- raster::rasterize(coords, raster, x$x2)
#'x2[is.na(x2)] <- rnorm(sum(is.na(raster::getValues(x2))))
#'
#'# Stack rasters
#'predictors <- raster::stack(x1, x2)
#'
#'# Pull together coordinates and PA data into SpatialPointsDataFrame
#'dataframe = sp::SpatialPointsDataFrame(coords = coords, data = data.frame(y = PA))
#'
#'# Run the model.
#' model <- inlaSDM(dataframe, 
#'                  predictors, 
#'                  spatial = TRUE, 
#'                  cross_validation = FALSE,
#'                  meshvals = list(cutoff = 0.3, inner.max.edge = 1))
#'                  
#'autoplot(model$mesh[[1]])
#'autoplot(model$models[[1]])
#'}

inlaSDM<-function(dataframe,
                  predictors, 
                  include = 1:raster::nlayers(predictors),
                  step = FALSE,
                  invariant = "0 + Intercept",
                  cross_validation = FALSE,
                  cv_folds = 5,
                  spatial = TRUE,
                  num.threads = 1,
                  meshvals = list(inner.max.edge = max(raster::res(predictors)) * 10, 
                                  outer.max.edge = max(raster::res(predictors)) * 100, 
                                  cutoff = 0, 
                                  inner.offset = -0.1,
                                  outer.offset = -0.3)
                  ){
  
  
  # Deal with names of response column.
  assert_that(ncol(dataframe) == 1)
  y <- 'y'
  names(dataframe) <- 'y'
  
  
  # Make meshvals be complete
  meshvals_complete = list(inner.max.edge = max(raster::res(predictors)) * 10, 
                           outer.max.edge = max(raster::res(predictors)) * 100, 
                           cutoff = 0, 
                           inner.offset = -0.1,
                           outer.offset = -0.3)
  # Check that all values in meshvals arg are correct
  assert_that(all(names(meshvals) %in% names(meshvals_complete)))
  
  
  meshvals_complete[names(meshvals)] <- meshvals

  
  
  if(!cross_validation) cv_folds <- 1
  
  # Empty list for all inla models
  models <- list()
  # Empty data frame for summaries
  model_res <- as.data.frame(matrix(NA, ncol = 4, nrow = cv_folds))
  names(model_res) <- c('replicate', 'AUC', 'WAIC', 'comp_time')
  # Empty list for meshes
  meshes <- list()
  formulas <- list()
  
  
  group <- dismo::kfold(dataframe, cv_folds, by = dataframe$y)
  
  
  
  
    
  for(cv in 1:cv_folds){
    
    if(cross_validation == TRUE){
      train <- dataframe[group != cv, ]
      test  <- dataframe[group == cv, ]
      test@data[, y] <- NA
      dataf1 <- suppressWarnings(rbind(train,test))
    } else { # Not sure how this works
      dataf1 <- dataframe
    }
    
    
    
    
    ###add training data and testing data (NAs) together
    sss <- as.data.frame(raster::extract(predictors[[include]], dataf1))
    # If there's only 1 predictor, we end up with funny names.
    if(NCOL(sss) == 1) colnames(sss) <- names(predictors[[include]])
    
    sss <- cbind(sss, dataf1@data[, y])
    names(sss)[NCOL(sss)] <- y
    
    # Why?
    # sss <- sss[complete.cases(sss), ]
    dataf1@data <- sss
    
    # Organise the data
    if(spatial == TRUE){
      ##make mesh
      mesh5 <- INLA::inla.mesh.2d(loc = sp::coordinates(dataf1), 
                            max.edge = c(meshvals_complete$inner.max.edge, meshvals_complete$outer.max.edge), 
                            cutoff = meshvals_complete$cutoff, 
                            offset = c(meshvals_complete$inner.offset, meshvals_complete$outer.offset))
      
      ####The SPDE model is defined 
      spde <- INLA::inla.spde2.matern(mesh5, alpha=2)
      
      ###projector matrix for known
      A <- INLA::inla.spde.make.A(mesh5, loc = sp::coordinates(dataf1))
      
      ###make index for spatial field
      s.index <-
        INLA::inla.spde.make.index(name = "spatial.field", n.spde = spde$n.spde)
      

      # Assumes invariant has `Intercept`
      stk.est <- INLA::inla.stack(data = list(y = dataf1$y),
                            A = list(A, 1), 
                            effects = list(c(s.index,list(Intercept = 1)),
                                         list(dataf1@data[, names(dataf1) != y, drop = FALSE])),
                            tag = 'est')
      
      ###projector matrix for known
      A.val <- INLA::inla.spde.make.A(mesh5, loc = sp::coordinates(dataf1))
      
      
    } else {
      # Assumes invariant has `Intercept`
      stk.est <- INLA::inla.stack(data = list(y = dataf1$y),
                            A = list(1), 
                            effects = list(data.frame(Intercept = 1, dataf1@data[, names(dataf1) != y, drop = FALSE])),
                            tag = 'est')
      
      spde = NULL # Set this NULL so INLAstep know what to do.
      
    }
    
    
    # Create the formula.
    if(step == TRUE){
      
      if(spatial){
        invariantStep <- paste(invariant, ' + f(spatial.field, model = spde)')
      } else {
        invariantStep <- invariant
      }
      
      includeStep <- which(names(dataf1) != y)
      
      # Data is a raster (or at least some s4 thing).
      INLAstep_model <- INLAstep(fam1 = "binomial", #?
                           dataf1@data,
                           in_stack = stk.est,
                           spde = spde,
                           invariant = invariantStep,
                           direction = 'backwards',
                           include = includeStep,
                           y = y,
                           y2 = y,
                           powerl = 1,
                           inter = 1,
                           thresh = 2,
                           num.threads = num.threads)
      
      form1 <- deparse(INLAstep_model$best_formula)
      # Remove the spatial field. This gets added back in later.
      form1 <- gsub(' + f(spatial.field, model = spde)', '', form1)
      
    } else {
      # Make formula of all covariates but not
      form1 <- paste0(y, ' ~ ', invariant, ' + ', paste(names(predictors[[include]]), collapse = ' + '))
    }
    
    
    # Fit the model
    if(spatial == TRUE){
      
      # Create formula object adding the spatial.field random effect
      form1 <- formula(paste(form1, ' + f(spatial.field, model = spde)'))
      
      # Fit spatial inla model.
      res5 <- INLA::inla(
        form1,
        data = INLA::inla.stack.data(stk.est, spde = spde),
        family = "binomial",
        control.predictor = list(A = INLA::inla.stack.A(stk.est), compute =
                                   TRUE),
        control.compute = list(cpo = TRUE, waic = TRUE, dic = TRUE),
        control.fixed = list(expand.factor.strategy = "inla"),
        num.threads = num.threads,
        silent = TRUE
      )
      
    } else {
      
      # Create formula object
      form1 <- formula(form1)
      # Fit unspatial inla model
      res5 <- INLA::inla(
        form1,
        data = INLA::inla.stack.data(stk.est),
        family = "binomial",
        control.compute = list(cpo = TRUE, waic = TRUE, dic = TRUE),
        control.fixed = list(expand.factor.strategy = "inla"),
        control.predictor = list(compute = TRUE, link = 1),
        num.threads = num.threads,
        silent = TRUE
      )
      
    }
    
    
    inla.time <- (res5$cpu.used)
    waic <- (res5$waic$waic)
    dic <- (res5$dic$dic)
    cpo.fit <- (sum(log(res5$cpo$cpo), na.rm = T))
    
    ##put it all together ## replace with function!!!!
    res1 <-
      data.frame(row.names = rownames(res5$summary.fixed), res5$summary.fixed)
    res1$sig <-
      "non-sig"##is a term significant? i.e. does it include 0 in distribution
    res1$sig[(res1$X0.025quant < 0 &
                res1$X0.975quant < 0) |
               (res1$X0.025quant > 0 & res1$X0.975quant > 0)] <- "sig"
    
    
    if (cross_validation == TRUE) {

      linearpredictor <-
        as.data.frame(res5$summary.fitted.values[1:nrow(dataf1), c("mean", "sd")])
      
      presencePreds <- linearpredictor[group == cv & dataframe$y == 1, 'mean']
      absencePreds <- linearpredictor[group == cv & dataframe$y == 0, 'mean']
      auc <- dismo::evaluate(presencePreds, absencePreds)@auc
    } else {
      auc <- NA
    }
    
    model_res[cv,] <- c(
      replicate = cv,
      AUC = auc,
      WAIC = res5$waic$waic,
      comp_time = inla.time[['Total']]
    )
                        
    models[[cv]] <- res5
    if(spatial){
      meshes[[cv]] <- mesh5
    }
    
    formulas[[cv]] <- form1
  }## end cv
  
  output <- list(result_summary = model_res, models = models, mesh = meshes, formula = formulas)
  class(output) <- 'inlaSDM'

  return(output)
}
