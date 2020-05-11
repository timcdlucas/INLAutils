#' A function to run a spatial leave-one-out cross-validation in R using INLA.
#' 
#' This function will apply a spatial leave-one-out cross-validation (SLOO-CV) of one or several models running on INLA.
#' The function requires a dataframe that includes a response, coordinates (longitude, latidude). 
#' Covariates can be inluced as well in the dataframe. Furthermore, the user needs to provide a radius
#' around which points will be removed during the SLOO-CV process. The number of iterations needs to be provided as well. 
#' Also the user needs to provide INLA inputs, including: formula, family (and ntrials for Binomial), and mesh
#' In addition, one can provide further INLA specifications, such as int.strategy='eb'.
#' SLOO-CV computes the root mean square error (rmse) with a tolerance for confidence interval defined by the user. 
#' In option, other scores can be computed, such as the mean absolute error, dawid-sebastiani or using square root of the data.
#' @param dataframe dataframe including the variables to be used 
#' @param long Character string giving the name of the longitude variable
#' @param lat Character string giving the name of the latitude variable
#' @param y Character string giving the names of the response
#' @param ss Numeric value giving the sample size to process (number of SLOO runs)
#' @param rad Numeric value giving the radius for the spatial buffer around left-out point's location
#' @param modform Formula or list of Formula for the model or models, respectively in INLA format
#' @param family Character string or list of Character string giving the family or families, respectively of the response in INLA format 
#' @param mesh inla.mesh object, consisting in triangle mesh to be defined using INLA.
#' @param ntrials vector of positive integers c(1,2,...) setting the number of trials for a Binomial family
#' @param int.strategy Character string giving INLA integration strategy
#' @param alpha Numeric value (0,...,1) giving the threshold for computing confidence intervals (1-alpha) of rmse and mae estimation
#' @param mae If TRUE, compute the mean absolute error (mae) and the root mean square error (rmse). If FALSE, compute the rmse only.
#' @param ds If TRUE, compute the Dawid-Sebastiani score (ds). If FALSE, does not compute ds.
#' @param sqroot If TRUE, compute the square root of the observed and predicted values to generate the rmse and/or mae. 
#' If FALSE, the rmse and/or mae are computed without transformation of the data.
#' @param print Logical to determine whether to print a summary of the results.
#' @param plot Logical to determine whether to print the plot of the results. 
#' @param ... other arguments passed to inla
#'
#' @export
#' 
#' @name inlasloo
#' 
#' @examples 
#'  \dontrun{
#' require(INLA)
#' require(sp)
#' require(grDevices)
#' 
#' # generate a dataframe and INLA output for the function
#' set.seed(10)
#' coords <- data.frame(long = c(rnorm(70), rnorm(30, 3)), lat = rnorm(100))
#' PA <- rep(c(0, 1), each = 50)
#' x <- data.frame(x1 = rnorm(100),x2 = c(rnorm(70), rnorm(30, 2)))# x1 no relat., x2 pos. relat.
#' dataf1 <- sp::SpatialPointsDataFrame(coords = coords, data = data.frame(y = PA, x))
#' mesh <- INLA::inla.mesh.2d(loc = sp::coordinates(dataf1),max.edge = c(3, 3),cutoff = 1.3)
#' spde <- INLA::inla.spde2.matern(mesh, alpha=2)#SPDE model is defined
#' A <- INLA::inla.spde.make.A(mesh, loc = sp::coordinates(dataf1))#projector matrix
#' dataframe <- data.frame(dataf1) #generate dataframe with response and covariate
#' modform<-stats::as.formula(paste('y ~ -1+ x1 + x2 + y.intercept + f(spatial.field, model=spde)'))
#' stk <- INLA::inla.stack(data=list(y=dataframe$y),A=list(A, 1),
#' effects=list(list(spatial.field=1:spde$n.spde),
#' list(y.intercept=rep(1,length(dataframe$y)),covariate=dataframe[c(-1)])),tag='est')
#' out <- INLA::inla(modform, family='normal',Ntrials = 1, data=INLA::inla.stack.data(stk, spde=spde),
#'                   control.predictor = list(A=INLA::inla.stack.A(stk),link=1),
#'                   control.compute = list( config=TRUE),control.inla = list(int.strategy='eb'))
#' out.field <- INLA::inla.spde2.result(out,'spatial.field', spde, do.transf=TRUE)
#' range.out <- INLA::inla.emarginal(function(x) x, out.field$marginals.range.nominal[[1]])
#'
#' # parameters for the SLOO process
#' ss <- 1#sample size to process (number of SLOO runs)
#' rad <- range.out*0.15#define the radius of the spatial buffer surrounding the removed point
#' mesh <- mesh#use the mesh of the model
#' dataframe <- dataframe#dataframe with response 'y' and covariates 'x1', 'x2'
#' dataframe$y <- round(runif(length(dataframe$y), 1, 12))#create positive discrete response
#' modform <- stats::as.formula(paste('y ~ -1+ y.intercept + x1 + f(spatial.field, model=spde)'))
#' family <- list('gamma')#one model
#' ntrials <- rep(round(max(dataframe$y,na.rm=TRUE)*2),length(dataframe$y))# create ntrials for Binomial family
#' alpha <- 0.05#rmse and mae confidence intervals (1-alpha)
#'
#' # run the function
#' inlasloo(dataframe=dataframe, long='long', lat='lat',y= 'y', ss=ss, rad=rad,
#' modform=modform,mesh=mesh,family=family,alpha=0.05,mae=TRUE,ds=TRUE,sqroot=FALSE)
#'
#' # SLOO function with one model formula (Binomial) and linear terms
#' sloo1<-inlasloo(dataframe=dataframe, long='long', lat='lat', y='y', ss=1, rad=0.1,
#' ntrials=ntrials,modform='y ~ -1+ x1 + x2 + y.intercept + f(spatial.field, model=spde)',mesh=mesh,
#' family='binomial',alpha=0.05,mae=FALSE,ds=FALSE,sqroot=FALSE)
#' 
#' # SLOO function with two families (Binomial and Poisson) and linear terms
#' sloo2<-inlasloo(dataframe=dataframe, long='long', lat='lat', y='y', ss=1, rad=0.1,
#' ntrials=ntrials,modform='y ~ -1+ x1 + x2 + y.intercept + f(spatial.field, model=spde)',mesh=mesh,
#' family=list('binomial','poisson'),alpha=0.05,mae=TRUE,ds=FALSE,sqroot=FALSE)
#' 
#' # SLOO function with one family (Binomial) and two model formulas
#' sloo3<-inlasloo(dataframe=dataframe, long='long', lat='lat', y='y', ss=1,
#' rad=0.1, ntrials=ntrials,modform=list('y ~ -1+ y.intercept + f(spatial.field, model=spde)',
#' 'y ~ -1+ x1 + x2 + y.intercept + f(spatial.field, model=spde)'),
#' mesh=mesh,family='binomial',alpha=0.05,mae=TRUE,ds=FALSE,sqroot=FALSE)
#'  
#' # SLOO function with two families (Binomial and Poisson), two model formulas, 
#' # and MAE, DS, and sqroot activated
#' sloo4<-inlasloo(dataframe=dataframe, long='long', lat='lat', y='y', ss=1,
#' rad=0.1, ntrials=ntrials,modform=list('y ~ -1+ y.intercept + f(spatial.field, model=spde)',
#' 'y ~ -1+ x1 + x2 + y.intercept + f(spatial.field, model=spde)'),
#' mesh=mesh,list('binomial','poisson'),alpha=0.05,mae=TRUE,ds=TRUE,sqroot=TRUE)
#' }


inlasloo <- function(dataframe, long, lat, y, ss, rad, modform, 
                     family, mesh, ntrials = NULL, int.strategy = "eb", alpha = 0.05,
                     mae = FALSE, ds = FALSE, sqroot = FALSE, print = FALSE, plot= TRUE, ...) {
  message("Identification of input parameters values")
  message("#########################################", "\n")
  if (class(modform) != "list") {
    modform <- list(modform)  #if there is only one model make it as list
  } else {
    modform <- modform  # keep it as list for multiple models
  }
  if (class(family) != "list") {
    family <- list(family)  #if there is only one model make it as list
  } else {
    family <- family  # keep it as list for multiple models
  }
  famnames <- (paste(family, collapse = ","))
  message(paste("number of models =", length(modform) * length(family)))
  message(ifelse(missing(dataframe), "dataframe not specified", paste("number of rows in dataframe =", nrow(dataframe))))
  message(ifelse(missing(long), "long not specified", paste("longitude =", long)))
  message(ifelse(missing(lat), "lat not specified", paste("latitude =", lat)))
  message(ifelse(missing(y), "y not specified", paste("response =", y)))
  message(ifelse(missing(ss), "ss not specified", paste("sampling size =", ss)))
  message(ifelse(missing(rad), "rad not specified", paste("radius of disc of removed observations =", round(rad, 2))))
  message(ifelse(mae == TRUE, "RMSE and MAE computed", "RMSE computed"))
  message(ifelse(ds == TRUE, "DS computed", "DS not computed"))
  message(ifelse(sqroot == TRUE, "square root for RMSE and MAE computed", "square root for RMSE and MAE not computed"))
  message(ifelse(missing(family), "family not specified", paste("INLA family distribution of response =", famnames)))
  message(ifelse((family=="binomial"), 
                 ifelse(isTRUE(all(ntrials>0) & all(abs(ntrials - round(ntrials)) < .Machine$double.eps^0.5))==TRUE,
                        "number of trials provided for binomial family", "ntrials should be a vector of positive integers"),
                 ""))
  message(ifelse(missing(mesh), "mesh not specified", paste("number of mesh vertices =", mesh$n)))
  message(ifelse(int.strategy == "eb", "INLA integration strategy =  empirical bayes ", paste(int.strategy, ": user-defined INLA integration strategy")))
  message(ifelse(alpha != 0.05, paste("(1-alpha)", "", "% credible intervals of scores"), "default 95% credible intervals of scores"))
  message("End identification of input parameters values", "\n")
  message("#############################################", "\n")
  
  if (class(dataframe) != "data.frame") 
    stop("unexpected class instead of data.frame class for the argument \"dataframe\"")
  if (class(long) != "character") 
    stop("unexpected class instead of character class for the argument \"long\"")
  if (class(lat) != "character") 
    stop("unexpected class instead of character class for the argument \"lat\"")
  if (class(y) != "character") 
    stop("unexpected class instead of character class for the argument \"y\"")
  if (class(mesh) != "inla.mesh") 
    stop("unexpected class instead of inla.mesh class for the argument \"mesh\"")
  if (ss > nrow(dataframe)) 
    stop("sample size larger than number of observations in the dataframe")
  if (ss < 1) 
    stop("sample size smaller than 1")
  if (any(is.na(dataframe[, long]) == TRUE)) 
    stop("NA present in long")
  if (any(is.na(dataframe[, lat]) == TRUE)) 
    stop("NA present in lat")
  if (rad >= min(max(dataframe[, long]) - min(dataframe[, long]), max(dataframe[, lat]) - min(dataframe[, lat]))) 
    stop("radius (rad) equal or larger than geographic extent of the observed data")
  if (rad < 0) 
    stop("only non-negative values for the radius (rad) can be used")
  # end tests
  
  # start of the function
  # set values based on user input or default
  dataframe <- dataframe
  rad <- rad
  ss <- ss
  int.strategy <- int.strategy
  
  # data tranformation for further computing
  
  # Stupid way of getting correct column names without collisions (almost certainly).
  colnames(dataframe)[colnames(dataframe) == y] <- "tmp_y_column_name_eeeeee"
  colnames(dataframe)[colnames(dataframe) == long] <- "tmp_long_column_name_eeeeee"
  colnames(dataframe)[colnames(dataframe) == lat] <- "tmp_lat_column_name_eeeeee"
  
  colnames(dataframe)[colnames(dataframe) == "tmp_y_column_name_eeeeee"] <- "y"
  colnames(dataframe)[colnames(dataframe) == "tmp_long_column_name_eeeeee"] <- "long"
  colnames(dataframe)[colnames(dataframe) == "tmp_lat_column_name_eeeeee"] <- "lat"
  
  measurevar <- "y"  #replace response by 'y' in model formula
  for (i in 1:length(modform)) {
    modform[i] <- gsub(".*~", "", modform[i])
    modform[i] <- paste(measurevar, paste(modform[i]), sep = " ~ ")
  }
  
  # start of SLOO process
  test_rows <- sample(nrow(dataframe), ss)
  test.df <- dataframe[test_rows, ]  #sample subset of size ss from the dataframe
  test.ntrials <- ntrials[test_rows]
  slooresults <- list()
  slooresultsprint<- list()
  familys <- family
  modforms <- modform
  result.list <- expand.grid(familys, modforms)  #create all possible combinations of family and modforms
  result.list <- lapply(apply(result.list, 1, identity), unlist)  #put into a list
  for (j in 1:(length(result.list))) {
    family <- as.character(result.list[[j]][1])  #select family for j element
    modform <- stats::as.formula(as.character(result.list[[j]][2]))  #select modform for j element
    spde <- INLA::inla.spde2.matern(mesh, alpha = 2)  #SPDE computation
    
    for (i in 1:nrow(test.df)) {
      if (i == 1) {
        p <- c()
      }
      # Training data (test point & buffer removed)
      train <- dataframe[sqrt((dataframe$long - test.df[i, ]$long)^2 + (dataframe$lat - test.df[i, ]$lat)^2) > rad, ]
      # predictions in test locations
      predptdf <- test.df[i, ]  #predict on test point (one point only)
      A.pred <- INLA::inla.spde.make.A(mesh = mesh, loc = cbind(test.df[i, ]$long, test.df[i, ]$lat))
      stk.pred <- INLA::inla.stack(data = list(y = NA), A = list(A.pred, 1, 1, 1), effects = list(spatial.field = 1:spde$n.spde, 
                                                                                                  iid = 1:length(predptdf$y), y.intercept = rep(1, length(predptdf$y)), covariate = subset(predptdf, select = -c(y, 
                                                                                                                                                                                                                 long, lat))), tag = "pred.y")  #covariate: remove resp and lat long to keep just covariates
      
      # model at response location (not prediction)
      A <- INLA::inla.spde.make.A(mesh = mesh, loc = cbind(train$long, train$lat))
      covariate_all = subset(train, select = -c(y, long, lat))  #remove resp and lat long to keep just covariates
      stk.y <- INLA::inla.stack(data = list(y = train$y), A = list(A, 1, 1), effects = list(list(spatial.field = 1:spde$n.spde), 
                                                                                            list(iid = 1:length(train$y)), list(y.intercept = rep(1, length(train$y)), covariate = covariate_all)), tag = "est.y")
      # put all together prediction + at response level
      stk.full <- INLA::inla.stack(stk.y, stk.pred)
      datastk  <-  INLA::inla.stack.data(stk.full, spde = spde)
      
      if (family == "bernoulli") {
        out <- INLA::inla(modform, family = "binomial", Ntrials = 1, data = datastk, control.predictor = list(A = INLA::inla.stack.A(stk.full), 
                                                                                                              link = 1), control.inla = list(int.strategy = int.strategy), ...)
        indpred <- INLA::inla.stack.index(stk.full, "pred.y")$data
        pall <- round(out$summary.fitted.values[indpred, ], 3)  #get mean, sd, etc of predicted reponse at test.df location
        p <- c(p, as.numeric(pall[1]))  #first element is the mean of predictions
        
        
      } else if (family == "binomial") {
        ntrials_sub <- c(ntrials[sqrt((dataframe$long - test.df[i, ]$long)^2 + (dataframe$lat - test.df[i, ]$lat)^2) > rad], test.ntrials[i])
        out <- INLA::inla(modform, family = "binomial", Ntrials = ntrials_sub, data = datastk, control.predictor = list(A = INLA::inla.stack.A(stk.full), 
                                                                                                                        link = 1), control.inla = list(int.strategy = int.strategy), ...)
        indpred <- INLA::inla.stack.index(stk.full, "pred.y")$data
        pall <- round(out$summary.fitted.values[indpred, ], 3)  #get mean, sd, etc of predicted reponse at test.df location
        p <- c(p, as.numeric(pall[1]))  #first element is the mean of predictions
        
        
      } else {
        out <- INLA::inla(modform, family = family, data = datastk, control.predictor = list(A = INLA::inla.stack.A(stk.full), 
                                                                                             link = 1), control.inla = list(int.strategy = int.strategy), ...)
        indpred <- INLA::inla.stack.index(stk.full, "pred.y")$data
        pall <- round(out$summary.fitted.values[indpred, ], 3)  #get mean, sd, etc of predicted reponse at test.df location
        p <- c(p, as.numeric(pall[1]))  #first element is the mean of predictions
      }
    }  #end of i loop
    
    if (sqroot == TRUE & any(test.df$y < 0) == FALSE & any(p < 0) == FALSE) {
      # compute the sqrt of predicted and observed for gamma function
      p.res <- sqrt(test.df$y) - sqrt(p)
    } else {
      p.res <- test.df$y - p
    }
    
    p.rmse <- sqrt(mean(p.res^2, na.rm = TRUE))
    
    if (mae == TRUE) {
      p.mae <- mean(abs(p.res), na.rm = TRUE)
    } else {
      p.mae <- NA
    }
    if (ds == TRUE) {
      p.ds <- mean((test.df$y - (mean(p, na.rm = TRUE) * stats::sd(p, na.rm = TRUE)))^2 + 2 * log(stats::sd(p, na.rm = TRUE)), na.rm = TRUE)
    } else {
      p.ds <- NA
    }  #good for general predictive model choice; stable for size imbalance in categorical predictors)
    
    slooresults[[j]] <- list(Observed_response = test.df$y, Predictions = p, Residuals = p.res, RMSE = p.rmse, 
                             MAE = p.mae, DS = p.ds, family = family, ntrials = test.ntrials, test.df = test.df, Rownames_test = as.numeric(rownames(test.df)))
    
    slooresultsprint <- slooresults[[j]]
    slooresultsprint[[1]]<-round(as.numeric(slooresultsprint[[1]]),3)
    slooresultsprint[[2]]<-round(as.numeric(slooresultsprint[[2]]),3)
    slooresultsprint[[3]]<-round(as.numeric(slooresultsprint[[3]]),3)
    slooresultsprint[[4]]<-round(as.numeric(slooresultsprint[[4]]),3)
    slooresultsprint[[5]]<-round(as.numeric(slooresultsprint[[5]]),3)
    slooresultsprint[[6]]<-round(as.numeric(slooresultsprint[[6]]),3)
    if (print == TRUE) {
      message("\n")
      message("Summary of the Spatial leave-one-out analysis")
      message("#############################################", "\n")
      message("MODEL", "", j, "\n")
      print(slooresultsprint[-c(9:10)])
      message("End summary of the Spatial leave-one-out analysis")
      message("#################################################", "\n")
    }
  }  #end of j loop
  if (plot == TRUE) {
    
  # plot locations of observation and test points
  par(mfrow = grDevices::n2mfrow(length(result.list)), family = "mono", mar=c(5,4,4,2) + 0.1,oma = c(0, 0, 2, 0))
  for (j in 1:(length(result.list))) {
    slooplot(alpha = alpha, df = slooresults[[j]], mae = mae, ds = ds, family = as.character(result.list[[j]][1]), ntrials = test.ntrials, 
             sqroot = sqroot)
    graphics::mtext(paste0("MODEL", "", j), side = 3, adj = 1, cex = 0.8, line = 1.6)
  }
  graphics::title(paste("SLOO with number of iterations =", "", ss), outer = TRUE)
  # end plot
  # plot locations of observation and test points
  par(mfrow = c(1, 1), family = "mono", mar=c(5,4,4,2) + 0.1,oma = c(2, 2, 2, 2))
  sloopoint(points = cbind(dataframe$long, dataframe$lat), test = cbind(slooresults[[1]]$test.df$long, slooresults[[1]]$test.df$lat), 
            rad = rad)
  }
  return(slooresults)#save the results as an object
  }

