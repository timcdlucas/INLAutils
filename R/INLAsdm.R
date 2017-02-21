
#create k-fold function that can accept user defined groups? 
#Need to do stepwise within? I guess to robust check parameters - how to combine output.
#Let it take presences/y and then absence makes it binomial
#kfold just presences if not an SDM
#doesnt need spatial or not
#spit out sum CPO as well as DIC and wAIC, pred-obs data, standardised residuals
#how to deal with invariant??

#' Fit INLA species distribution models.
#' 
#'@param dataframe A \code{\link[sp]{SpatialPointsDataFrame}}
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
#'@export

##without dismo
inlaSDM<-function(dataframe,
                  predictors, 
                  include = 1:raster::nlayers(predictors),
                  step = FALSE,
                  invariant = "0 + Intercept",
                  cross_validation = FALSE,
                  cv_folds = 5,
                  spatial = TRUE,
                  num.threads = 1,
                  meshvals = list(minME = max(raster::res(predictors)) * 10, 
                                  maxME = max(raster::res(predictors)) * 100, 
                                  co = 0, 
                                  minOS = -0.1,
                                  maxOS = -0.3)
                  ){
  
    
  # Deal with names of response column.
  assert_that(ncol(dataframe) == 1)
  y <- 'y'
  names(dataframe) <- 'y'
  
  if(!cross_validation) cv_folds <- 1
  
  # Empty list for all inla models
  models <- list()
  # Empty data frame for summaries
  model_res <- as.data.frame(matrix(NA, ncol = 4, nrow = cv_folds))
  names(model_res) <- c('replicate', 'AUC', 'WAIC', 'comp_time')
  
  
    
  for(cv in cv_folds){
      
    
    
    if(cross_validation==TRUE){
      group <- dismo::kfold(dataframe, 5)
      train <- dataframe[group != cv, ]
      test  <- dataframe[group == cv, ]
      test@data[, y] <- NA
      dataf1 <- rbind(train,test)
    } else { # Not sure how this works
      dataf1 <- dataframe
    }
    
      
      
    
    
    ###add training data and testing data (NAs) together
    sss <- as.data.frame(raster::extract(predictors, dataf1))
    sss <- cbind(sss, dataf1@data[, y])
    names(sss)[NCOL(sss)] <- y
    
    # Why?
    sss <- sss[complete.cases(sss), ]
    dataf1@data<-sss
    
    if(step == TRUE){
      # Data is a raster (or at least some s4 thing).
      stepINLA <- stepINLA(fam1 = "binomial", #?
                          dataf1,
                          invariant = invariant,
                          direction = 'backwards',
                          include = 1:ncol(dataf1),
                          y = y,
                          y2 = y,
                          in_stack = NULL,
                          powerl = 1,
                          inter = 1,
                          thresh = 2,
                          Ntrials = ,
                          num.threads = num.threads)
      
      form1 <- paste0(as.character(stepINLA$best.formula), collapse = FALSE)
      
    } else {
      # Make formula of all covariates but not
      form1 <- paste0(y, ' ~ ', invariant, ' + ', paste(names(predictors), collapse = ' + '))
    }
    
    if(spatial==TRUE){
      ##make mesh
      mesh5 <- inla.mesh.2d(loc = sp::coordinates(dataf1), 
                            max.edge = c(meshvals$minME, meshvals$maxME), 
                            cutoff = meshvals$co, 
                            offset = c(meshvals$minOS, meshvals$maxOS))
      
      ####The SPDE model is defined 
      spde <- inla.spde2.matern(mesh5, alpha=2)
      
      ###projector matrix for known
      A <- inla.spde.make.A(mesh5, loc = sp::coordinates(dataf1))
      
      ###make index for spatial field
      s.index<-inla.spde.make.index(name="spatial.field",n.spde=spde$n.spde)
      
      ##and the stack data is defined to include effects IMPORTANT they have same names as columns so the formula below will work
      #if(bin1==FALSE){dataf1$Presence<-dataf1$p; fam1=fam2}
      
      # Assumes invariant has `Intercept`
      stk.est <- inla.stack(data = list(y = dataf1$y),
                            A = list(A, 1), 
                            effects = list(c(s.index,list(Intercept = 1)),
                                         list(dataf1@data[, names(dataf1) != y])),
                            tag = 'est')
      
      ###projector matrix for known
      A.val <- inla.spde.make.A(mesh5, loc = sp::coordinates(dataf1))
      
      ##and the stack data is defined to include effects IMPORTANT they have same names as columns so the formula below will work
      # Todo What is this for!
      # Guessing it's supposed to be the test data? But not defined while taking into account the cross_validation groups above
      # 
      # stk.val <- inla.stack(data=list(y=NA),
      #                       A=list(A.val,1), 
      #                       effects=list(c(s.index, list(Intercept=1)), 
      #                                    list(dataf1@data[, names(dataf1) != y])), tag='val')
      # 
      # join.stack<-inla.stack(stk.est,stk.val)
      ntt <- rep(1, nrow(dataf1))
      
      # # What is predict1?
      # if(!predict1==TRUE){
      #   join.stack<-stk.est
      #   ntt=dataf1$trials
      # }
      
      # Create formula object adding the spatial.field random effect
      form1 <- formula(paste(form1, ' + f(spatial.field, model = spde)'))
      
      # Fit spatial inla model.
      res5<-inla(form1,
                 data=inla.stack.data(stk.est, spde=spde),
                 family="binomial",
                 Ntrials=ntt,
                 control.predictor=list(A=inla.stack.A(stk.est),compute=TRUE),
                 control.compute=list(cpo=TRUE,waic=TRUE,dic=TRUE),
                 control.fixed = list(expand.factor.strategy = "inla"),
                 num.threads=num.threads,
                 silent=TRUE)
      
      
    }else{
      
      # Assumes invariant has `Intercept`
      stk.est <- inla.stack(data=list(y=dataf1$y),
                            A=list(1), 
                            effects=list(data.frame(Intercept = 1, dataf1@data[, names(dataf1) != y])),
                            tag='est')
      
      
      ##and the stack data is defined to include effects IMPORTANT they have same names as columns so the formula below will work
      # Todo W  hat is this for!
      # Guessing it's supposed to be the test data? But not defined while taking into account the cross_validation groups above
      # 
      # stk.val <- inla.stack(data=list(y=NA),
      #                       A=list(1), 
      #                       effects=list(data.frame(Intercept = 1, dataf1@data[, names(dataf1) != y])),
      #                       tag = 'val')
      
      
      ntt <- rep(1,nrow(dataf1))
      
      # Create formula object
      form1 <- formula(form1)
      # Fit unspatial inla model
      res5<-inla(form1,
                 data=inla.stack.data(stk.est),
                 family="binomial",
                 Ntrials=ntt,
                 control.compute=list(cpo=TRUE, waic=TRUE, dic=TRUE),
                 control.fixed = list(expand.factor.strategy = "inla"),
                 num.threads=num.threads,
                 silent=TRUE)
    }
    
    
    inla.time<-(res5$cpu.used)
    waic<-(res5$waic$waic)
    dic<-(res5$dic$dic)
    cpo.fit<-(sum(log(res5$cpo$cpo),na.rm=T))
    
    ##put it all together ## replace with function!!!!
    res1<-data.frame(row.names=rownames(res5$summary.fixed),res5$summary.fixed)
    res1$sig<-"non-sig"##is a term significant? i.e. does it include 0 in distribution
    res1$sig[(res1$X0.025quant<0 & res1$X0.975quant<0)|(res1$X0.025quant>0 & res1$X0.975quant>0)]<-"sig"
    #res1<-res1[res1$sig=="sig",]
    
    #predicted.p.value<-c()
    #n<-length(dataf1@data[,1])
    #for(i in (1:n)){
    #predicted.p.value[i]<-inla.pmarginal(q=dataf1$Presence[i],marginal=res5$marginals.fitted.values[[i]])}
    #check1<-data.frame(dataf1$Presence,res5$summary.fitted.values$mean[1:length(dataf1$p)])
    #plot(dataf1$Presence,res5$summary.fitted.values$mean[1:length(dataf1$p)],xlab="Observed",ylab="Fitted")
    #hist(predicted.p.value)
    #dim(predict_rast@data)
    #graphics.off()
    #U-shaped histograms indicate under-dispersed predictive distributions, hump or inverse-U shaped histograms point at overdispersion, and skewed histograms occur when central tendencies are biased
    
    dataf1@data <- cbind(dataf1@data[,1:ncol(dataf1)],
                         res5$summary.fitted.values[(nrow(dataf1)+1):(nrow(dataf1) + nrow(dataf1)),])
    
    
    #predict_rast@data$antimean<-exp(exp(predict_rast@data$mean)+1)-1
    #head(res5$summary.fitted.values[(nrow(dataf1)+1):(nrow(dataf1)+nrow(predict_rast)),])
    
    # test1<-rasterize(predict_rast2,predictxm2,field="mean",mean)
    # names(test1)<-paste("Quarter_",QQ,"_ONI_",ONI,sep="")
    
    if(cross_validation==TRUE){
      c3 <- as.data.frame(res5$summary.fitted.values[1:nrow(dataf1), c("mean", "sd") ])
      c3$p_pred<-c3$mean#round(c3$mean,0)
      c3$p_real<-rbind(pres_train,backg_train,pres_test,backg_test,datat, datatb)$Presence
      ###internal testing points
      c5<-c3[(nrow(pres_train)+nrow(backg_train))+1:(nrow(pres_train)+nrow(backg_train)+nrow(pres_test)+nrow(backg_test)),]
      ##evaluate INLA output ## full predict
      e5b <-dismo::evaluate(p=c5[c5$p_real==1,"p_pred"],a=c5[c5$p_real==0,"p_pred"])
    } else {
      e5b <- NA
    }
    
    model_res[cv, ] <- c(replicate=cv,
                         #sens_run=xx,
                         #layersx[1,],
                         AUC=e5b,
                         WAIC=res5$waic$waic,
                         #arguements=paste(args1,collapse=" "),
                         #beta=beta1,
                         comp_time=inla.time[['Total']])
                        
    models[[cv]] <- res5
  }## end cv
  
  output <- list(model_res, models)
  class(output) <- 'inlaSDM'

  return(output)
}
