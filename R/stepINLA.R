
#' Perform stepwise variable selection with INLA
#' 
#' Run forwards or backwards stepwise variable selection with INLA regression.
#' Use hold data to test new models.
#' Chose the level of interactions and power terms
#' Force part of the model to remain in the model (e.g. the spatial term and intercept).
#' 
#'@name stepINLA
#'@param fam1 String defining the likelihood familiy
#'@param dataf A SpatialPointsDataFrame including covariates and response data.
#'@param spde An spde model object for the spatial component
#'@param in_stack An inla.data.stack object containing all needed data
#'@param invariant The part of the formula that should not change (e.g. the intercept and the spatial component.)
#'@param direction string 'forwards' for forward variable selection and 'backwards' for backwards variable elimination.
#'@param include Vector of integers to determine which columns in dataf should be used.
#'@param y String determining the response variable.
#'@param y2 String determining the name of the test response data.
#'@param powerl Integer up to 3 determining which power terms to include.
#'@param inter Integer up to 3 determining how many levels of intereactions to include. 
#'@param thresh Threshold for whether a new model should replace the old model.
#'@param Ntrials Not sure.
#'@param num.threads How many threads to use for INLA computation.
#'@importFrom stats formula
#'@export
#@examples todo

stepINLA<-function(fam1="gaussian",
                   dataf,
                   spde = NULL,
                   in_stack=NULL,
                   invariant="0 + Intercept",
                   direction=c("forwards","backwards"),
                   include=1:ncol(dataf),
                   y=NULL,
                   y2=NULL,
                   powerl=1,
                   inter=1,
                   thresh=2,
                   Ntrials=NULL,
                   num.threads=1) {
  
  z<-NULL
  
  # Basic checks
  if(is.null(nrow(dataf))){stop("error in data frame")}
  if(nrow(dataf)==0){stop("no rows in data frame")}
  if(is.null(y)){stop("no y variable")}
  if(is.null(in_stack)){stop("no stack defined")}
  if(!(class(dataf)=="data.frame"|class(dataf)=="SpatialPointsDataFrame")){stop("data not data frame")}
  if(!class(in_stack)=="inla.data.stack"){stop("in_stack not INLA stack")}
  
  
  facts<-sapply(dataf@data, is.factor)[include]
  explF<-names(dataf)[include]
  expl<-explF[!facts]
  
  # Sort and combine all different variable types (should refactor and then test nicely.)
  #   Think its function(expl, powerl, inter) return(expl)

  if(length(expl)>0){
    if(powerl==2){expl2<-paste("I(",expl,"^2)",sep="");expl3<-NULL;expl4<-NULL;explX<-c(expl,expl2,expl3,expl4)}
    if(powerl==3){expl2<-paste("I(",expl,"^2)",sep="");expl3<-paste("I(",expl,"^3)",sep="");expl4<-NULL;explX<-c(expl,expl2,expl3,expl4)}
    if(powerl>=4){expl2<-paste("I(",expl,"^2)",sep="");expl3<-paste("I(",expl,"^3)",sep="");expl4<-paste("I(",expl,"^4)",sep="");explX<-c(expl,expl2,expl3,expl4)}
    if(powerl>1){expl<-explX}
    if(inter>=2){
      lvls<- data.frame(p1=utils::combn(expl,2)[1,],
                        p2=utils::combn(expl,2)[2,])
      lvls2<-do.call(paste, c(lvls[names(lvls)], sep=":"))
      expl2<-c(expl,lvls2)
    }
    if(inter>=3){
      lvls<- data.frame(p1=utils::combn(expl,3)[1,],
                        p2=utils::combn(expl,3)[2,],
                        p3=utils::combn(expl,3)[3,])
      lvls3<-do.call(paste, c(lvls[names(lvls)], sep=":"))
      expl2<-c(expl2,lvls3)
    }
    if(inter>=4){
      lvls<- data.frame(p1=utils::combn(expl,4)[1,],
                        p2=utils::combn(expl,4)[2,],
                        p3=utils::combn(expl,4)[3,],
                        p4=utils::combn(expl,4)[4,])
      lvls4<-do.call(paste, c(lvls[names(lvls)], sep=":"))
      expl2<-c(expl2,lvls4)
    }
    if(inter>1){expl<-expl2}
  }
  if(length(explF[facts])>0){expl<-c(expl,explF[facts])}
  
  choice<-NULL;chosen<-NULL; new1<-NULL;dicloss<-999; dicold<-NULL
  
  
  ###keep looping until nothing is gained 
  while(length(expl)>0){
    # If backwards.... ? 
    if(direction=="backwards"){
      runs<-c(1:length(expl),9999999)
    }else{
      runs<-1:length(expl)
    }
    
    for (ii in runs){
      if(direction=="backwards"){
        if(ii==9999999){
          ii<-1:length(expl)
        }else{
          ii <- {-1 * ii} # Drop each variable in turn. Final run is ii == 9999 and therefore do all variables.
        }
      }
      
      #print(ii)
      # Refactor function(chose, invariant, expl, ii) return(formula2)
      if(is.null(chosen)){
        if(length(expl[ii]) > 0){
          formula2 <- formula(paste(y,"~",invariant,"+",paste(expl[ii],collapse="+"),sep=""))
        } else {
          formula2 <- formula(paste(y,"~",invariant))
        }
      } else{
        if(length(expl[ii]) > 0){
          formula2 <- formula(paste(y,"~",invariant,"+",chosen," + ",expl[ii],sep=""))
        } else {
          formula2 <- formula(paste(y,"~",invariant,"+",chosen))
        }
      }
      
      print(formula2)
      result2 <- INLA::inla(formula2,
                            family=fam1,
                            num.threads=num.threads,
                            Ntrials=Ntrials,
                            control.compute=list(cpo=TRUE,dic=TRUE,waic=TRUE),
                            verbose=FALSE,
                            data=inla.stack.data(in_stack,spde=spde),
                            control.predictor=list(A=inla.stack.A(in_stack), compute=TRUE),
                            control.fixed = list(expand.factor.strategy = "inla"))
      
      rmse<- sqrt(mean((dataf@data[,y2]-result2$summary.fitted.values$mean[1:nrow(dataf)])^2,na.rm=TRUE))
      sumcpo<-sum(log(result2$cpo$cpo),na.rm=TRUE)
      
      if(length(ii)>1){
        var1<-paste(expl[ii],collapse="+")
      }else{
        var1<-expl[abs(ii)]
      }
      if(is.null(choice)){
        choice <- data.frame(var=var1,aic=result2$waic$waic,rmse,sumcpo,stringsAsFactors=FALSE)
      }else{
        choice <- rbind(choice,data.frame(var=var1,aic=result2$waic$waic,rmse,sumcpo,stringsAsFactors=FALSE))
      }
      
    }##end of run through
    
    new1<-choice[choice$aic==min(choice$aic,na.rm=TRUE)[1],1][1]
    if(!is.null(dicold)){dicloss<-dicold-(min(choice$aic,na.rm=TRUE)[1])}
    dicold<-choice[choice$aic==min(choice$aic,na.rm=TRUE)[1],2]
    
    if(is.null(z)){
      progress<-choice[choice$var==new1,]
      z<-1
    }else{
      progress<-rbind(progress,choice[choice$var==new1,])
    }
    
    print(paste(new1," - ",min(choice$aic,na.rm=TRUE)),sep="")
    choice<-NULL
    if(dicloss>thresh){
      if(direction=="backwards"){expl<-expl[!expl==new1]}
      if(direction=="forwards"){
        if(is.null(chosen)){
          chosen<-new1;expl<-expl[!expl==new1]
        }else{
          chosen<-paste(chosen," + ",new1,sep="");expl<-expl[!expl==new1]
        }
      }
    } else {
      break
    }
    
  }
  
  if(direction=="backwards"){
    formulax <- formula(paste(y,"~",invariant,"+",paste(expl[ii],collapse="+"),sep=""))
  } else {
    formulax <- formula(paste(y,"~",invariant,"+",chosen,sep=""))
  }
  
  
  return(list(best_formula=formulax, waic=dicold, progress=progress, best_model = result2))			
  
}##end of function
