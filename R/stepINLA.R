


stepINLA <- function(fam1="gaussian", 
                     dataf, 
                     invariant="0 + Intercept", 
                     direction=c("forwards","backwards"),
                     include=1:nco(data),
                     y=NULL,
                     y2=NULL,
                     in_stack=NULL,
                     powerl=1,
                     inter=1,
                     thresh=2,
                     Ntrials=NULL,
                     num.threads=1) {

  z<-NULL

  if(is.null(nrow(dataf))){return(print("error in data frame"))}
  if(nrow(dataf)==0){return(print("no rows in data frame"))}
  if(is.null(y)){return(print("no y variable"))}
  if(is.null(in_stack)){return(print("no stack defined"))}
  if(!(class(dataf)=="data.frame"|class(dataf)=="SpatialPointsDataFrame")){return(print("data not data frame"))}
  if(!class(in_stack)=="inla.data.stack"){return(print("in_stack not INLA stack"))}

  facts<-sapply(dataf@data, is.factor)[include]
  explF<-names(dataf)[include]
  expl<-explF[!facts]
  if(length(expl)>0){
	  if(powerl==2){expl2<-paste("I(",expl,"^2)",sep="");expl3<-NULL;expl4<-NULL;explX<-c(expl,expl2,expl3,expl4)}
	  if(powerl==3){expl2<-paste("I(",expl,"^2)",sep="");expl3<-paste("I(",expl,"^3)",sep="");expl4<-NULL;explX<-c(expl,expl2,expl3,expl4)}
	  if(powerl>=4){expl2<-paste("I(",expl,"^2)",sep="");expl3<-paste("I(",expl,"^3)",sep="");expl4<-paste("I(",expl,"^4)",sep="");explX<-c(expl,expl2,expl3,expl4)}
	  if(powerl>1){expl<-explX}
	  if(inter>=2){lvls<- data.frame(p1=combn(expl,2)[1,],p2=combn(expl,2)[2,]);lvls2<-do.call(paste, c(lvls[names(lvls)], sep=":"));expl2<-c(expl,lvls2)}
	  if(inter>=3){lvls<- data.frame(p1=combn(expl,3)[1,],p2=combn(expl,3)[2,],p3=combn(expl,3)[3,]);lvls3<-do.call(paste, c(lvls[names(lvls)], sep=":"));expl2<-c(expl2,lvls3)}
	  if(inter>=4){lvls<- data.frame(p1=combn(expl,4)[1,],p2=combn(expl,4)[2,],p3=combn(expl,4)[3,],p4=combn(expl,4)[4,]);lvls4<-do.call(paste, c(lvls[names(lvls)], sep=":"));expl2<-c(expl2,lvls4)}
	  if(inter>1){expl<-expl2}
			  }
  if(length(explF[facts])>0){expl<-c(expl,explF[facts])}

  choice<-NULL;chosen<-NULL; new1<-NULL;dicloss<-999; dicold<-NULL
  ###keep looping until nothing is gained 

  while(length(expl)>0){
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
          ii<--1*ii
        }
      }
		  if(is.null(chosen)){
        formula2 <- formula(paste(y,"~",invariant,"+",paste(expl[ii],collapse="+"),sep=""))
      }else{
			  formula2 <- formula(paste(y,"~",invariant,"+",chosen," + ",expl[ii],sep=""))
      }

		  result2 <- inla(formula2,
                   family=fam1,
                   num.threads=num.threads,
                   Ntrials=Ntrials,
                   control.compute=list(cpo=TRUE,dic=TRUE,waic=TRUE),
                   verbose=FALSE,
                   data=inla.stack.data(in_stack,spde=spde),
                   control.predictor=list(A=inla.stack.A(in_stack), compute=TRUE),
                   control.fixed = list(expand.factor.strategy = "inla"))
		  rmse <- sqrt(mean((dataf@data[,y2]-result2$summary.fitted.values$mean[1:nrow(dataf)])^2,na.rm=T))
		  sumcpo<-sum(log(result2$cpo$cpo),na.rm=T)

		  if(length(ii)>1){var1<-paste(expl[ii],collapse="+")}else{var1<-expl[abs(ii)]}
		  if(is.null(choice)){choice=data.frame(var=var1,aic=result2$waic$waic,rmse,sumcpo,stringsAsFactors=F)}else{choice=rbind(choice,data.frame(var=var1,aic=result2$waic$waic,rmse,sumcpo,stringsAsFactors=F))}

					  }##end of run through

	  new1<-choice[choice$aic==min(choice$aic,na.rm=T)[1],1][1]
	  if(!is.null(dicold)){dicloss<-dicold-(min(choice$aic,na.rm=T)[1])}
	  dicold<-choice[choice$aic==min(choice$aic,na.rm=T)[1],2]
	  if(is.null(z)){
      progress<-choice[choice$var==new1,]
      z<-1
    }else{
      progress<-rbind(progress,choice[choice$var==new1,])
    }
	  print(paste(new1," - ",min(choice$aic,na.rm=T)),sep="")
	  choice<-NULL
    if(dicloss>thresh){
	    if(direction=="backwards"){expl<-expl[!expl==new1]}
	    if(direction=="forwards"){
        if(is.null(chosen)){
          chosen<-new1;expl<-expl[!expl==new1]}else{chosen<-paste(chosen," + ",new1,sep="");expl<-expl[!expl==new1]
        }
      }
	  }else{
      break
    }
	
  }

  if(direction=="backwards"){
    formulax <- formula(paste(y,"~",invariant,"+",paste(expl[ii],collapse="+"),sep=""))
  }else{
		formulax <- formula(paste(y,"~",invariant,"+",chosen,sep=""))
  }

  return(list(best_formula=formulax,waic=dicold,progress=progress))			

}##end of function
