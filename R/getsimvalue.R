#' @title Get Simulated Values
#' @description Simulate values for use in optimization.  This function is used to obtain draws from a distribution for the sensitivity and specificity
#' for each reference test and for the prevalence of each population tested.
#' @param means \code{vector} A named vector containing point estimates for the prevalence of each population (when prevalence = TRUE, see below) or a data frame where each column
#' corresponds to a reference test and the rows are sensitivity (\eqn{\pi}) and \eqn{\psi} (or specificity (\eqn{\theta})  and \eqn{\phi}).
#' @param distn \code{vector}  A vector of same length as \code{means}. Values may be one
#' of \code{NULL}, 'beta', or 'triangular'. \code{NULL} will be treated as 'beta'
#' @param spread \code{vector} A vector of same length as \code{means}. Values may be one of \code{NULL},
#' 'wide', 'medium', or 'narrow'. \code{NULL} will be treated as 'wide'.
#' @param nsim The number of simulations to draw from the sensitivity and specificity distribution(s) for each reference test or the prevalence
#' distribution from each population.
#' @param step.size Provides the level of resolution in values simulated from a triangular distribution.
#' @param prevalence logical (TRUE/FALSE)  TRUE indicates that the function is simulating values of prevalence.  This will determine the structure of the output.
#' @return final.mat A matrix of simulated values.  If prevalence is TRUE, final.mat will have the number of columns corresponding to the number of populations
#' sampled else if prevalence is FALSE, final.mat will have number of columns twice the number of reference tests.  The columns are sensitivity (or specificity) of the first reference test,
#' the probability of a suspect result as a fraction of the non-correct test result (i.e. either \eqn{\delta} or \eqn{\gamma}) for the first reference and continues in the same pattern for all reference tests.
# @author Monica Reising \email{monica.m.reising@@aphis.usda.gov}
#' @author \link{DiagTestKit-package}
get.simulated.values<-function(means,distn,spread,nsim,step.size,prevalence){
  final.mat<-NULL

  if(prevalence==TRUE) means<-matrix(means,nrow=1)

  if(is.null(distn) & is.null(spread)){
    #the default distribution is going to be a "wide" beta
    for(i in 1:ncol(means)){
      alpha.beta<-betaParm(B=c(mu=means[1,i],sigma2=0.002))
      current.draws<-rbeta(nsim,shape1=alpha.beta[1],shape2=alpha.beta[2])
      if(prevalence==FALSE) final.mat<-cbind(final.mat,current.draws,rep(means[2,i]))
      if(prevalence==TRUE) final.mat<-cbind(final.mat,current.draws)
    }
  } else if(!is.null(distn) & !is.null(spread)){
    for(i in 1:ncol(means)){
      if(distn[i]=='beta'){
        s2<-ifelse(spread[i]=='wide',0.002,ifelse(spread[i]=='medium',0.004,ifelse(spread[i]=='narrow',0.0001,stop('Spread must be wide, medium, or narrow'))))
        alpha.beta<-betaParm(B=c(mu=means[1,i],sigma2=s2))
        current.draws<-rbeta(nsim,shape1=alpha.beta[1],shape2=alpha.beta[2])
        if(prevalence==FALSE) final.mat<-cbind(final.mat,current.draws,rep(means[2,i],nsim))
        if(prevalence==TRUE) final.mat<-cbind(final.mat,current.draws)

      } else if(distn[i]=='triangular'){
        if(spread[i]=='wide'){
          omega<-c(0.08,0.08,0.06)
          hi<-c(2,1.6)

        } else if(spread[i]=='medium'){
          omega<-c(0.02,0.03,0.08)
          hi<-c(2,1.2)
        } else if(spread[i]=='narrow'){
          omega<-c(0.005,0.01,0.01)
          hi<-c(2,0.5)
        } else{
          stop('Spread must be wide, medium, or narrow')
        }

        values.to.sample<-SampDist(m=means[1,i],w=omega,h=hi,stepwidth=step.size)
        current.draws<-sample(x=values.to.sample$x,size=nsim,prob=values.to.sample$p,replace=T)
        if(prevalence==FALSE) final.mat<-cbind(final.mat,current.draws,rep(means[2,i],nsim))
        if(prevalence==TRUE) final.mat<-cbind(final.mat,current.draws)
      }else{
        stop('Distribution must be beta or triangular')
      }

    }
  } else if(is.null(distn) & !is.null(spread)){
    for(i in 1:ncol(means)){
      s2<-ifelse(spread[i]=='wide',0.002,ifelse(spread[i]=='medium',0.004,ifelse(spread[i]=='narrow',0.0001,stop('Spread must be wide, medium, or narrow'))))
      alpha.beta<-betaParm(B=c(mu=means[1,i],sigma2=s2))
      current.draws<-rbeta(nsim,shape1=alpha.beta[1],shape2=alpha.beta[2])
      if(prevalence==FALSE) final.mat<-cbind(final.mat,current.draws,rep(means[2,i],nsim))
      if(prevalence==TRUE) final.mat<-cbind(final.mat,current.draws)

    }
  } else if(!is.null(distn) & is.null(spread)){
    #this will defalut to wide
    for(i in 1:ncol(means)){
      if(distn[i]=='beta'){
        alpha.beta<-betaParm(B=c(mu=means[1,i],sigma2=0.002))
        current.draws<-rbeta(nsim,shape1=alpha.beta[1],shape2=alpha.beta[2])
        if(prevalence==FALSE) final.mat<-cbind(final.mat,current.draws,rep(means[2,i]))
        if(prevalence==TRUE) final.mat<-cbind(final.mat,current.draws)
      } else if(distn[i]=='triangular'){
        omega<-c(0.08,0.08,0.06)
        hi<-c(2,1.6)
        values.to.sample<-SampDist(m=means[1,i],w=omega,h=hi,stepwidth=step.size)
        current.draws<-sample(x=values.to.sample$x,size=nsim,prob=values.to.sample$p,replace=T)
        if(prevalence==FALSE) final.mat<-cbind(final.mat,current.draws,rep(means[2,i],nsim))
        if(prevalence==TRUE) final.mat<-cbind(final.mat,current.draws)

      } else{
        stop('Distribution must be beta or triangular')
      }
    }
  }
  colnames(final.mat) <- NULL ## remove colnames that are an artifact of cbind
  return(final.mat)
}
