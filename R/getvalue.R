#' @title Optimization of Sensitivity and Specificity
#' @description Determine final optimized values for the sensitivity and specificity of an experimental test kit (and probability of suspect given disease positive and given disease negative for a 3-state kit).
#' @param dat \code{vector} A vector of counts ordered in a manner consistent with output from the cellS function.
#' @param SnR.vec \code{data.frame}  Each column corresponds to one reference test.  Row 1 contains the sensitivity for the reference test(s).
#' Row 2 contains a value between 0 and 1 (inclusive) representing the probability of a suspect result as a fraction of the non-correct test result.
#' P(T? | D+) = \eqn{\psi} = \eqn{\delta} * (1 - \eqn{\pi}) where \eqn{\delta} is the second row for a given column (reference test).  \eqn{\delta = \frac{\psi}{(1 - \pi)}}.  Use a zero for a 2-state
#' test (i.e. no suspect region).
#' @param SpR.vec \code{data.frame} Each column corresponds to one reference test.  Row 1 contains the specificity for the reference test(s).
#' Row 2 contains a value between 0 and 1 (inclusive) representing the probability of a suspect result as a fraction of the non-correct test result.
#' P(T? | D-) = \eqn{\phi} = \eqn{\gamma} * (1 - \eqn{\theta}) where \eqn{\gamma} is the second row for a given column (reference test). \eqn{\gamma = \frac{\phi}{(1 - \theta)}}.  Use a zero for a 2-state
#' test (i.e. no suspect region).
#' @param prev.vec \code{vector}  A named vector containing the prevalence for each population sampled.
#' @param N.vec \code{vector}  A named vector containing the sample size for each population sampled.
#' @param nstates \code{vector} A vector with length one more than the number of reference tests.  The first element is the number of states of the experimental test and the remaining entries are the number
#' of states of each reference test (using the same ordering as SnR.vec and SpR.vec).
#' @param tolerance Setting a limit on the pgtol used in the optim function with the 'L-BFGS-B' method. See also \code{\link{optim}}.
#' @param rep.iter logical (TRUE/FALSE)  Indicates if updates should be printed regarding the number of iterations completed.
#' @param iter.n  integer indicating the frequency of updates for the number of iterations completed.
#' @param parm \code{vector}  A vector of starting values to be used for the optimization that is passed to \code{minCell}.  For a 2-state experimental test, this is a vector of length 2 with entries (\eqn{\pi}, \eqn{\theta}). 
#' For a 3-state experimental test, this is a vector of length 4 with entries (\eqn{\pi}, \eqn{\delta}, \eqn{\theta}, \eqn{\gamma}). See also \code{\link{estimateSnSp}}.
#' @return A list: \cr \cr
#' The following will be returned for both 2 and 3-state experimental tests -- \cr
#' \itemize{
#' \item{sens.final}  \code{vector} The optimized values for the sensitivity of the experimental test kit.
#' \item{spec.final}  \code{vector} The optimized values for the specificity of the experimental test kit.
#' \item{converge}  \code{vector} Each entry is an integer code detailing the convergence of the optimization for each iteration.  0 indicates successful completion. See also \code{\link{optim}}.
#' \item{message}  \code{vector}  Each entry includes a character string giving any additional information returned by the optimizer or NULL.  See also \code{\link{optim}}.
#' }
#'
#' If three states -- \cr
#' \itemize{
#' \item{\eqn{\delta}}  \code{vector} The optimized values for the probability of a suspect result as a fraction of the non-correct test result for diseased samples.
#' \item{\eqn{\gamma}} \code{vector} The optimized value for the probability of a suspect result as a fraction of the non-correct test result for non-diseased samples.
#' }
#' @author CVB Statistics \email{CVB.Data.Help@@aphis.usda.gov}
# @author Monica Reising \email{monica.m.reising@@aphis.usda.gov}
get.values<-function(dat,SnR.vec,SpR.vec,prev.vec,N.vec,nstates,tolerance,rep.iter,iter.n,parm=NULL){
  #Put in the error checking...

  #dat should be a vector of counts ordered in a manner consistent that was output from the cellS function
  #i tried to put this in an order that would be consistent with a ddply statment that had .variables = .(Exp,Ref1,Ref2, etc.)

  #I need to create the named vectors required for the cellS function (used within minCell)

  ndraws<-nrow(SnR.vec)
  ntests<-ncol(SnR.vec)/2
  test.names<-paste('Ref',1:ntests,sep='')
  if(is.vector(prev.vec)){
    pop.names<-'A'
  } else{
    pop.names<-LETTERS[1:ncol(prev.vec)]
  }

  if(is.null(parm)){
    if(nstates[1]==2){
      parm<-c(0.9,0.9)
    } else if(nstates[1]==3){
      parm<-c(0.9,0.67,0.9,0.67)
    }
  }

  sens.final<-NULL
  spec.final<-NULL
  if(length(parm)==4){
    p.pos<-NULL
    p.neg<-NULL
  }
  converge<-NULL
  message<-NULL

  for(i in 1:ndraws){
    if(i==1) cat('The optimization has begun',fill=TRUE)

    SnR.current<-data.frame(matrix(SnR.vec[i,],nrow=2,byrow=F,dimnames=list(NULL,test.names)))
    SpR.current<-data.frame(matrix(SpR.vec[i,],nrow=2,byrow=F,dimnames=list(NULL,test.names)))



    if(is.null(dim(prev.vec))){
      prev.current<-as.vector(prev.vec[i])

    } else{
      prev.current<-prev.vec[i,]
    }

    names(prev.current)<-pop.names

    current.fit<-optim(parm,minCell,SnR=SnR.current,SpR=SpR.current,Prev=prev.current,xdat=dat,N=N.vec,nstates=nstates,method='L-BFGS-B',lower=0,upper=1,control=list(pgtol=tolerance))

    current.ests<-current.fit$par
    current.con<-current.fit$convergence
    message.current<-ifelse(is.null(current.fit$message),'NA',current.fit$message)
    if(rep.iter) if(i%%iter.n==0) cat('The following is the number of iterations completed: ',i,fill=T)

    if(length(parm)==2){
      sens.final<-c(sens.final,current.ests[1])
      spec.final<-c(spec.final,current.ests[2])
      converge<-c(converge,current.con)
      message<-c(message,message.current)
    } else if(length(parm)==4){
      sens.final<-c(sens.final,current.ests[1])
      spec.final<-c(spec.final,current.ests[3])
      p.pos<-c(p.pos,current.ests[2])
      p.neg<-c(p.neg,current.ests[4])
      converge<-c(converge,current.con)
      message<-c(message,message.current)
    }

  }
  if(length(parm)==2){
    return(list(sens.final,spec.final,converge,message))
  } else if(length(parm)==4){
    return(list(sens.final,p.pos,spec.final,p.neg,converge,message))
  }
  #return(list(SnR.current,SpR.current,prev.current))


}
