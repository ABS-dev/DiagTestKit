#' @title Obtain cell counts
#' @description This function creates expected cell counts (and probabilities) for a specific test pattern based on the diagnostic
#' characteristics of the reference test(s) and experimental test.
#' @param SnR \code{data.frame}  Each column corresponds to one reference test.  Row 1 contains the sensitivity for the reference test(s).
#' Row 2 contains the probability of a suspect result as a fraction of the non-correct test result. This is a value between 0 and 1 (inclusive).
#' Namely, P(T? | D+) = \eqn{\psi} = \eqn{\delta} * (1 - \eqn{\pi}) where \eqn{\delta} is the second row for a given column (reference test).  \eqn{\delta = \frac{\psi}{(1 - \pi)}}{\delta = \psi/(1 - \pi)}.  Use a zero for a 2-state
#' test (i.e. no suspect region).
#' @param SpR \code{data.frame} Each column corresponds to one reference test.  Row 1 contains the specificity for each reference test.
#' Row 2 contains the probability of a suspect result as a fraction of the non-correct test result.  This is a value between 0 and 1 (inclusive).
#' Namely, P(T? | D-) = \eqn{\phi} = \eqn{\gamma} * (1 - \eqn{\theta}) where \eqn{\gamma} is the second row for a given column (reference test). \eqn{\gamma = \frac{\phi}{(1 - \theta)}}{\gamma = \phi/(1 - \theta)}.  Use a zero for a 2-state
#' test (i.e. no suspect region).
#' @param Prev \code{vector}  A named vector containing the prevalence for each population sampled.
#' @param SnE Sensitivity of the experimental test kit.
#' @param SpE Specificity of the experimental test kit.
#' @param sus.perc \code{vector} A vector containing 2 elements, c(\eqn{\delta}, \eqn{\gamma}) for the experimental test kit.  A vector of zeros for a 2-state experimental kit. \eqn{\delta} and \eqn{\gamma} are values
#' between 0 and 1 (inclusive) corresponding to the proportion of the remaining probability (i.e. 1 - \eqn{\pi} or 1 - \eqn{\theta}) that is suspect (\eqn{\psi} or \eqn{\phi}).  \eqn{\delta = \frac{\psi}{(1-\pi)}} and
#' \eqn{\gamma = \frac{\phi}{(1-\theta)}}.
#' @param N \code{vector}  A named vector containing the sample size for each population sampled.
#' @param nstates \code{vector} A vector with length one greater than the number of reference tests.  The first element is the number of states of the experimental test and the remaining entries are the number
#' of states of each reference test (using the same ordering as SnR and SpR).
#' @return  \code{vector} A vector of expected counts corresponding to the properties of the reference and experimental tests.  The expected counts are obtained based on a conditional independence assumption of all test methods.
# @author David Siev \email{david.siev@@aphis.usda.gov} modified by Monica Reising \email{monica.m.reising@@aphis.usda.gov}
#' @author \link{DiagTestKit-package}
cellS <- function(SnR, SpR, Prev, SnE, SpE, sus.perc, N, nstates){
  suspect.pos<-sus.perc[1]*(1-SnE)
  suspect.neg<-sus.perc[2]*(1-SpE)
  SnR[2,]<-SnR[2,]*(1-SnR[1,])
  SpR[2,]<-SpR[2,]*(1-SpR[1,])
  Sn <- data.frame(Exp=c(SnE,suspect.pos), SnR)
  Sp <- data.frame(Exp=c(SpE,suspect.neg), SpR)
  row.names(Sn)<-NULL
  row.names(Sp)<-NULL
  ntests <- ncol(Sn)
  tests <- data.frame(matrix(c('positive','suspect','negative'),3,ntests,dimnames=list(NULL,names(Sn))))
  X <- expand.grid(tests)
  ncells <- nrow(X)
  Xpos <- as.matrix(1*(X=='positive'))
  Xsus <- as.matrix(1*(X=='suspect'))
  Xneg <- as.matrix(1*(X=='negative'))
  tpdp <- matrix(unlist(Sn[1,]), ncells, ntests, byrow=TRUE)
  tsdp <- matrix(unlist(Sn[2,]), ncells, ntests, byrow=TRUE)
  tndp <- matrix(1-apply(Sn, 2, sum), ncells, ntests, byrow=TRUE)
  tndn <- matrix(unlist(Sp[1,]), ncells, ntests, byrow=TRUE)
  tsdn <- matrix(unlist(Sp[2,]), ncells, ntests, byrow=TRUE)
  tpdn <- matrix(1-apply(Sp, 2, sum), ncells, ntests, byrow=TRUE)
  cellP <- as.matrix(apply((Xpos * tpdp) + (Xneg * tndp) + (Xsus * tsdp), 1, prod) %*% matrix(Prev,nrow=1) + apply((Xpos * tpdn) + (Xneg * tndn) + (Xsus * tsdn), 1, prod) %*% matrix((1-Prev),nrow=1))
  colnames(cellP)<-paste('P',names(N),sep='_')
  ## identify the column ids for 2-state tests
  twostatecols <- which(nstates == 2)

  ## look for the rows in the 2-state tests which have "suspect" case
  ## we use the data.frame to be able to handle case where length(twostatecols) == 1
  suspect2staterows <- which(data.frame(X[,twostatecols]) == "suspect", arr.ind = TRUE)[,"row"]

  ## remove the rows which have a "suspect" value for the 2-state tests


  cellP.short<-as.matrix(cellP[-suspect2staterows,])
  #print(cellP.short)
  colnames(cellP.short)<-paste('P',names(N),sep='_')
  cellN <- matrix(rep(N,each=ifelse(is.vector(cellP),1,dim(cellP)[1])),ncol=length(N),byrow=FALSE) * cellP
  colnames(cellN)<-paste('N',names(N),sep='_')
  cellN.short<-as.matrix(cellN[-suspect2staterows,])
  #print(cellN.short)
  colnames(cellN.short)<-paste('N',names(N),sep='_')

  X.short <- cbind(X[-suspect2staterows,],cellP.short,cellN.short)
  setorder(X.short)
  rownames(X.short) <- 1:nrow(X.short)
  xcols<-ncol(X.short)
  count.vec<-c(as.matrix(X.short[,(xcols-(length(N)-1)):xcols]))
  return(count.vec)

}
