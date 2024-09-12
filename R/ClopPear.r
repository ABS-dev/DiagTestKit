#' @title Binomial confidence interval, Clopper-Pearson method.
#' @description Evaluate binomial confidence interval using Clopper-Pearson method. A function written by CVB Statistics to estimate the sensitivity and specificity
#' of an experimental diagnostic test kit in accordance with \href{https://www.aphis.usda.gov/aphis/ourfocus/animalhealth/veterinary-biologics/biologics-regulations-and-guidance/ct_vb_statwi}{CVB STATWI0002}.
#' @param dat \code{data.frame}  A data frame with a column for the experimental test results, a column for the infallible reference test results,  and a column for the corresponding count.  The column
#' name for the experimental test results must contatin 'exp' and the column name for the infallible reference test results must include 'ref'.  The counts should be the last column.
#' @param est.Sn logical (TRUE/FALSE) Indicating if the sensitivity and its confidence interval should be supplied (TRUE) or if the specificity and its confidence interval should be supplied (FALSE).
#' @param alpha Complement of confidence level.
#' @return An object of type \code{cp} that extends \code{list}. \cr \cr
#' \describe{
#' \item{\strong{calcVal}} {Named vector of point estimates and estimated simulated intervals. See below.}
#' \item{\strong{data}} {Test and Total values of the data. See below.}
#' \item{\strong{alpha}} {Complement of the confidence interval as provided above.}
#' }
#'
#' @section If \code{est.Sn == TRUE}:
#'
#' \code{calcVal} is a list with the following elements \cr
#' \itemize{
#' \item{\strong{Sn}} Sensitivity estimate.
#' \item{\strong{Sn.LL}} Lower confidence limit for sensitivity.
#' \item{\strong{Sn.UL}} Upper confidence limit for sensitivity.
#' }

#' \code{data} is a list with the following elements \cr
#' \itemize{
#' \item{\strong{Test.Positive}} Number of experimental test positives.
#' \item{\strong{Total.Positive}} Total number of positive samples.
#' }
#'
#' @section If \code{est.Sn == FALSE}:
#'
#' \code{calcVal} is a list with the following elements \cr
#' \itemize{
#' \item{\strong{Sp}} Specificity estimate.
#' \item{\strong{Sp.LL}} Lower confidence limit for specificity.
#' \item{\strong{Sp.UL}} Upper confidence limit for specificity.
#' }
#'
#' \code{data} is a list with the following elements \cr
#' \itemize{
#' \item{\strong{Test.Negative}} Number of experimental test negatives.
#' \item{\strong{Total.Negative}} Total number of negative samples.
#' }
# A matrix with a single row.  If est.Sn = T the columns correspond to the number of experimental test positives,
# the total number of positive samples, sensitivity, the lower confidence limit for sensitivity and the upper confidence limit for sensitivity.
# If est.Sn=F, the columns correspond to the number of experimental test negatives, the total number of negative samples, specificity,
# the lower confidence limit for specificity and the upper confidence limit for specificity.
#' @references Clopper CJ, Pearson ES, 1934. The use of confidence or fiducial limits illustrated in the case of the binomial. \emph{Biometrika} 26:404-413.
#' @export
# @author David Siev modified by Monica Reising
#' @author CVB Statistics \email{CVB.Data.Help@@aphis.usda.gov}
#' @examples
#' CP.Sn <- cloppearSnSp(dat = dat_infal, est.Sn = TRUE)
#' CP.Sn
#' # Sn = P(T+|D+): 0.987013 (95% CI: 0.953876, 0.998423)
#' CP.Sp <- cloppearSnSp(dat = dat_infal, est.Sn = FALSE)
#' CP.Sp
#' # Sp = P(T-|D-): 0.970297 (95% CI: 0.915643, 0.915643)
cloppearSnSp <- function(dat,alpha=0.05,est.Sn=TRUE){
#-------------------------------------------------------
# Clopper-Pearson exact binomial confidence interval by
#   beta distribution method (changed from F dist method pre-92)
# coded by D.Siev 11/04/92, updated 1/9/10
# show.warnings=F suppresses unnecessary warnings from ifelse
#-------------------------------------------------------

  dat[sapply(dat,is.character)]<-lapply(dat[sapply(dat,is.character)],as.factor)

  if(sum(grepl(pattern='exp',names(dat),ignore.case=TRUE))==0){
    stop('Column names must indicate which is the experimental test')
  }

  if(sum(grepl(pattern='ref',names(dat),ignore.case=TRUE))==0){
    stop('Column names must indicate which belong to the infallible reference test')
  }

  #rename the last column in the data frame to counts
  names(dat)[ncol(dat)]<-'count'
  names(dat)[grepl(pattern='exp',names(dat),ignore.case=TRUE)]<-'exp'
  names(dat)[grepl(pattern='ref',names(dat),ignore.case=TRUE)]<-'ref'

  dat$exp[grepl(pattern='pos',dat$exp,ignore.case=TRUE)]<-'positive'
  dat$exp[grepl(pattern='neg',dat$exp,ignore.case=TRUE)]<-'negative'


  if(est.Sn){
    y<-dat$count[dat$exp=='positive' & grepl(pattern='pos',dat$ref,ignore.case=TRUE)]
    n<-sum(dat$count[grepl(pattern='pos',dat$ref,ignore.case=TRUE)])
  }
  if(!est.Sn){
    y<-dat$count[dat$exp=='negative' & grepl(pattern='neg',dat$ref,ignore.case=TRUE)]
    n<-sum(dat$count[grepl(pattern='neg',dat$ref,ignore.case=TRUE)])

  }
p <- y/n
cpl <- ifelse(y>0,qbeta(alpha/2,y,n-y+1),0)
cpu <- ifelse(y<n,qbeta(1-alpha/2,y+1,n-y),1)
dataout <- list(y, n)
calcVal <- list(p,cpl,cpu)

if(est.Sn){
  names(dataout) <- c('Test.Positive', 'Total.Positive')
  names(calcVal) <- c('Sn','Sn.LL','Sn.UL')
} else if(!est.Sn) {
  names(dataout) <- c('Test.Negative','Total.Negative')
  names(calcVal) <- c('Sp','Sp.LL','Sp.UL')
}

out <- cp$new(calcVal = calcVal, data = dataout, alpha = alpha)
return(out)
}
