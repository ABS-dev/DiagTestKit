#' @name emp.hpd
#' @title Calculate the empirical hpd.
#' @description Empirical highest posterior density by shortest length interval.
#' @param X vector of values
#' @param alpha 1 - confidence
#' @return highest posterior density (1-alpha) interval
#' @note Uses type 7 \code{\link{quantile}}. Also used in package \code{MF}
#' @export
# @author Dave Siev \email{david.siev@@aphis.usda.gov}
#' @author CVB Statistics \email{CVB.Data.Help@@aphis.usda.gov}
emp.hpd <- function (X, alpha){
  # empirical hpd by shortest length interval
  X <- sort(X)
  probs <- cbind(low=seq(0,alpha,.001),high=seq(1-alpha,1,.001))
  int.len <- quantile(X,prob=probs[,'high'])-quantile(X,prob=probs[,'low'])
  shortest <- min(int.len)
  first <- which(int.len==shortest)[1]
  hpd <- quantile(X,prob=probs[first,],type=7)
  # see documentation for quantile() for type
  return(hpd)
}
