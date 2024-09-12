#' @title Update alpha values for existing simulation
#' @description Report interval estimates with updated alpha values, using a
#'   previously evaluated simulation.
#' @param x output from \code{\link{estimateSnSp}}
#' @param newAlpha updated alpha value. Must be within \[0, 1\]
#' @return an object of type \code{snsp}. See output for
#'   \code{\link{estimateSnSp}}
#' @author \link{DiagTestKit-package}
#' @seealso \code{\link{estimateSnSpControl}}
#' @importFrom stats median
#' @export
#' @examples
#' data.1 <- data.frame(
#'   exp_result = rep(c('positive', 'negative'), each = 2),
#'   ref1_result = rep(c('positive', 'negative'), 2),
#'   count = c(82, 11, 5, 22))
#' example.1 <- estimateSnSp(dat      = data.1,
#'                           Sn.ref   = data.frame(ref = c(0.90, 0)),
#'                           Sp.ref   = data.frame(ref = c(0.99, 0)),
#'                           prev.pop = c(A = 0.80),
#'                           control  = estimateSnSpControl(seed = 64725))
#' example.1a <- updateAlpha(example.1, newAlpha = 0.25)
#' example.1a
#'
#' # 1000  simulations
#' # 75 % Interval Estimates
#' #
#' #                    Point.Estimate     Lower     Upper
#' # Sn = P(T+|D+)      0.9449821          0.9053901 0.9791017
#' # Sp = P(T-|D-)      0.9062769          0.8336064 1.0000000
updateAlpha <- function(x, newAlpha) {
  NEWdetailOut <- x$detailOut
  NEWinput <- x$input

  ## figure out if 2 states or 3
  if (length(x$calcVal) == 6) {
    nstates <- 2
  } else {
    nstates <- 3
  }

  if (nstates == 2) {
    NEWcalcVal <- list(
      Nsim       = x$calcVal$Nsim,
      Confidence = (1 - newAlpha),
      SnPE       = median(NEWdetailOut$Exp.Sn),
      SnInterval = .emp_hpd(NEWdetailOut$Exp.Sn, alpha = newAlpha),
      SpPE       = median(NEWdetailOut$Exp.Sp),
      SpInterval = .emp_hpd(NEWdetailOut$Exp.Sp, alpha = newAlpha))
  } else {
    NEWcalcVal <- list(
      Nsim              = x$calcVal$Nsim,
      Confidence        = (1 - newAlpha),
      SnPE              = median(NEWdetailOut$Exp.Sn),
      SnInterval        = .emp_hpd(NEWdetailOut$Exp.Sn, alpha = newAlpha),
      SpPE              = median(NEWdetailOut$Exp.Sp),
      SpInterval        = .emp_hpd(NEWdetailOut$Exp.Sp, alpha = newAlpha),
      SusDisPosPE       =
        median((1 - NEWdetailOut$Exp.Sn) * NEWdetailOut$Exp.pos.p),
      SusDisPosInterval =
        .emp_hpd((1 - NEWdetailOut$Exp.Sn) * NEWdetailOut$Exp.pos.p,
                alpha = newAlpha),
      SusDisNegPE       =
        median((1 - NEWdetailOut$Exp.Sp) * NEWdetailOut$Exp.neg.p),
      SusDisNegInterval =
        .emp_hpd((1 - NEWdetailOut$Exp.Sp) * NEWdetailOut$Exp.neg.p,
                alpha = newAlpha))

  }

  updatedOut <- snsp$new(calcVal   = NEWcalcVal,
                         detailOut = NEWdetailOut,
                         input     = NEWinput)
  return(updatedOut)
}
