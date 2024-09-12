#' @title control values for estimateSnSp
#' @description The values supplied in the function--call replace the defaults and a list with all
#' possible arguments is returned. The returned list is used as the \code{control} argument to
#' the function \code{estimateSnSp}.
#' @param seed The seed used in the random generation of the distributions of sensitivity and specificity for all reference tests and
#' prevalence of each population.  See also \code{\link{set.seed}}.
#' @param Sn.distn \code{vector}  A named vector with length equal to the number of reference tests.  Determines which disibution should be used for sampling sensitivity of each reference test.  Inputs are 'beta' or 'triangular'.
#' Defaults to 'beta' for each reference test.
#' @param Sn.spread \code{vector}  A named vector with length equal to the number of reference tests. Describes the width of the distribution for the sensitivity of each reference test.  Inputs are 'wide', 'medium',
#' or 'narrow'.  Defaults to 'wide' for each reference test.
#' @param Sp.distn \code{vector}  A named vector with length equal to the number of reference tests.  Determines which disibution should be used for sampling specificity of each reference test.  Inputs are 'beta' or 'triangular'.
#' Defaults to 'beta' for each reference test.
#' @param Sp.spread \code{vector}  A named vector with length equal to the number of reference tests. Describes the width of the distribution for the specificity of each reference test.  Inputs are 'wide', 'medium',
#' or 'narrow'.  Defaults to 'wide' for each reference test.
#' @param prev.distn \code{vector}  A named vector with length equal to the number of populations. Determines which disibution should be used for sampling the prevalence of each population.  Inputs are 'beta' or 'triangular'.
#' Defaults to 'beta'.
#' @param prev.spread \code{vector}  A named vector with length equal to the number of populations. Describes the width of the distribution for the prevalence of each population.  Inputs are 'wide', 'medium', or 'narrow'.
#' Defaults to 'wide' for each population.
#' @param tolerance Setting a limit on the pgtol used in the \code{\link{optim}} function with the 'L-BFGS-B' method. See also \code{\link{optim}}.  Defaults to 1E-03.
#' @param step.size Provides the level of resolution in values simulated from a triangular distribution.  Defaults to 1E-06.
#' @param alpha Significance levels.  Defaults to 0.05.
#' @param rep.iter logical (TRUE/FALSE) Indicates if updates should be printed regarding the number of iterations completed.  Defaluts to TRUE.
#' @param iter.n  integer indicating the frequency of updates for the number of iterations completed.  Defaluts to 50.
#' @param parm \code{vector}  Starting values for the optimization of the parameters of the experimental test.  If the experimental test has 2 states, this vector is of length two with elements corresponding to
#' sensitivity and specificity, respectively.  If the experimental test has 3 states, this vector is of length 4 with elements corresponding to sensitivity (\eqn{\pi}), the proportion of 1-Sn corresponding to the suspect region for disease
#' positive samples (\eqn{\delta}), specificity (\eqn{\theta}), and the proportion of 1-Sp corresponding to the suspect region for disease negative samples (\eqn{\gamma}).  All values are between 0 and 1, inclusive.
#' @return A list with the following elements (as defined above): \code{seed, Sn.disn, Sn.spread,
#' Sp.distn,} \code{ Sp.spread, prev.distn, prev.spread, tolerance, step.size, parm}
#' @author \link{DiagTestKit-package}
#' @export
#' @examples
#' estimateSnSpControl()
#' estimateSnSpControl(seed = 64725)
estimateSnSpControl <- function(seed = NULL, Sn.distn = NULL, Sn.spread=NULL,
                                             Sp.distn = NULL, Sp.spread=NULL, prev.distn=NULL,
                                             prev.spread=NULL, tolerance=1e-3, alpha=0.05,
                                             step.size=1e-06, parm=NULL,
                                             rep.iter=TRUE, iter.n=50){

  if(is.null(seed)){
    seed <- round(100000 * runif(1, 0, 1))
  }

  return(list(seed = seed, Sn.distn = Sn.distn, Sn.spread = Sn.spread, Sp.distn = Sp.distn,
              prev.distn = prev.distn, prev.spread = prev.spread, tolerance = tolerance,
              step.size = step.size, parm = parm,alpha=alpha,rep.iter=rep.iter,iter.n=iter.n))
}
