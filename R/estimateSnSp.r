#' @title Estimate Sensitivity and Specificity
#' @description A function written by CVB Statistics to estimate the sensitivity
#'   and specificity of an experimental diagnostic test kit in accordance with
#'   \href{https://www.aphis.usda.gov/aphis/ourfocus/animalhealth/veterinary-biologics/biologics-regulations-and-guidance/ct_vb_statwi}{CVB
#'   STATWI0002}.
#' @param dat \code{data.frame}  This is a data frame where the first column
#'   includes information for the population sampled (if more than one
#'   population is sampled).  The next column is the possible outcomes of the
#'   experimental test followed by one column for the possible outcomes for each
#'   reference test (one column per test).  The last column of the data frame
#'   provides the number of samples with each pattern of test outcomes.  The
#'   columns must be included in the order described.  If more than one
#'   population is sampled, the column name for the column containing the
#'   population information must be 'population'.  The column containing the
#'   test results for the experimental test must have 'exp' in the name, such as
#'   experimental, experiment, exp, Exp, etc.  The column names containing the
#'   reference test results much contain 'ref' in the name, such as Ref1, Ref2,
#'   ref1_results, Reference2, etc.
#' @param Sn.ref \code{data.frame}  Each column corresponds to one reference
#'   test.  Row 1 contains the sensitivity for the reference test(s). Row 2
#'   contains the probability of a suspect result as a fraction of the
#'   non-correct test result. This is a value between 0 and 1 (inclusive).
#'   Namely, P(T? | D+) = \eqn{\psi} = \eqn{\delta} * (1 - \eqn{\pi}) where
#'   \eqn{\delta} is the second row for a given column (reference test).
#'   \eqn{\delta = \frac{\psi}{(1 - \pi)}}{\delta = \psi/(1 - \pi)}.  Use a zero
#'   for a 2-state test (i.e. no suspect region).  Alternatively, if all
#'   reference tests are 2-state tests, the sensitivities can be input as a
#'   named vector.  Specifically, each element in the vector must be given a
#'   name which includes 'ref' (see above) and the column names (or names of the
#'   elements within the vector) must match those for Sp.ref.
#' @param Sp.ref \code{data.frame} Each column corresponds to one reference
#'   test. Row 1 contains the specificity for each reference test. Row 2
#'   contains the probability of a suspect result as a fraction of the
#'   non-correct test result.  This is a value between 0 and 1 (inclusive).
#'   Namely, P(T? | D-) = \eqn{\phi} = \eqn{\gamma} * (1 - \eqn{\theta}) where
#'   \eqn{\gamma} is the second row for a given column (reference test).
#'   \eqn{\gamma = \frac{\phi}{(1 - \theta)}}{\gamma = \phi/(1 - \theta)}. Use a
#'   zero for a 2-state test (i.e. no suspect region).  Alternatively, if all
#'   reference tests are 2-state tests, the specificity can can be input as a
#'   named vector.  Specifically, each element in the vector must be given a
#'   name which includes 'ref' (see above) and the column names (or names of the
#'   elements within the vector) must match those for Sn.ref.
#' @param prev.pop \code{vector}  A named vector containing the prevalence for
#'   each population sampled.  The names in the vector must match the population
#'   labels used in 'dat'.
#' @param nsim The number of simulations to draw from the sensitivity and
#'   specificity distribution(s) for each reference test and the prevalence
#'   distribution from each population.
#' @param control list of control values to replace defaults. See
#'   \code{\link{estimateSnSpControl}} for details.
#' @return An object of type \code{snsp} that extends \code{list}. \cr \cr
#'  \describe{
#'  \item{\strong{calcVal}}{Point estimates and estimated simulated intervals
#'  for properties of the experimental kit. See below.}
#'  \item{\strong{detailOut}}{Detailed output values. See below.}
#'  \item{\strong{input}}{Simulated values.  See below.}
#'  }
#'
#' @section \code{calcVal}:
#'
#'   A list with the following values which will include the following for both
#'   2- and 3-state experimental tests -- \cr
#' \itemize{
#' \item{\strong{Nsim}}  Number of simulations performed.
#' \item{\strong{Confidence}}   1 - \eqn{\alpha}.
#' \item{\strong{SnPE}}  Sensitivity point estimate obtained as the median of
#'  the estimated values.
#' \item{\strong{SnInterval}} Estimated simulated interval for sensitivity.
#' \item{\strong{SpPE}} Specificity point estimate obtained as the median of
#' the estimated values.
#' \item{\strong{SpInterval}} Estimated simulated interval for specificity.
#' }
#'
#'   If three states, the list will also include -- \cr
#' \itemize{
#' \item{\strong{SusDisPosPE}} Point estimate for the probability of test
#'  suspect given disease positive (\eqn{\psi}) which is the median of the
#'  calculated values (\eqn{\psi} = \eqn{\delta}*(1-\eqn{\pi})).
#' \item{\strong{SusDisPosInterval}} Estimated simulated interval for the
#'  probability of test suspect given disease positive (\eqn{\psi}).
#' \item{\strong{SusDisNegPE}} Point estimate for the probability of test
#'  suspect given disease negative (\eqn{\phi}) which is the median of the
#'   calculated values (\eqn{\phi} = \eqn{\gamma}*(1-\eqn{\theta})).
#' \item{\strong{SusDisNegInterval}} Estimated simulated interval for the
#'  probability of test suspect given disease negative (\eqn{\phi}).
#' }
#'
#' @section \code{detailOut}:
#'
#'   A list with the following detailed output values which will include the
#'   following for both 2- and 3-state experimental tests -- \cr
#' \itemize{
#' \item{\strong{Exp.Sn}}  \code{vector} The optimized values for the
#' sensitivity of the experimental test kit.
#' \item{\strong{Exp.Sp}}  \code{vector} The optimized values for the
#' specificity of the experimental test kit.
#' \item{\strong{Converge}}  \code{vector} Each entry is an integer code
#' detailing the convergence of the optimization for each iteration.  0
#'  indicates successful completion. See also \code{\link{optim}}.
#' \item{\strong{Message}}  \code{vector}  Each entry includes a character
#' string providing any additional information returned by the optimizer or
#'  NULL.  See also \code{\link{optim}}.
#' }
#'
#'   If three states, the list will also inlcude -- \cr
#' \itemize{
#' \item{\strong{Exp.pos.p}}  \code{vector} The optimized values for the
#' proportion of the remaining probability (1-Sn) that corresponds to a
#' suspect region for diseased samples, namely \eqn{\delta}.
#' \item{\strong{Exp.sus.pos}}  \code{vector} The values for
#' P(T? | D+) (\eqn{\psi}) calculated from Exp.sn and Exp.pos.p.
#' P(T?|D+) = \eqn{\delta} * (1 - \eqn{\pi}).
#' \item{\strong{Exp.neg.p}} \code{vector} The optimized value for the
#' proportion of the remaining probability (1-Sp) that corresponds to a
#' suspect region for non-diseased samples, namely \eqn{\gamma}.
#' \item{\strong{Exp.sus.neg}} \code{vector} The values for
#' P(T? | D-) (\eqn{\phi}) calculated from Exp.sp and Exp.neg.p.
#' P(T?|D-) = \eqn{\gamma} * (1 - \eqn{\theta}).
#' }
#'
#' @section \code{input}: A list containing the seed used and the simulated
#'   values.
#'
#' \itemize{
#' \item{\strong{seed}}  The seed used in the random generation of the
#' distributions of sensitivity and specificity for all reference tests and
#' prevalence of each population.  See also \code{\link{set.seed}}
#' \item{\strong{Sn.sims}}  \code{matrix} The simulated values for the
#'  sensitivity of each reference test and \eqn{\psi} where \eqn{\psi} was
#'  specified in the second row of Sn.ref (or zero if Sn.ref was a vector).
#'  The first two
#' columns correspond to the first reference test, columns 3 and 4 to the
#' second reference test if it exists, etc.
#' \item{\strong{Sp.sims}} \code{matrix} The simulated values for the
#'  specificity of each reference test and \eqn{\phi} where \eqn{\phi} was
#'  specified in the second row of Sp.ref (or zero is Sp.ref was a vector).
#'  The first two
#' columns correspond to the first reference test, columns 3 and 4 to the
#' second reference test if it exists, etc.
#' \item{\strong{prev.sims}}  \code{matrix} The simulated values of prevalence
#' for each population.  Each column correspond to one population.
#' }
#' @author \link{DiagTestKit-package}
#' @seealso \code{\link{estimateSnSpControl}}
#' @importFrom data.table setorder
#' @importFrom plyr ddply summarize "."
#' @importFrom stats median
#' @importFrom methods new
#' @export
#' @examples
#' data.1 <- data.frame(exp_result = rep(c('positive', 'negative'), each = 2),
#'                      ref1_result = rep(c('positive', 'negative'), 2),
#'                      count = c(82, 11, 5, 22))
#' example.1 <- estimateSnSp(dat = data.1,
#'                           Sn.ref = data.frame(ref = c(0.90, 0)),
#'                           Sp.ref = data.frame(ref = c(0.99, 0)),
#'                           prev.pop = c(A = 0.80),
#'                           control = estimateSnSpControl(seed = 64725))
#' example.1
#'
#' # 1000  simulations
#' # 95 % Interval Estimates
#' #
#' #               Point.Estimate     Lower  Upper
#' # Sn = P(T+|D+)      0.9449821 0.9019639      1
#' # Sp = P(T-|D-)      0.9062769 0.7523346      1
#'
#'
#' \dontrun{
#' data.2 <- data.frame(Population = rep(LETTERS[1:3], each = 24),
#'                      exp_result = rep(rep(
#'                        c('negative', 'positive', 'suspect'), each = 8), 3),
#'                      ref1_result = rep(rep(
#'                        c('negative', 'positive'), each = 4), 9),
#'                      ref2_result = rep(rep(
#'                        c('negative', 'positive'), each = 2), 18),
#'                      ref3_result = rep(c('negative', 'positive'), 36),
#'                      count = c(3, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 5,
#'                                1, 8, 11, 62, 0, 0, 0, 0, 0, 0, 0,
#'                                2, 27, 2, 3, 0, 4, 0, 1, 1, 0, 0, 1,
#'                                4, 1, 6, 7, 41, 0, 0, 0, 0, 0, 0,
#'                                0, 2, 57, 5, 6, 1, 9, 1, 1, 0, 0, 0,
#'                                0, 1, 0, 2, 2, 12, 1, 0, 0, 0, 0, 0, 0, 0))
#' example.2 <- estimateSnSp(dat = data.2,
#'   Sn.ref = data.frame(ref1 = c(0.92, 0),
#'                       ref2 = c(0.88, 0),
#'                       ref3 = c(0.85, 0)),
#'   Sp.ref = data.frame(ref1 = c(0.86, 0),
#'                       ref2 = c(0.90, 0),
#'                       ref3 = c(0.92, 0)),
#'   prev.pop = c(A = 0.95, B = 0.62, C = 0.18),
#'   control = estimateSnSpControl(seed = 865213))
#' # 1000  simulations
#' # 95 % Interval Estimates
#'
#' #                Point.Estimate     Lower      Upper
#' # Sn = P(T+|D+)      0.96541704 0.8879949 1.00000000
#' # Sp = P(T-|D-)      0.98351924 0.9016964 1.00000000
#' # SsP = P(T?|D+)     0.02568427 0.0000000 0.06339616
#' # SsN = P(T?|D-)     0.01534125 0.0000000 0.05604950
#' }
estimateSnSp <- function(dat, Sn.ref, Sp.ref, prev.pop, nsim = 1000,
                         control = NULL) {
  # convert any character variables in dat to factors as this will be needed
  # later
  dat[sapply(dat, is.character)] <-
    lapply(dat[sapply(dat, is.character)], as.factor)
  if (is.null(control)) {
    control <- estimateSnSpControl()
  }

  # Sn.ref and Sp.ref are data.frames with the means of sensitivity and
  # specificity for all reference tests, one column per test two rows, the
  # second row being a value between 0 and 1 representing the proportion of the
  # remaining probability (i.e. 1-Sn or 1-Sp) that is "suspect" so the
  # P(suspect|disease +) = p * (1-Sn) where p is the the 2nd row for a given
  # column (reference test) Use a zero for a 2-state test (i.e. no suspect
  # region)

  # these will be used to obtain the distribution to sample coerce all the names
  # in the data frame to lower case
  names(dat) <- tolower(names(dat))

  if (is.vector(Sn.ref)) {
    Sn.ref <- data.frame(rbind(Sn.ref, rep(0, length(Sn.ref))),
                         row.names = NULL)
  }

  if (is.vector(Sp.ref)) {
    Sp.ref <- data.frame(rbind(Sp.ref, rep(0, length(Sp.ref))),
                         row.names = NULL)
  }

  if (is.null(names(Sn.ref)) || is.null(names(Sp.ref))) {
    stop("Sn.ref and Sp.ref must be named")
  }
  if (!all(names(Sn.ref) == names(Sp.ref))) {
    stop("SnR and SpR not named the same")
  }

  if (ncol(Sn.ref)  !=  ncol(Sp.ref)) {
    stop("Sn.ref and Sp.ref suggest different number of reference tests")
  }

  # check that if distn is not null that the length is equal to the number of
  # columns of Sn.ref
  if (!is.null(control$Sn.distn) &&
      !is.null(control$Sn.spread) &&
      length(control$Sn.distn)  !=  length(control$Sn.spread)) {
    stop("Sn.distn & Sn.spread must be the same length. ",
         "Check values passed to control.")
  }

  if (!is.null(control$Sp.distn) &&
      !is.null(control$Sp.spread) &&
      length(control$Sp.distn) != length(control$Sp.spread)) {
    stop("Sp.distn & Sp.spread must be the same length. ",
         "Check values passed to control.")
  }

  if (names(dat)[1] != "population") {
    warning("The data suggests a single population was tested",
            immediate. = TRUE)
  }

  if (sum(grepl(pattern = "exp", names(dat), ignore.case = TRUE)) == 0) {
    stop("Column names must indicate which is the experimental test")
  }

  if (sum(grepl(pattern = "ref", names(dat), ignore.case = TRUE)) == 0) {
    stop("Column names must indicate which belong to the reference test(s)")
  }

  # rename the last column in the data frame to counts
  names(dat)[ncol(dat)] <- "count"

  # get the number of states for each test, experimental and all reference tests
  finding_n_states <- unlist(lapply(lapply(dat, levels), length))
  y1 <- grepl(pattern = "exp", names(finding_n_states), ignore.case = TRUE)
  y2 <- grepl(pattern = "ref", names(finding_n_states), ignore.case = TRUE)
  n.states <- finding_n_states[as.logical(y1 + y2)]

  if (!any(grepl(pattern = "pop", colnames(dat), ignore.case = TRUE))) {
    N <- c(A = sum(dat[, ncol(dat)]))
  } else {
    # make sure the number of unique populations is the same in the dataset and
    # in the prev.pop vector
    if (length(levels(as.factor(dat$population))) == length(prev.pop)) {
      prev.pop <- prev.pop[order(names(prev.pop))]
      dat$population <- factor(dat$population, levels = names(prev.pop))
    } else {
      stop("The number of populations specified in the data does ",
           "not match the number of populations in the prev.pop vector")
    }


    pop_counts <- ddply(dat, .(population), summarize, N = sum(count))
    N <- pop_counts$N
    names(N) <- pop_counts$population
  }

  set.seed(control$seed)
  prev.sims <- .get_simulated_values(means      = prev.pop,
                                    distn      = control$prev.distn,
                                    spread     = control$prev.spread,
                                    nsim       = nsim,
                                    step_size  = control$step.size,
                                    prevalence = TRUE)
  Sn.sims <- .get_simulated_values(means      = Sn.ref,
                                  distn      = control$Sn.distn,
                                  spread     = control$Sn.spread,
                                  nsim       = nsim,
                                  step_size  = control$step.size,
                                  prevalence = FALSE)
  Sp.sims <- .get_simulated_values(means      = Sp.ref,
                                  distn      = control$Sp.distn,
                                  spread     = control$Sp.spread,
                                  nsim       = nsim,
                                  step_size  = control$step.size,
                                  prevalence = FALSE)

  # this will order the data according the factors in the columns if there are
  # multiple populations, that should be the first column the counts should be
  # the last column
  dat <- setorder(dat)

  if (n.states[1] == 3) {
    message("Optimization is more time consuming for a ",
            "3-state experimental test, be patient!")
  }
  final_values <- .get_values(dat = dat[, ncol(dat)],
                             SnR.vec   = Sn.sims,
                             SpR.vec   = Sp.sims,
                             prev.vec  = prev.sims,
                             N.vec     = N,
                             nstates   = n.states,
                             tolerance = control$tolerance,
                             parm      = control$parm,
                             rep.iter  = control$rep.iter,
                             iter.n    = control$iter.n)

  if (n.states[1] == 2) {
    detailOut <- list(final_values[[1]], final_values[[2]],
                      final_values[[3]], final_values[[4]])
    names(detailOut) <- c("Exp.Sn", "Exp.Sp", "Converge", "Message")

    calcVal <- list(
      Nsim       = nsim,
      Confidence = (1 - control$alpha),
      SnPE       = median(final_values[[1]]),
      SnInterval = .emp_hpd(final_values[[1]], alpha = control$alpha),
      SpPE       = median(final_values[[2]]),
      SpInterval = .emp_hpd(final_values[[2]], alpha = control$alpha))
  } else if (n.states[1] == 3) {
    detailOut <- list(final_values[[1]], final_values[[2]],
                      (1 - final_values[[1]]) * final_values[[2]],
                      final_values[[3]], final_values[[4]],
                      (1 - final_values[[3]]) * final_values[[4]],
                      final_values[[5]], final_values[[6]])
    names(detailOut) <- c("Exp.Sn", "Exp.pos.p", "Exp.sus.pos", "Exp.Sp",
                          "Exp.neg.p", "Exp.sus.neg", "Convergence", "Message")
    calcVal <- list(
      Nsim              = nsim,
      Confidence        = (1 - control$alpha),
      SnPE              = median(final_values[[1]]),
      SnInterval        = .emp_hpd(final_values[[1]], alpha = control$alpha),
      SpPE              = median(final_values[[3]]),
      SpInterval        = .emp_hpd(final_values[[3]],
                                  alpha = control$alpha),
      SusDisPosPE       = median((1 - final_values[[1]]) * final_values[[2]]),
      SusDisPosInterval = .emp_hpd((1 - final_values[[1]]) * final_values[[2]],
                                  alpha = control$alpha),
      SusDisNegPE       = median((1 - final_values[[3]]) * final_values[[4]]),
      SusDisNegInterval = .emp_hpd((1 - final_values[[3]]) * final_values[[4]],
                                  alpha = control$alpha))
  }
  input <- list(control$seed, Sn.sims, Sp.sims, prev.sims)
  names(input) <- c("seed", "Sn.sims", "Sp.sims", "prev.sims")
  out <- snsp$new(calcVal = calcVal,
                  detailOut = detailOut,
                  input = input)

  return(out)
}

# to keep R CMD happy
utils::globalVariables(c("population", "count"))
