#' @title minimize cell
#' @description A function used for optimizing the values of sensitivity and
#'   specificity (and \eqn{\delta} and \eqn{\gamma} for a 3-state kit). The
#'   objective function minimizes the sum of the squared deviations (expected -
#'   observed cell counts).
#' @param parm `vector`   A vector of starting values to be used for the
#'   optimization that is passed to `.minimize_cell`.  For a 2-state
#'   experimental test, this is a vector of length 2 with entries (\eqn{\pi},
#'   \eqn{\theta}) For a 3-state experimental test, this is a vector of length 4
#'   with entries (\eqn{\pi}, \eqn{\delta}, \eqn{\theta}, \eqn{\gamma}). See
#'   also [estimateSnSp].
#' @param SnR `data.frame`  Each column corresponds to one reference test. Row 1
#'   contains the sensitivity for the reference test(s). Row 2 contains the
#'   probability of a suspect result as a fraction of the non-correct test
#'   result. This is a value between 0 and 1 (inclusive). Namely, P(T? | D+) =
#'   \eqn{\psi} = \eqn{\delta} * (1 - \eqn{\pi}) where \eqn{\delta} is the
#'   second row for a given column (reference test).  \eqn{\delta =
#'   \frac{\psi}{(1 - \pi)}}{\delta = \psi / (1 - \pi)}.  Use a zero for a
#'   2-state test (i.e. no suspect region).
#' @param SpR `data.frame` Each column corresponds to one reference test. Row 1
#'   contains the specificity for each reference test. Row 2 contains the
#'   probability of a suspect result as a fraction of the non-correct test
#'   result.  This is a value between 0 and 1 (inclusive). Namely, P(T? | D-) =
#'   \eqn{\phi} = \eqn{\gamma} * (1 - \eqn{\theta}) where \eqn{\gamma} is the
#'   second row for a given column (reference test). \eqn{\gamma =
#'   \frac{\phi}{(1 - \theta)}}{\gamma = \phi / (1 - \theta)}.  Use a zero for a
#'   2-state test (i.e. no suspect region).
#' @param Prev `vector`  A named vector containing the prevalence for each
#'   population sampled.
#' @param xdat `vector`  A vector of the observed cell counts.
#' @param N_mat `matrix`  Needs to be filled out
#' @param nstates `vector` A vector with length one more than the number of
#'   reference tests.  The first element is the number of states of the
#'   experimental test and the remaining entries are the number of states of
#'   each reference test (using the same ordering as SnR and SpR).
#' @param suspect2staterows Needs to be filled out.
#' @param X Needs to be filled out.
#' @param Xpos Needs to be filled out.
#' @param Xsus Needs to be filled out.
#' @param Xneg Needs to be filled out.
#' @param ncells Needs to be filled out.
#' @param ntests Needs to be filled out.
#' @returns The sum of the squared deviations between the expected and observed
#'   cell counts.
#' @author [DiagTestKit-package]
.minimize_cell <- function(parm, SnR, SpR, Prev, xdat, N_mat, nstates,
                           suspect2staterows, X, Xpos, Xsus, Xneg, ncells,
                           ntests) {
  if (length(parm) == 2) {
    SnE <- parm[1]
    SpE <- parm[2]
    sus.perc <- c(0, 0)
  } else if (length(parm) == 4) {
    SnE <- parm[1]
    SpE <- parm[3]
    sus.perc <- c(parm[2], parm[4])
  }
  x <- .cell_counts(SnR, SpR, Prev, SnE, SpE, sus.perc, N_mat, nstates,
                    suspect2staterows, X, Xpos, Xsus, Xneg, ncells, ntests)
  return(sum((x - xdat)^2))
}
