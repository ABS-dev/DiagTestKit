#' @title Obtain cell counts
#' @description This function creates expected cell counts (and probabilities)
#'   for a specific test pattern based on the diagnostic characteristics of the
#'   reference test(s) and experimental test.
#' @param SnR \code{data.frame}  Each column corresponds to one reference test.
#'   Row 1 contains the sensitivity for the reference test(s). Row 2 contains
#'   the probability of a suspect result as a fraction of the non-correct test
#'   result. This is a value between 0 and 1 (inclusive). Namely, P(T? | D+) =
#'   \eqn{\psi} = \eqn{\delta} * (1 - \eqn{\pi}) where \eqn{\delta} is the
#'   second row for a given column (reference test).  \eqn{\delta =
#'   \frac{\psi}{(1 - \pi)}}{\delta = \psi/(1 - \pi)}.  Use a zero for a 2-state
#'   test (i.e. no suspect region).
#' @param SpR \code{data.frame} Each column corresponds to one reference test.
#'   Row 1 contains the specificity for each reference test. Row 2 contains the
#'   probability of a suspect result as a fraction of the non-correct test
#'   result.  This is a value between 0 and 1 (inclusive). Namely, P(T? | D-) =
#'   \eqn{\phi} = \eqn{\gamma} * (1 - \eqn{\theta}) where \eqn{\gamma} is the
#'   second row for a given column (reference test). \eqn{\gamma =
#'   \frac{\phi}{(1 - \theta)}}{\gamma = \phi/(1 - \theta)}.  Use a zero for a
#'   2-state test (i.e. no suspect region).
#' @param Prev \code{vector}  A named vector containing the prevalence for each
#'   population sampled.
#' @param SnE Sensitivity of the experimental test kit.
#' @param SpE Specificity of the experimental test kit.
#' @param sus.perc \code{vector} A vector containing 2 elements, c(\eqn{\delta},
#'   \eqn{\gamma}) for the experimental test kit.  A vector of zeros for a
#'   2-state experimental kit. \eqn{\delta} and \eqn{\gamma} are values between
#'   0 and 1 (inclusive) corresponding to the proportion of the remaining
#'   probability (i.e. 1 - \eqn{\pi} or 1 - \eqn{\theta}) that is suspect
#'   (\eqn{\psi} or \eqn{\phi}).  \eqn{\delta = \frac{\psi}{(1-\pi)}} and
#'   \eqn{\gamma = \frac{\phi}{(1-\theta)}}.
#' @param N_mat \code{matrix} Needs to be filled out
#' @param nstates \code{vector} A vector with length one greater than the number
#'   of reference tests.  The first element is the number of states of the
#'   experimental test and the remaining entries are the number of states of
#'   each reference test (using the same ordering as SnR and SpR).
#' @param suspect2staterows Needs to be filled out.
#' @param X Needs to be filled out.
#' @param Xpos Needs to be filled out.
#' @param Xsus Needs to be filled out.
#' @param Xneg Needs to be filled out.
#' @param ncells Needs to be filled out.
#' @param ntests Needs to be filled out.
#' @return  \code{vector} A vector of expected counts corresponding to the
#'   properties of the reference and experimental tests.  The expected counts
#'   are obtained based on a conditional independence assumption of all test
#'   methods.
#' @author \link{DiagTestKit-package}
#' @importFrom data.table setorder
.cell_counts <- function(SnR, SpR, Prev, SnE, SpE, sus.perc, N_mat, nstates,
                  suspect2staterows, X, Xpos, Xsus, Xneg, ncells, ntests) {
  suspect_pos <- sus.perc[1] * (1 - SnE)
  suspect_neg <- sus.perc[2] * (1 - SpE)
  SnR[2, ] <- SnR[2, ] * (1 - SnR[1, ])
  SpR[2, ] <- SpR[2, ] * (1 - SpR[1, ])
  Sn <- cbind(matrix(c(SnE, suspect_pos), nrow = 2), SnR)
  Sp <- cbind(matrix(c(SpE, suspect_neg), nrow = 2), SpR)

  tpdp <- matrix(Sn[1, ], ncells, ntests, byrow = TRUE)
  tsdp <- matrix(Sn[2, ], ncells, ntests, byrow = TRUE)
  tndp <- 1 - tpdp - tsdp
  tndn <- matrix(Sp[1, ], ncells, ntests, byrow = TRUE)
  tsdn <- matrix(Sp[2, ], ncells, ntests, byrow = TRUE)
  tpdn <- 1 - tndn - tsdn
  cellP <-
    apply((Xpos * tpdp) +
            (Xneg * tndp) +
            (Xsus * tsdp), 1, prod) %*% Prev +
      apply((Xpos * tpdn) +
              (Xneg * tndn) +
              (Xsus * tsdn), 1, prod) %*% (1 - Prev)
  cellN <- N_mat * cellP
  X.short <- cbind(X, cellP, cellN)[-suspect2staterows, ]
  setorder(X.short)

  xcols <- ncol(X.short)
  count.vec <- c(as.matrix(X.short[, (xcols - ncol(N_mat) + 1):xcols]))
  return(count.vec)
}
