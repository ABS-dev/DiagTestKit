#' @title Create Triangular Distribution
#' @description Creates a discrete step/triangular distribution that can be used to sample values for sensitivity or specificity of a reference test or prevalence of a population.
#' @param m This is a point estimate for the parameter in which you are obtaining the distribution, e.g. sensitivity, specificity, or prevalence.
#' @param w \code{vector}  A vector that provides the half widths of the 3 regions, (w1 closest, w3 farthest).
#' @param h \code{vector}  A vector of "y" (pseduo-value until scaled to be a probability) corresponding
#' to the height of the shoulder and the height of the plateau.
#' @param threestate logical (TRUE/FALSE) Indicates whether or not there is a "suspect"
#' region (i.e. positive/suspect/negative).
#' @param suspect A fraction that indicates what percentage of the remaining probability would
#' be assigned to the suspect region.  For instance, if the function gives sensitivity and
#' then the probability of "suspect" is (1 - sensitivity)*suspect.
#' @param stepwidth distance between the 'x' in the discrete distribution, resolution of possible observations of the created distribution.
#' @param sumOne whether to expresss 'p' as a proportion of its sum.
#' @return \code{data.frame} of 'x', 'y', and 'p'.
# @author Dave Siev \email{david.siev@@aphis.usda.gov}
#' @author \link{DiagTestKit-package}
SampDist <- function(m, w, h, threestate=FALSE, suspect=2/3, stepwidth=0.005, sumOne=TRUE){
  halfwidth <- sum(w)
  H <- cumsum(w)
  X <- m + c(-rev(H), H)
  x <- seq(min(X), max(X), stepwidth)

  region <- y <- slope <- int <- rep(NA, length(x))
  region[x < X[1]] <- 1
  region[x >= X[1] & x < X[2]] <- 1
  region[x >= X[2] & x < X[3]] <- 2
  region[x >= X[3] & x < X[4]] <- 3
  region[x >= X[4] & x < X[5]] <- 4
  region[x >= X[5]] <- 5

  slope[region==1] <- h[2]/w[3]
  slope[region==2] <- (h[1]-h[2])/w[2]
  slope[region==3] <- 0
  slope[region==4] <- unique(-slope[region==2])
  slope[region==5] <- unique(-slope[region==1])
  int[region==1] <- -slope[region==1] * X[1]
  int[region==2] <- h[2] - slope[region==2] * X[2]
  int[region==3] <- h[1]
  int[region==4] <- h[1] - slope[region==4] * X[4]
  int[region==5] <- h[2] - slope[region==5] * X[5]

  y <- slope*x + int
  p <- y/sum(y)
  out <- data.frame(x,y,p)
  # truncate
  out <- out[out$x>=0 & out$x<=1,]
  if(sumOne) out$p <- out$p/sum(out$p)
  if(threestate){
    out$xsus <- suspect*(1-out$x)
    out <- out[c(1,4,2,3)]
  }

  return(out)
}
