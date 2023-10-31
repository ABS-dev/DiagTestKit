#' @title Convert Beta Parameterizations
#' @description Convert between the paramaterizations of a beta distribution.
#' @param b  \code{vector} A named vector specifying non-NULL values for 2
#'   parameters. e.g. c(alpha=NA, beta=NA, mu=.6, theta=NA, phi=1.6, sigma2=NA)
#'   or just c(mu=.6, phi=1.6)
#' @param to Specification of desired parameters, options are one of
#'   "alpha.beta", "mu.phi", "mu.theta" or "mu.sigma2".
#' @author \link{DiagTestKit-package}
#' @return \code{vector} A named vector with values for the parameters specified
#'   in the "to" argument of the input.
.beta_parm <- function(b, to = "alpha.beta") {
  # convert parameterizations
  # from and to must be one of alpha.beta, mu.theta, mu.sigma2, or mu.phi
  # input is named vector
  # e.g. c(alpha=NA, beta=NA, mu=.6, theta=NA, phi=1.6, sigma2=NA)
  # or just c(mu=.6, phi=1.6)
  # R uses the "usual" alpha, beta parametrization of the beta distribution

  # mu is the mean of the distribution; mu = alpha/(alpha + beta)
  #
  # sigma2 is the variance of the distribution;  sigma2 = (alpha*beta)/(((alpha
  # + beta)^2)(alpha + beta + 1))
  #
  # sigma2 can also be expressed as mu(1-mu)/(theta + 1)  #see below for a
  # description of theta
  #
  # sometimes the beta is parameterized in terms of the mean (mu) and the
  # "sample size" (theta = alpha + beta (not a true sample size but mimics that
  # "role"))
  #
  # theta = alpha + beta which can also be expressed as ((mu (1-mu))/sigma2) - 1
  #
  # and phi = 1/(theta + 1)
  #
  # so alpha can be expressed as mu(((mu(1-mu))/sigma2)-1) or as mu*theta
  #
  # so beta can be expressed as (1-mu)theta

  from <- paste(names(b[!is.na(b)]), collapse = ".")
  if (from == "mu.phi") {
    mu <- b["mu"]
    phi <- b["phi"]
    names(mu) <- NULL
    names(phi) <- NULL
    theta <- (1 - phi) / phi
    sigma2 <- mu * (1 - mu) / (theta + 1)
    alpha <- mu * theta
    beta <- (1 - mu) * theta
  } else if (from == "mu.sigma2") {
    mu <- b["mu"]
    sigma2 <- b["sigma2"]
    names(mu) <- NULL
    names(sigma2) <- NULL
    theta <- (mu * (1 - mu) / sigma2) - 1
    phi <- 1 / (theta + 1)
    alpha <- mu * theta
    beta <- (1 - mu) * theta
  } else if (from == "mu.theta") {
    mu <- b["mu"]
    theta <- b["theta"]
    names(mu) <- NULL
    names(theta) <- NULL
    phi <- 1 / (theta + 1)
    sigma2 <- (mu * (1 - mu)) / (theta + 1)
    alpha <- mu * theta
    beta <- (1 - mu) * theta
  } else if (from == "alpha.beta") {
    alpha <- b["alpha"]
    beta <- b["beta"]
    names(alpha) <- NULL
    names(beta) <- NULL
    theta <- alpha + beta
    mu <- alpha / theta
    phi <- 1 / (theta + 1)
    sigma2 <- (mu * (1 - mu)) / (theta + 1)
  } else {
    stop("check your parameter vector")
  }
  out <- switch(to,
                alpha.beta = c(alpha = alpha, beta = beta),
                mu.phi     = c(mu = mu, phi = phi),
                mu.theta   = c(mu = mu, theta = theta),
                mu.sigma2  = c(mu = mu, sigma2 = sigma2)
  )
  return(out)
}
