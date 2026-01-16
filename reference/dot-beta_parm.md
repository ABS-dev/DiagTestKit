# Convert Beta Paramaterizations

Convert between the paramaterizations of a beta distribution.

## Usage

``` r
.beta_parm(b, to = "alpha.beta")
```

## Arguments

- b:

  `vector` A named vector specifying non-NULL values for 2 parameters.
  e.g. c(alpha = NA, beta = NA, mu = .6, theta = NA, phi = 1.6, sigma2 =
  NA) or just c(mu = .6, phi = 1.6)

- to:

  Specification of desired parameters, options are one of "alpha.beta",
  "mu.phi", "mu.theta" or "mu.sigma2".

## Value

`vector` A named vector with values for the parameters specified in the
"to" argument of the input.

## Author

[DiagTestKit-package](https://abs-dev.github.io/DiagTestKit/reference/DiagTestKit-package.md)
