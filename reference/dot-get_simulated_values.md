# Get Simulated Values

Simulate values for use in optimization. This function is used to obtain
draws from a distribution for the sensitivity and specificity for each
reference test and for the prevalence of each population tested.

## Usage

``` r
.get_simulated_values(means, distn, spread, nsim, step_size, prevalence)
```

## Arguments

- means:

  `vector` A named vector containing point estimates for the prevalence
  of each population (when prevalence = TRUE, see below) or a data frame
  where each column corresponds to a reference test and the rows are
  sensitivity (\\\pi\\) and \\\psi\\ (or specificity (\\\theta\\) and
  \\\phi\\).

- distn:

  `vector` A vector of same length as `means.` Values may be one of
  `NULL`, "beta", or "triangular". `NULL` will be treated as "beta"

- spread:

  `vector` A vector of same length as `means`. Values may be one of
  `NULL`, "wide", "medium", or "narrow". `NULL` will be treated as
  "wide".

- nsim:

  The number of simulations to draw from the sensitivity and specificity
  distribution(s) for each reference test or the prevalence distribution
  from each population.

- step_size:

  Provides the level of resolution in values simulated from a triangular
  distribution.

- prevalence:

  logical (TRUE/FALSE) TRUE indicates that the function is simulating
  values of prevalence. This will determine the structure of the output.

## Value

final_mat A matrix of simulated values. If prevalence is TRUE, final_mat
will have the number of columns corresponding to the number of
populations sampled else if prevalence is FALSE, final_mat will have
number of columns twice the number of reference tests. The columns are
sensitivity (or specificity) of the first reference test, the
probability of a suspect result as a fraction of the non-correct test
result (i.e. either \\\delta\\ or \\\gamma\\) for the first reference
and continues in the same pattern for all reference tests.

## Author

[DiagTestKit-package](https://abs-dev.github.io/DiagTestKit/reference/DiagTestKit-package.md)
