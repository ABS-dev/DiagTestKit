# Optimization of Sensitivity and Specificity

Determine final optimized values for the sensitivity and specificity of
an experimental test kit (and probability of suspect given disease
positive and given disease negative for a 3-state kit).

## Usage

``` r
.get_values(
  dat,
  SnR.vec,
  SpR.vec,
  prev.vec,
  N.vec,
  nstates,
  tolerance,
  rep.iter,
  iter.n,
  parm = NULL
)
```

## Arguments

- dat:

  `vector` A vector of counts ordered in a manner consistent with output
  from the .cell_counts function.

- SnR.vec:

  `data.frame` Each column corresponds to one reference test. Row 1
  contains the sensitivity for the reference test(s). Row 2 contains the
  probability of a suspect result as a fraction of the non-correct test
  result. This is a value between 0 and 1 (inclusive). Namely, P(T? \|
  D+) = \\\psi\\ = \\\delta\\ \* (1 - \\\pi\\) where \\\delta\\ is the
  second row for a given column (reference test). \\\delta =
  \frac{\psi}{(1 - \pi)}\\. Use a zero for a 2-state test (i.e. no
  suspect region).

- SpR.vec:

  `data.frame` Each column corresponds to one reference test. Row 1
  contains the specificity for each reference test. Row 2 contains the
  probability of a suspect result as a fraction of the non-correct test
  result. This is a value between 0 and 1 (inclusive). Namely, P(T? \|
  D-) = \\\phi\\ = \\\gamma\\ \* (1 - \\\theta\\) where \\\gamma\\ is
  the second row for a given column (reference test). \\\gamma =
  \frac{\phi}{(1 - \theta)}\\. Use a zero for a 2-state test (i.e. no
  suspect region).

- prev.vec:

  `vector` A named vector containing the prevalence for each population
  sampled.

- N.vec:

  `vector` A named vector containing the sample size for each population
  sampled.

- nstates:

  `vector` A vector with length one more than the number of reference
  tests. The first element is the number of states of the experimental
  test and the remaining entries are the number of states of each
  reference test (using the same ordering as SnR.vec and SpR.vec).

- tolerance:

  Setting a limit on the pgtol used in the optim function with the
  "L-BFGS-B" method. See also
  [optim](https://rdrr.io/r/stats/optim.html).

- rep.iter:

  logical (TRUE/FALSE) Indicates if updates should be printed regarding
  the number of iterations completed.

- iter.n:

  integer indicating the frequency of updates for the number of
  iterations completed.

- parm:

  `vector` A vector of starting values to be used for the optimization
  that is passed to `.minimize_cell`. For a 2-state experimental test,
  this is a vector of length 2 with entries (\\\pi\\, \\\theta\\). For a
  3-state experimental test, this is a vector of length 4 with entries
  (\\\pi\\, \\\delta\\, \\\theta\\, \\\gamma\\). See also
  [estimateSnSp](https://abs-dev.github.io/DiagTestKit/reference/estimateSnSp.md).

## Value

A list:

The following will be returned for both 2 and 3-state experimental tests
–

- `sens.final`: `vector` The optimized values for the sensitivity of the
  experimental test kit.

- `spec.final`: `vector` The optimized values for the specificity of the
  experimental test kit.

- `converge`: `vector` Each entry is an integer code detailing the
  convergence of the optimization for each iteration. 0 indicates
  successful completion. See also
  [optim](https://rdrr.io/r/stats/optim.html).

- `message`: `vector` Each entry includes a character string giving any
  additional information returned by the optimizer or NULL. See also
  [optim](https://rdrr.io/r/stats/optim.html).

If three states –

- \\\delta\\: `vector` The optimized values for the probability of a
  suspect result as a fraction of the non-correct test result for
  diseased samples.

- \\\gamma\\: `vector` The optimized value for the probability of a
  suspect result as a fraction of the non-correct test result for
  non-diseased samples.

## Author

[DiagTestKit-package](https://abs-dev.github.io/DiagTestKit/reference/DiagTestKit-package.md)
