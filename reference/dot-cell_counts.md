# Obtain cell counts

This function creates expected cell counts (and probabilities) for a
specific test pattern based on the diagnostic characteristics of the
reference test(s) and experimental test.

## Usage

``` r
.cell_counts(
  SnR,
  SpR,
  Prev,
  SnE,
  SpE,
  sus.perc,
  N_mat,
  nstates,
  suspect2staterows,
  X,
  Xpos,
  Xsus,
  Xneg,
  ncells,
  ntests
)
```

## Arguments

- SnR:

  `data.frame` Each column corresponds to one reference test. Row 1
  contains the sensitivity for the reference test(s). Row 2 contains the
  probability of a suspect result as a fraction of the non-correct test
  result. This is a value between 0 and 1 (inclusive). Namely, P(T? \|
  D+) = \\\psi\\ = \\\delta\\ \* (1 - \\\pi\\) where \\\delta\\ is the
  second row for a given column (reference test). \\\delta =
  \frac{\psi}{(1 - \pi)}\\. Use a zero for a 2-state test (i.e. no
  suspect region).

- SpR:

  `data.frame` Each column corresponds to one reference test. Row 1
  contains the specificity for each reference test. Row 2 contains the
  probability of a suspect result as a fraction of the non-correct test
  result. This is a value between 0 and 1 (inclusive). Namely, P(T? \|
  D-) = \\\phi\\ = \\\gamma\\ \* (1 - \\\theta\\) where \\\gamma\\ is
  the second row for a given column (reference test). \\\gamma =
  \frac{\phi}{(1 - \theta)}\\. Use a zero for a 2-state test (i.e. no
  suspect region).

- Prev:

  `vector` A named vector containing the prevalence for each population
  sampled.

- SnE:

  Sensitivity of the experimental test kit.

- SpE:

  Specificity of the experimental test kit.

- sus.perc:

  `vector` A vector containing 2 elements, c(\\\delta\\, \\\gamma\\) for
  the experimental test kit. A vector of zeros for a 2-state
  experimental kit. \\\delta\\ and \\\gamma\\ are values between 0 and 1
  (inclusive) corresponding to the proportion of the remaining
  probability (i.e. 1 - \\\pi\\ or 1 - \\\theta\\) that is suspect
  (\\\psi\\ or \\\phi\\). \\\delta = \frac{\psi}{(1-\pi)}\\ and \\\gamma
  = \frac{\phi}{(1-\theta)}\\.

- N_mat:

  `matrix` Needs to be filled out

- nstates:

  `vector` A vector with length one greater than the number of reference
  tests. The first element is the number of states of the experimental
  test and the remaining entries are the number of states of each
  reference test (using the same ordering as SnR and SpR).

- suspect2staterows:

  Needs to be filled out.

- X:

  Needs to be filled out.

- Xpos:

  Needs to be filled out.

- Xsus:

  Needs to be filled out.

- Xneg:

  Needs to be filled out.

- ncells:

  Needs to be filled out.

- ntests:

  Needs to be filled out.

## Value

`vector` A vector of expected counts corresponding to the properties of
the reference and experimental tests. The expected counts are obtained
based on a conditional independence assumption of all test methods.

## Author

[DiagTestKit-package](https://abs-dev.github.io/DiagTestKit/reference/DiagTestKit-package.md)
