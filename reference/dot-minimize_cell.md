# minimize cell

A function used for optimizing the values of sensitivity and specificity
(and \\\delta\\ and \\\gamma\\ for a 3-state kit). The objective
function minimizes the sum of the squared deviations (expected -
observed cell counts).

## Usage

``` r
.minimize_cell(
  parm,
  SnR,
  SpR,
  Prev,
  xdat,
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

- parm:

  `vector` A vector of starting values to be used for the optimization
  that is passed to `.minimize_cell`. For a 2-state experimental test,
  this is a vector of length 2 with entries (\\\pi\\, \\\theta\\) For a
  3-state experimental test, this is a vector of length 4 with entries
  (\\\pi\\, \\\delta\\, \\\theta\\, \\\gamma\\). See also
  [estimateSnSp](https://abs-dev.github.io/DiagTestKit/reference/estimateSnSp.md).

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

- xdat:

  `vector` A vector of the observed cell counts.

- N_mat:

  `matrix` Needs to be filled out

- nstates:

  `vector` A vector with length one more than the number of reference
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

The sum of the squared deviations between the expected and observed cell
counts.

## Author

[DiagTestKit-package](https://abs-dev.github.io/DiagTestKit/reference/DiagTestKit-package.md)
