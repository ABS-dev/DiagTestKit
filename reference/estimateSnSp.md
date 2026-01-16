# Estimate Sensitivity and Specificity

A function written by CVB Statistics to estimate the sensitivity and
specificity of an experimental diagnostic test kit in accordance with
[CVB
STATWI0002](https://www.aphis.usda.gov/aphis/ourfocus/animalhealth/veterinary-biologics/biologics-regulations-and-guidance/ct_vb_statwi).

## Usage

``` r
estimateSnSp(dat, Sn.ref, Sp.ref, prev.pop, nsim = 1000, control = NULL)
```

## Arguments

- dat:

  `data.frame` This is a data frame where the first column includes
  information for the population sampled (if more than one population is
  sampled). The next column is the possible outcomes of the experimental
  test followed by one column for the possible outcomes for each
  reference test (one column per test). The last column of the data
  frame provides the number of samples with each pattern of test
  outcomes. The columns must be included in the order described. If more
  than one population is sampled, the column name for the column
  containing the population information must be "population". The column
  containing the test results for the experimental test must have "exp"
  in the name, such as experimental, experiment, exp, Exp, etc. The
  column names containing the reference test results much contain "ref"
  in the name, such as Ref1, Ref2, ref1_results, Reference2, etc.

- Sn.ref:

  `data.frame` Each column corresponds to one reference test. Row 1
  contains the sensitivity for the reference test(s). Row 2 contains the
  probability of a suspect result as a fraction of the non-correct test
  result. This is a value between 0 and 1 (inclusive). Namely, P(T? \|
  D+) = \\\psi\\ = \\\delta\\ \* (1 - \\\pi\\) where \\\delta\\ is the
  second row for a given column (reference test). \\\delta =
  \frac{\psi}{(1 - \pi)}\\. Use a zero for a 2-state test (i.e. no
  suspect region). Alternatively, if all reference tests are 2-state
  tests, the sensitivities can be input as a named vector. Specifically,
  each element in the vector must be given a name which includes "ref"
  (see above) and the column names (or names of the elements within the
  vector) must match those for Sp.ref.

- Sp.ref:

  `data.frame` Each column corresponds to one reference test. Row 1
  contains the specificity for each reference test. Row 2 contains the
  probability of a suspect result as a fraction of the non-correct test
  result. This is a value between 0 and 1 (inclusive). Namely, P(T? \|
  D-) = \\\phi\\ = \\\gamma\\ \* (1 - \\\theta\\) where \\\gamma\\ is
  the second row for a given column (reference test). \\\gamma =
  \frac{\phi}{(1 - \theta)}\\. Use a zero for a 2-state test (i.e. no
  suspect region). Alternatively, if all reference tests are 2-state
  tests, the specificity can can be input as a named vector.
  Specifically, each element in the vector must be given a name which
  includes "ref" (see above) and the column names (or names of the
  elements within the vector) must match those for Sn.ref.

- prev.pop:

  `vector` A named vector containing the prevalence for each population
  sampled. The names in the vector must match the population labels used
  in "dat".

- nsim:

  The number of simulations to draw from the sensitivity and specificity
  distribution(s) for each reference test and the prevalence
  distribution from each population.

- control:

  list of control values to replace defaults. See
  [estimateSnSpControl](https://abs-dev.github.io/DiagTestKit/reference/estimateSnSpControl.md)
  for details.

## Value

An object of type `snsp` that extends `list`.

- `calcVal`: Point estimates and estimated simulated intervals for
  properties of the experimental kit. See below;

- `detailOut`: Detailed output values. See below;

- `input`: Simulated values. See below;

## `calcVal`

A list with the following values which will include the following for
both 2- and 3-state experimental tests –

- `Nsim`: Number of simulations performed.

- `Confidence`: 1 - \\\alpha\\.

- `SnPE`: Sensitivity point estimate obtained as the median of the
  estimated values.

- `SnInterval`: Estimated simulated interval for sensitivity;

- `SpPE`: Specificity point estimate obtained as the median of the
  estimated values;

- `SpInterval`: Estimated simulated interval for specificity;

  If three states, the list will also include –

- `SusDisPosPE`: Point estimate for the probability of test suspect
  given disease positive (\\\psi\\) which is the median of the
  calculated values (\\\psi\\ = \\\delta\\(1-\\\pi\\));

- `SusDisPosInterval`: Estimated simulated interval for the probability
  of test suspect given disease positive (\\\psi\\);

- `SusDisNegPE`: Point estimate for the probability of test suspect
  given disease negative (\\\phi\\) which is the median of the
  calculated values (\\\phi\\ = \\\gamma\\(1-\\\theta\\));

- `SusDisNegInterval`: Estimated simulated interval for the probability
  of test suspect given disease negative (\\\phi\\);

## `detailOut`

A list with the following detailed output values which will include the
following for both 2- and 3-state experimental tests –

- `Exp.Sn`: `vector` The optimized values for the sensitivity of the
  experimental test kit;

- `Exp.Sp`: `vector` The optimized values for the specificity of the
  experimental test kit;

- `Converge`: `vector` Each entry is an integer code detailing the
  convergence of the optimization for each iteration. 0 indicates
  successful completion. See also
  [optim](https://rdrr.io/r/stats/optim.html);

- `Message`: `vector` Each entry includes a character string providing
  any additional information returned by the optimizer or NULL. See also
  [optim](https://rdrr.io/r/stats/optim.html);

  If three states, the list will also include –

- `Exp.pos.p`: `vector` The optimized values for the proportion of the
  remaining probability (1-Sn) that corresponds to a suspect region for
  diseased samples, namely \\\delta\\;

- `Exp.sus.pos`: `vector` The values for `P(T? | D+)` (\\\psi\\)
  calculated from `Exp.sn` and Exp.pos.p. `P(T?|D+) =` \\\delta\\ \*
  (1 - \\\pi\\);

- `Exp.neg.p`: `vector` The optimized value for the proportion of the
  remaining probability `(1-Sp)` that corresponds to a suspect region
  for non-diseased samples, namely \\\gamma\\;

- `Exp.sus.neg`: `vector` The values for P(T? \| D-) (\\\phi\\)
  calculated from Exp.sp and Exp.neg.p. P(T?\|D-) = \\\gamma\\ \* (1 -
  \\\theta\\);

## `input`

A list containing the seed used and the simulated values.

- `seed`: The seed used in the random generation of the distributions of
  sensitivity and specificity for all reference tests and prevalence of
  each population. See also
  [set.seed](https://rdrr.io/r/base/Random.html).

- `Sn.sims`: `matrix` The simulated values for the sensitivity of each
  reference test and \\\psi\\ where \\\psi\\ was specified in the second
  row of Sn.ref (or zero if Sn.ref was a vector). The first two columns
  correspond to the first reference test, columns 3 and 4 to the second
  reference test if it exists, etc.

- `Sp.sims`: `matrix` The simulated values for the specificity of each
  reference test and \\\phi\\ where \\\phi\\ was specified in the second
  row of Sp.ref (or zero is Sp.ref was a vector). The first two columns
  correspond to the first reference test, columns 3 and 4 to the second
  reference test if it exists, etc.

- `prev.sims`: `matrix` The simulated values of prevalence for each
  population. Each column correspond to one population.

## See also

[estimateSnSpControl](https://abs-dev.github.io/DiagTestKit/reference/estimateSnSpControl.md)

## Author

[DiagTestKit-package](https://abs-dev.github.io/DiagTestKit/reference/DiagTestKit-package.md)

## Examples

``` r
data.1 <- data.frame(exp_result = rep(c("positive", "negative"), each = 2),
                     ref1_result = rep(c("positive", "negative"), 2),
                     count = c(82, 11, 5, 22))
example.1 <- estimateSnSp(dat = data.1,
                          Sn.ref = data.frame(ref = c(0.90, 0)),
                          Sp.ref = data.frame(ref = c(0.99, 0)),
                          prev.pop = c(A = 0.80),
                          control = estimateSnSpControl(seed = 64725))
#> Warning: The data suggests a single population was tested
#> The optimization has begun
#> The following is the number of iterations completed: 50
#> The following is the number of iterations completed: 100
#> The following is the number of iterations completed: 150
#> The following is the number of iterations completed: 200
#> The following is the number of iterations completed: 250
#> The following is the number of iterations completed: 300
#> The following is the number of iterations completed: 350
#> The following is the number of iterations completed: 400
#> The following is the number of iterations completed: 450
#> The following is the number of iterations completed: 500
#> The following is the number of iterations completed: 550
#> The following is the number of iterations completed: 600
#> The following is the number of iterations completed: 650
#> The following is the number of iterations completed: 700
#> The following is the number of iterations completed: 750
#> The following is the number of iterations completed: 800
#> The following is the number of iterations completed: 850
#> The following is the number of iterations completed: 900
#> The following is the number of iterations completed: 950
#> The following is the number of iterations completed: 1000
example.1
#> 1000  simulations 
#> 95 % Interval Estimates
#> 
#>               Point.Estimate     Lower Upper
#> Sn = P(T+|D+)      0.9449821 0.9019639     1
#> Sp = P(T-|D-)      0.9062769 0.7523346     1

# 1000  simulations
# 95 % Interval Estimates
#
#               Point.Estimate     Lower  Upper
# Sn = P(T+|D+)      0.9449821 0.9019639      1
# Sp = P(T-|D-)      0.9062769 0.7523346      1


if (FALSE) { # \dontrun{
data.2 <- data.frame(Population = rep(LETTERS[1:3], each = 24),
                     exp_result = rep(rep(
                       c("negative", "positive", "suspect"), each = 8), 3),
                     ref1_result = rep(rep(
                       c("negative", "positive"), each = 4), 9),
                     ref2_result = rep(rep(
                       c("negative", "positive"), each = 2), 18),
                     ref3_result = rep(c("negative", "positive"), 36),
                     count = c(3, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 5,
                               1, 8, 11, 62, 0, 0, 0, 0, 0, 0, 0,
                               2, 27, 2, 3, 0, 4, 0, 1, 1, 0, 0, 1,
                               4, 1, 6, 7, 41, 0, 0, 0, 0, 0, 0,
                               0, 2, 57, 5, 6, 1, 9, 1, 1, 0, 0, 0,
                               0, 1, 0, 2, 2, 12, 1, 0, 0, 0, 0, 0, 0, 0))
example.2 <- estimateSnSp(dat = data.2,
  Sn.ref = data.frame(ref1 = c(0.92, 0),
                      ref2 = c(0.88, 0),
                      ref3 = c(0.85, 0)),
  Sp.ref = data.frame(ref1 = c(0.86, 0),
                      ref2 = c(0.90, 0),
                      ref3 = c(0.92, 0)),
  prev.pop = c(A = 0.95, B = 0.62, C = 0.18),
  control = estimateSnSpControl(seed = 865213))
# 1000  simulations
# 95 % Interval Estimates

#                Point.Estimate     Lower      Upper
# Sn = P(T+|D+)      0.96541704 0.8879949 1.00000000
# Sp = P(T-|D-)      0.98351924 0.9016964 1.00000000
# SsP = P(T?|D+)     0.02568427 0.0000000 0.06339616
# SsN = P(T?|D-)     0.01534125 0.0000000 0.05604950
} # }
```
