# Binomial confidence interval, Clopper-Pearson method.

Evaluate binomial confidence interval using Clopper-Pearson method. A
function written by CVB Statistics to estimate the sensitivity and
specificity of an experimental diagnostic test kit in accordance with
[CVB
STATWI0002](https://www.aphis.usda.gov/aphis/ourfocus/animalhealth/veterinary-biologics/biologics-regulations-and-guidance/ct_vb_statwi).

## Usage

``` r
cloppearSnSp(dat, alpha = 0.05, est.Sn = TRUE)
```

## Arguments

- dat:

  `data.frame` A data frame with a column for the experimental test
  results, a column for the infallible reference test results, and a
  column for the corresponding count. The column name for the
  experimental test results must contatin "exp" and the column name for
  the infallible reference test results must include "ref". The counts
  should be the last column.

- alpha:

  Complement of confidence level.

- est.Sn:

  logical (TRUE/FALSE) Indicating if the sensitivity and its confidence
  interval should be supplied (TRUE) or if the specificity and its
  confidence interval should be supplied (FALSE).

## Value

An object of type `cp` that extends `list`.

- `calcVal`: Named vector of point estimates and estimated simulated
  intervals. See below.

- `data`: Test and Total values of the data. See below.

- `alpha`: Complement of the confidence interval as provided above.

## If `est.Sn == TRUE`

`calcVal` is a list with the following elements

- `Sn`: Sensitivity estimate.

- `Sn.LL`: Lower confidence limit for sensitivity.

- `Sn.UL`: Upper confidence limit for sensitivity.

`data` is a list with the following elements

- `Test.Positive`: Number of experimental test positives.

- `Total.Positive`: Total number of positive samples.

## If `st.Sn == FALSE`

`calcVal` is a list with the following elements

- `Sp`: Specificity estimate.

- `Sp.LL`: Lower confidence limit for specificity.

- `Sp.UL`: Upper confidence limit for specificity.

`data` is a list with the following elements

- `Test.Negative`: Number of experimental test negatives.

- `Total.Negative`: Total number of negative samples.

A matrix with a single row. If est.Sn = TRUE the columns correspond to
the number of experimental test positives, the total number of positive
samples, sensitivity, the lower confidence limit for sensitivity and the
upper confidence limit for sensitivity. If est.Sn = FALSE, the columns
correspond to the number of experimental test negatives, the total
number of negative samples, specificity, the lower confidence limit for
specificity and the upper confidence limit for specificity.

## References

Clopper CJ, Pearson ES, 1934. The use of confidence or fiducial limits
illustrated in the case of the binomial. *Biometrika* 26:404-413.

## Author

[DiagTestKit-package](https://abs-dev.github.io/DiagTestKit/reference/DiagTestKit-package.md)

## Examples

``` r
CP.Sn <- cloppearSnSp(dat = dat_infal, est.Sn = TRUE)
CP.Sn
#> [1] "Sn = P(T+|D+): 0.987013 (95% CI: 0.953876, 0.998423)"
# Sn = P(T+|D+): 0.987013 (95% CI: 0.953876, 0.998423)
CP.Sp <- cloppearSnSp(dat = dat_infal, est.Sn = FALSE)
CP.Sp
#> [1] "Sp = P(T-|D-): 0.970297 (95% CI: 0.915643, 0.993832)"
# Sp = P(T-|D-): 0.970297 (95% CI: 0.915643, 0.915643)
```
