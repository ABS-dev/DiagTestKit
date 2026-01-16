# Data set for Example 5 in vignette "DiagTestKit Examples"

Samples randomly selected from 2 populations tested by one 2-state
reference test and a 3-state experimental test.

## Usage

``` r
data5
```

## Format

A data frame with 11 rows and four variables:

- `Population`: population identifier (A, B).

- `exp_result`: result of experimental test (positive, negative,
  suspect).

- `ref_result`: result of reference test (positive, negative).

- `count`: number of samples with the unique testing combination for the
  specific population.
