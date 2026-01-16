# Data set for Example 2 in vignette "DiagTestKit Examples"

Samples randomly selected from 3 populations are tested by one 2-state
fallible reference test and a 2-state experimental test.

## Usage

``` r
data2
```

## Format

A data frame with 12 rows and four variables:

- `population`: population identifier (A, B, C).

- `exp_result`: result of experimental test (positive, negative).

- `ref_result`: result of reference test (positive, negative).

- `count`: number of samples with the unique testing combination for the
  specific population.
