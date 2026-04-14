# Data set for Example 4 in vignette "DiagTestKit Examples"

Samples randomly selected from 3 populations tested by one fallible
3-state reference test and a 2-state experimental test.

## Usage

``` r
data4
```

## Format

A data frame with 18 rows and four variables:

- `population`: population identifier (A, B, C).

- `exp_result`: result of experimental test (positive, negative).

- `ref_result`: result of reference test (positive, negative, suspect).

- `count`: number of samples with the unique testing combination for the
  specific population.
