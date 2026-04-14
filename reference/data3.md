# Data set for Example 3 in vignette "DiagTestKit Examples"

Samples randomly selected from single population tested by one 3-state
fallible reference test and a 2-state experimental test.

## Usage

``` r
data3
```

## Format

A data frame with six rows and three variables:

- `exp_result`: result of experimental test (positive, negative).

- `ref_result`: result of reference test (positive, negative, suspect).

- `count`: number of samples with the unique testing combination.
