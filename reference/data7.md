# Data set for Example 7 in vignette "DiagTestKit Examples"

Samples randomly selected from 2 populations tested with two 3-state
reference test and a 2-state experimental test.

## Usage

``` r
data7
```

## Format

A data frame with 39 rows and five variables:

- `Population`: population identifier (A, B).

- `exp_result`: result of experimental test (positive, negative).

- `ref1_result`: result of first reference test (positive, negative,
  suspect).

- `ref2_result`: result of second reference test (positive, negative,
  suspect).

- `count`: number of samples with the unique testing combination for the
  specific population.
