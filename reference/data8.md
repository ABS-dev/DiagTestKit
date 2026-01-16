# Data set for Example 8 in vignette "DiagTestKit Examples"

Samples randomly selected from 3 populations tested by three 2-state
reference tests and a 3-state experimental test.

## Usage

``` r
data8
```

## Format

A data frame with 72 rows and six variables:

- `Population`: population identifier (A, B, C).

- `exp_result`: result of experimental test (positive, negative,
  suspect).

- `ref1_result`: result of first reference test (positive, negative).

- `ref2_result`: result of second reference test (positive, negative).

- `ref3_result`: result of third reference test (positive, negative).

- `count`: number of samples with the unique testing combination for the
  specific population.
