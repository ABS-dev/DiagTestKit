# Counts data used in vignette "DiagTestKit GettingStarted" section 4

Counts of the positive and negative results for experimental and
reference tests. A 2-state experimental test when an infallible
reference test has been used to determine the true disease status of
each sample.

## Usage

``` r
dat_infal
```

## Format

A data frame with 4 rows and 3 variables:

- `Experimental`: Result of the experimental test (positive or
  negative).

- `Reference`: Result of the reference test (positive or negative).

- `Count`: Number of samples observed with the unique testing
  combination.
