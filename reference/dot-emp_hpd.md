# Calculate the empirical hpd.

Empirical highest posterior density by shortest length interval.

## Usage

``` r
.emp_hpd(X, alpha)
```

## Arguments

- X:

  vector of values

- alpha:

  1 - confidence

## Value

highest posterior density (1-alpha) interval

## Note

Uses type 7 [quantile](https://rdrr.io/r/stats/quantile.html). Also used
in package `MF`

## Author

[DiagTestKit-package](https://abs-dev.github.io/DiagTestKit/reference/DiagTestKit-package.md)
