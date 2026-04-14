# Update alpha values for existing simulation

Report interval estimates with updated alpha values, using a previously
evaluated simulation.

## Usage

``` r
updateAlpha(x, newAlpha)
```

## Arguments

- x:

  output from
  [estimateSnSp](https://abs-dev.github.io/DiagTestKit/reference/estimateSnSp.md)

- newAlpha:

  updated alpha value. Must be within \[0, 1\]

## Value

an object of type `snsp`. See output for
[estimateSnSp](https://abs-dev.github.io/DiagTestKit/reference/estimateSnSp.md)

## See also

[estimateSnSpControl](https://abs-dev.github.io/DiagTestKit/reference/estimateSnSpControl.md)

## Author

[DiagTestKit-package](https://abs-dev.github.io/DiagTestKit/reference/DiagTestKit-package.md)

## Examples

``` r
data.1 <- data.frame(
  exp_result = rep(c("positive", "negative"), each = 2),
  ref1_result = rep(c("positive", "negative"), 2),
  count = c(82, 11, 5, 22))
example.1 <- estimateSnSp(dat      = data.1,
                          Sn.ref   = data.frame(ref = c(0.90, 0)),
                          Sp.ref   = data.frame(ref = c(0.99, 0)),
                          prev.pop = c(A = 0.80),
                          control  = estimateSnSpControl(seed = 64725))
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
example.1a <- updateAlpha(example.1, newAlpha = 0.25)
example.1a
#> 1000  simulations 
#> 75 % Interval Estimates
#> 
#>               Point.Estimate     Lower     Upper
#> Sn = P(T+|D+)      0.9449821 0.9053901 0.9791017
#> Sp = P(T-|D-)      0.9062769 0.8336064 1.0000000

# 1000  simulations
# 75 % Interval Estimates
#
#                    Point.Estimate     Lower     Upper
# Sn = P(T+|D+)      0.9449821          0.9053901 0.9791017
# Sp = P(T-|D-)      0.9062769          0.8336064 1.0000000
```
