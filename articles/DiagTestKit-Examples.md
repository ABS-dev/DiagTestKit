# Examples

## Introduction

This vignette includes several examples for estimating the performance
characteristics of an experimental kit testing samples from one or more
populations with the experimental test and one or more reference tests.
The examples in this vignette begin with a data frame containing the
counts for all possible testing combinations for all populations. For a
more basic example that includes obtaining the data frame of counts
necessary to use the `estimateSnSp` function, please see the [Getting
Started
Vignette](https://github.com/ABS-dev/DiagTestKit/blob/master/inst/doc/GettingStarted.pdf).
The notation used here is consistent with that found in [CVB
STATWI0002](https://www.aphis.usda.gov/aphis/ourfocus/animalhealth/veterinary-biologics/biologics-regulations-and-guidance/ct_vb_statwi).

The simplest case illustrated here is estimating the sensitivity and
specificity of a 2–state experimental test using the test results of the
experimental kit and those obtained from a 2–state fallible reference
test on samples tested from a single population. The time necessary to
perform all optimizations was slightly over a minute. The optimization
takes significantly longer when estimating the parameters for a 3–state
experimental test because the optimization is over 4 parameters for a
3–state experimental test, rather than over 2 parameters for a 2–state
experimental test. The most complex example here is a 3–state
experimental test, three 2–state fallible reference tests and samples
tested from 3 populations. The time necessary to perform all
optimizations was slightly less than 12 minutes using a Windows 10
platform with an Intel i7 processor. Code in this vignette was run using
version 0.6.11 of the `DiagTestKit` package.

## Example 1 — One, 2–state reference test, One population, 2–state experimental test

In this example, samples randomly selected from a single population are
tested by one 2–state fallible reference test and a 2–state experimental
test. Here, \\S_1\\ = 2, \\S_2\\ = 2 and \\K\\ = 4. There are 4 unique
sets of testing combinations. Further, the probabilities associated with
a suspect test result are 0 for both the experimental test method and
the reference test method. Specifically, \\\phi_1\\ = \\\psi_1\\ =
\\\phi_2\\ = \\\psi_2\\ = 0. This example uses named data frames for the
input variables `Sn.ref` and `Sp.ref`.

``` r
ex1 <- estimateSnSp(dat = data1,
          Sn.ref = data.frame(ref = c(0.90, 0)),
          Sp.ref = data.frame(ref = c(0.99, 0)),
          prev.pop = c(A = 0.80),
          control = estimateSnSpControl(seed = 64725,
                                        rep.iter = FALSE))
```

    ## Warning in estimateSnSp(dat = data1, Sn.ref =
    ## data.frame(ref = c(0.9, 0)), : The data suggests
    ## a single population was tested

    ## The optimization has begun

``` r
unique(ex1$detailOut$Converge)
```

    ## [1] 0

``` r
unique(ex1$detailOut$Message)
```

    ## [1] "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL"
    ## [2] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

``` r
ex1
```

    ## 1000  simulations 
    ## 95 % Interval Estimates
    ## 
    ##               Point.Estimate     Lower Upper
    ## Sn = P(T+|D+)      0.9449821 0.9019639     1
    ## Sp = P(T-|D-)      0.9062769 0.7523346     1

### Changing the confidence level

The confidence level can be updated without requiring repeating the
simulations.

``` r
ex1_update <- updateAlpha(ex1, newAlpha = 0.01)
ex1_update
```

    ## 1000  simulations 
    ## 99 % Interval Estimates
    ## 
    ##               Point.Estimate     Lower Upper
    ## Sn = P(T+|D+)      0.9449821 0.8848417     1
    ## Sp = P(T-|D-)      0.9062769 0.7111645     1

## Example 2 — One, 2–state reference test, Three populations, 2–state experimental test

In this example, samples randomly selected from 3 populations are tested
by one 2–state fallible reference test and a 2–state experimental test.
Here, \\S_1\\ = 2, \\S_2\\ = 2 and \\K\\ = 4. Again there are 4 unique
sets of testing combinations, but a total of 12 counts, one for each
unique test combination in each of the three populations. The
probabilities associated with a suspect test result are 0 for both the
experimental test method and the reference test method. Specifically,
\\\psi_1\\ = \\\phi_1\\ = \\\psi_2\\ = \\\phi_2\\ = 0. This example uses
named vectors for the input variables `Sn.ref` and `Sp.ref` because the
reference test only had 2 states.

``` r
ex2 <- estimateSnSp(dat = data2,
         Sn.ref = c(ref_result = 0.90),
         Sp.ref = c(ref_result = 0.94),
         prev.pop = c(A = 0.92, B = 0.20, C = 0.50),
         control = estimateSnSpControl(seed = 4902342,
                                       rep.iter = FALSE))
```

    ## The optimization has begun

``` r
unique(ex2$detailOut$Converge)
```

    ## [1] 0

``` r
unique(ex2$detailOut$Message)
```

    ## [1] "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL"

``` r
ex2
```

    ## 1000  simulations 
    ## 95 % Interval Estimates
    ## 
    ##               Point.Estimate     Lower     Upper
    ## Sn = P(T+|D+)      0.8614248 0.8171083 0.9162939
    ## Sp = P(T-|D-)      0.9818038 0.9260871 1.0000000

## Example 3 — One, 3–state reference test, One population, 2–state experimental test

Samples randomly selected from a single population are tested by one
3–state fallible reference test and a 2–state experimental test. Here,
\\S_1\\ = 2, \\S_2\\ = 3 and \\K\\ = 6. There are 6 unique sets of
testing combinations. The probabilities associated with a suspect test
result for the experimental test are zero (i.e. \\\psi_1\\ = \\\phi_1\\
= 0). Because the reference test has 3 states, the input must include
values of \\\delta_2 = \frac{\psi_2}{1 - \pi_2}\\ and \\\gamma_2 =
\frac{\phi_2}{1 - \theta_2}\\.

``` r
ex3 <- estimateSnSp(dat = data3,
        Sn.ref = data.frame(ref = c(0.99, 0.75)),
        Sp.ref = data.frame(ref = c(0.85, 0.67)),
        prev.pop = c(A = 0.92),
        control = estimateSnSpControl(seed = 896421,
                                      rep.iter = FALSE))
```

    ## Warning in estimateSnSp(dat = data3, Sn.ref =
    ## data.frame(ref = c(0.99, 0.75)), : The data
    ## suggests a single population was tested

    ## The optimization has begun

``` r
unique(ex3$detailOut$Converge)
```

    ## [1] 0

``` r
unique(ex3$detailOut$Message)
```

    ## [1] "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL"
    ## [2] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

``` r
ex3
```

    ## 1000  simulations 
    ## 95 % Interval Estimates
    ## 
    ##               Point.Estimate     Lower     Upper
    ## Sn = P(T+|D+)      0.9195444 0.8882768 0.9792288
    ## Sp = P(T-|D-)      0.9211223 0.6734354 1.0000000

## Example 4 — One, 3–state reference test, Three populations, 2–state experimental test

In this example, samples randomly selected from three populations are
tested by one fallible 3–state reference test and a 2–state experimental
test. There are 6 unique sets of testing combinations (\\S_1\\ = 2,
\\S_2\\ = 3 and \\K\\ = 6) and a total of 18 counts, one for each unique
test combination in each population. The probabilities associated with a
suspect test result for the experimental test are zero (i.e. \\\psi_1\\
= \\\phi_1\\ = 0).

``` r
ex4 <- estimateSnSp(dat = data4,
        Sn.ref = data.frame(ref = c(0.95, 0.55)),
        Sp.ref = data.frame(ref = c(0.93, 0.48)),
        prev.pop = c(A = 0.97, B = 0.25, C = 0.68),
        control = estimateSnSpControl(seed = 6589732,
                                    rep.iter = FALSE))
```

    ## The optimization has begun

``` r
unique(ex4$detailOut$Converge)
```

    ## [1] 0

``` r
unique(ex4$detailOut$Message)
```

    ## [1] "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL"

``` r
ex4
```

    ## 1000  simulations 
    ## 95 % Interval Estimates
    ## 
    ##               Point.Estimate     Lower     Upper
    ## Sn = P(T+|D+)      0.9101593 0.8722713 0.9675238
    ## Sp = P(T-|D-)      0.8802044 0.8221522 0.9373587

## Example 5 — One, 2–state reference test, Two populations, 3–state experimental test

In this example, samples randomly selected from two populations are
tested by a 3–state experimental test and one 2–state reference test.
There are 6 unique testing combinations (\\S_1\\ = 3, \\S_2\\ = 2 and
\\K\\ = 6) and a total of 12 counts as samples were obtained from 2
populations. The probabilities associated with a suspect test result for
the experimental test (\\\psi_1\\ and \\\phi_1\\) are included in the
output (but the optimization occurs in terms of \\\delta_1\\ and
\\\gamma_1\\). The function provides a message to inform the user that
the experimental test has 3 states and the optimization will be more
time consuming.

``` r
ex5 <- estimateSnSp(dat = data5,
         Sn.ref = c(Ref1 = 0.90),
         Sp.ref = c(Ref1 = 0.99),
         prev.pop = c(A = 0.80, B = 0.90),
         control = estimateSnSpControl(seed = 1249856,
                                       rep.iter = FALSE))
```

    ## Optimization is more time consuming for a 3-state experimental test, be patient!

    ## The optimization has begun

``` r
unique(ex5$detailOut$Converge)
```

    ## [1] 0

``` r
unique(ex5$detailOut$Message)
```

    ## [1] "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL"
    ## [2] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

``` r
ex5
```

    ## 1000  simulations 
    ## 95 % Interval Estimates
    ## 
    ##               Point.Estimate     Lower      Upper
    ## Sn = P(T+|D+)     0.94126806 0.8937435 1.00000000
    ## Sp = P(T-|D-)     0.87213528 0.7169586 1.00000000
    ## P(T?|D+)          0.03792794 0.0000000 0.06171887
    ## P(T?|D-)          0.04930292 0.0000000 0.16122575

## Example 6 — Two, 2–state reference tests, One population, 2–state experimental

In this example, samples randomly selected from a single population are
tested with two 2–state reference tests and a 2–state experimental test.
Here, \\S_1\\ = 2, \\S_2\\ = 2, \\S_3\\ = 2 and \\K\\ = 8. There are 8
unique sets of testing combinations. The probabilities associated with a
suspect test result for the experimental test are zero (i.e. \\\psi_1\\
= \\\phi_1\\ = 0).

``` r
ex6 <- estimateSnSp(dat = data6,
          Sn.ref = c(Ref1_result = 0.95, ref2 = 0.91),
          Sp.ref = c(Ref1_result = 0.85, ref2 = 0.98),
          prev.pop = c(A = 0.82),
          control = estimateSnSpControl(seed = 2948217,
                                        rep.iter = FALSE))
```

    ## Warning in estimateSnSp(dat = data6, Sn.ref =
    ## c(Ref1_result = 0.95, ref2 = 0.91), : The data
    ## suggests a single population was tested

    ## The optimization has begun

``` r
unique(ex6$detailOut$Converge)
```

    ## [1] 0

``` r
unique(ex6$detailOut$Message)
```

    ## [1] "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL"
    ## [2] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

``` r
ex6
```

    ## 1000  simulations 
    ## 95 % Interval Estimates
    ## 
    ##               Point.Estimate     Lower Upper
    ## Sn = P(T+|D+)      0.9589171 0.9106663     1
    ## Sp = P(T-|D-)      0.9456264 0.7962622     1

## Example 7 — Two, 3–state reference tests, Two populations, 2–state experimental

In this example, samples selected from 2 populations were tested by two
3–state reference tests and a 2–state experimental test. Here, \\S_1\\ =
2, \\S_2\\ = 3, \\S_3\\ = 3 and \\K\\ = 18. There are 18 unique sets of
testing combinations and a total of 36 counts, one for each unique test
combination in each of the 2 populations. The probabilities associated
with a suspect test result for the experimental test are zero
(i.e. \\\psi_1\\ = \\\phi_1\\ = 0).

``` r
ex7 <- estimateSnSp(dat = data7,
          Sn.ref = data.frame(ref1 = c(0.88, 0.75), ref2 = c(0.90, 0.55)),
          Sp.ref = data.frame(ref1 = c(0.97, 0.6), ref2 = c(0.95, 0.5)),
          prev.pop = c(A = 0.87, B = 0.35),
          control = estimateSnSpControl(seed = 1937457,
                                        rep.iter = FALSE))
```

    ## The optimization has begun

``` r
unique(ex7$detailOut$Converge)
```

    ## [1]  0 52

``` r
unique(ex7$detailOut$Message)
```

    ## [1] "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL"
    ## [2] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH" 
    ## [3] "ERROR: ABNORMAL_TERMINATION_IN_LNSRCH"

``` r
ex7
```

    ## 1000  simulations 
    ## 95 % Interval Estimates
    ## 
    ##               Point.Estimate     Lower Upper
    ## Sn = P(T+|D+)      0.9661330 0.9157368     1
    ## Sp = P(T-|D-)      0.9215106 0.8715217     1

## Example 8 — Three, 2–state reference tests, Three populations, 3–state experimental

In this example, samples randomly selected from 3 populations were
tested by three 2–state reference tests and a 3–state experimental test.
Here, \\S_1\\ = 3, \\S_2\\ = 2, \\S_3\\ = 2, \\S_4\\ = 2 and \\K\\ = 24.
There are 24 unique sets of testing combinations and a total of 72
counts, one for each unique test combination in each of the 3
populations. The probabilities associated with a suspect test result for
the experimental test (\\\psi_1\\ and \\\phi_1\\) are included in the
output. The function provides a message to inform the user that the
experimental test has 3 states and the optimization will be more time
consuming.

``` r
ex8 <- estimateSnSp(dat = data8,
       Sn.ref = c(ref1_result = 0.92, ref2_result = 0.88, ref3_result = 0.85),
       Sp.ref = c(ref1_result = 0.86, ref2_result = 0.90, ref3_result = 0.92),
       prev.pop = c(A = 0.95, B = 0.62, C = 0.18),
       control = estimateSnSpControl(seed = 865213,
                                     rep.iter = FALSE))
```

    ## Optimization is more time consuming for a 3-state experimental test, be patient!

    ## The optimization has begun

``` r
unique(ex8$detailOut$Converge)
```

    ## [1] 0

``` r
unique(ex8$detailOut$Message)
```

    ## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH" 
    ## [2] "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL"

``` r
ex8
```

    ## 1000  simulations 
    ## 95 % Interval Estimates
    ## 
    ##               Point.Estimate     Lower      Upper
    ## Sn = P(T+|D+)     0.96541704 0.8879949 1.00000000
    ## Sp = P(T-|D-)     0.98351924 0.9016964 1.00000000
    ## P(T?|D+)          0.02568427 0.0000000 0.06339616
    ## P(T?|D-)          0.01534125 0.0000000 0.05604950
