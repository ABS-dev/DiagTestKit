# control values for `estimateSnSp`

The values supplied in the function–call replace the defaults and a list
with all possible arguments is returned. The returned list is used as
the `control` argument to the function `estimateSnSp`.

## Usage

``` r
estimateSnSpControl(
  seed = NULL,
  Sn.distn = NULL,
  Sn.spread = NULL,
  Sp.distn = NULL,
  Sp.spread = NULL,
  prev.distn = NULL,
  prev.spread = NULL,
  tolerance = 0.001,
  alpha = 0.05,
  step.size = 1e-06,
  parm = NULL,
  rep.iter = TRUE,
  iter.n = 50
)
```

## Arguments

- seed:

  The seed used in the random generation of the distributions of
  sensitivity and specificity for all reference tests and prevalence of
  each population. See also
  [set.seed](https://rdrr.io/r/base/Random.html).

- Sn.distn:

  `vector` A named vector with length equal to the number of reference
  tests. Determines which disibution should be used for sampling
  sensitivity of each reference test. Inputs are "beta" or "triangular".
  Defaults to "beta" for each reference test.

- Sn.spread:

  `vector` A named vector with length equal to the number of reference
  tests. Describes the width of the distribution for the sensitivity of
  each reference test. Inputs are "wide", "medium", or "narrow".
  Defaults to "wide" for each reference test.

- Sp.distn:

  `vector` A named vector with length equal to the number of reference
  tests. Determines which disibution should be used for sampling
  specificity of each reference test. Inputs are "beta" or "triangular".
  Defaults to "beta" for each reference test.

- Sp.spread:

  `vector` A named vector with length equal to the number of reference
  tests. Describes the width of the distribution for the specificity of
  each reference test. Inputs are "wide", "medium", or "narrow".
  Defaults to "wide" for each reference test.

- prev.distn:

  `vector` A named vector with length equal to the number of
  populations. Determines which disibution should be used for sampling
  the prevalence of each population. Inputs are "beta" or "triangular".
  Defaults to "beta".

- prev.spread:

  `vector` A named vector with length equal to the number of
  populations. Describes the width of the distribution for the
  prevalence of each population. Inputs are "wide", "medium", or
  "narrow". Defaults to "wide" for each population.

- tolerance:

  Setting a limit on the pgtol used in the
  [optim](https://rdrr.io/r/stats/optim.html) function with the
  "L-BFGS-B" method. See also
  [optim](https://rdrr.io/r/stats/optim.html). Defaults to 1E-03.

- alpha:

  Significance levels. Defaults to 0.05.

- step.size:

  Provides the level of resolution in values simulated from a triangular
  distribution. Defaults to 1E-06.

- parm:

  `vector` Starting values for the optimization of the parameters of the
  experimental test. If the experimental test has 2 states, this vector
  is of length two with elements corresponding to sensitivity and
  specificity, respectively. If the experimental test has 3 states, this
  vector is of length 4 with elements corresponding to sensitivity
  (\\\pi\\), the proportion of 1-Sn corresponding to the suspect region
  for disease positive samples (\\\delta\\), specificity (\\\theta\\),
  and the proportion of 1-Sp corresponding to the suspect region for
  disease negative samples (\\\gamma\\). All values are between 0 and 1,
  inclusive.

- rep.iter:

  logical (`TRUE`/`FALSE`) Indicates if updates should be printed
  regarding the number of iterations completed. Defaluts to TRUE.

- iter.n:

  integer indicating the frequency of updates for the number of
  iterations completed. Defaluts to 50.

## Value

A list with the following elements (as defined above): `seed`,
`Sn.disn`, `Sn.spread`, `Sp.distn`, `Sp.spread`, `prev.distn`,
`prev.spread`, `tolerance`, `step.size`, `parm`.

## Author

[DiagTestKit-package](https://abs-dev.github.io/DiagTestKit/reference/DiagTestKit-package.md)

## Examples

``` r
estimateSnSpControl()
#> $seed
#> [1] 85795
#> 
#> $Sn.distn
#> NULL
#> 
#> $Sn.spread
#> NULL
#> 
#> $Sp.distn
#> NULL
#> 
#> $prev.distn
#> NULL
#> 
#> $prev.spread
#> NULL
#> 
#> $tolerance
#> [1] 0.001
#> 
#> $step.size
#> [1] 1e-06
#> 
#> $parm
#> NULL
#> 
#> $alpha
#> [1] 0.05
#> 
#> $rep.iter
#> [1] TRUE
#> 
#> $iter.n
#> [1] 50
#> 
estimateSnSpControl(seed = 64725)
#> $seed
#> [1] 64725
#> 
#> $Sn.distn
#> NULL
#> 
#> $Sn.spread
#> NULL
#> 
#> $Sp.distn
#> NULL
#> 
#> $prev.distn
#> NULL
#> 
#> $prev.spread
#> NULL
#> 
#> $tolerance
#> [1] 0.001
#> 
#> $step.size
#> [1] 1e-06
#> 
#> $parm
#> NULL
#> 
#> $alpha
#> [1] 0.05
#> 
#> $rep.iter
#> [1] TRUE
#> 
#> $iter.n
#> [1] 50
#> 
```
