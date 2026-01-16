# Create Triangular Distribution

Creates a discrete step/triangular distribution that can be used to
sample values for sensitivity or specificity of a reference test or
prevalence of a population.

## Usage

``` r
.create_triangle_dist(
  m,
  w,
  h,
  threestate = FALSE,
  suspect = 2/3,
  step_size = 0.005,
  p_proportion = TRUE
)
```

## Arguments

- m:

  This is a point estimate for the parameter in which you are obtaining
  the distribution, e.g. sensitivity, specificity, or prevalence.

- w:

  `vector` A vector that provides the half widths of the 3 regions, (w1
  closest, w3 farthest).

- h:

  `vector` A vector of "y" (pseudo-value until scaled to be a
  probability) corresponding to the height of the shoulder and the
  height of the plateau.

- threestate:

  logical (TRUE/FALSE) Indicates whether or not there is a "suspect"
  region (i.e. positive/suspect/negative).

- suspect:

  A fraction that indicates what percentage of the remaining probability
  would be assigned to the suspect region. For instance, if the function
  gives sensitivity and then the probability of "suspect" is (1 -
  sensitivity) \* suspect.

- step_size:

  distance between the "x" in the discrete distribution, resolution of
  possible observations of the created distribution.

- p_proportion:

  whether to express "p" as a proportion of its sum.

## Value

`data.frame` of "x", "y", and "p".

## Author

[DiagTestKit-package](https://abs-dev.github.io/DiagTestKit/reference/DiagTestKit-package.md)
