# DiagTestKit


A package written by CVB Statistics to estimate the sensitivity and specificity of an experimental diagnostic
test kit in accordance with [CVB STATWI0002](https://www.aphis.usda.gov/aphis/ourfocus/animalhealth/veterinary-biologics/biologics-regulations-and-guidance/ct_vb_statwi) supporting the 2018 revision to VSM 800.73.


### To install or update DiagTestKit:

From **within R**

1. Installing current release [v0.5.3](https://github.com/ABS-dev/DiagTestKit/releases/tag/0.5.3)

```
## From source, all platforms, slow.
devtools::install_github("ABS-dev/DiagTestKit", ref = "0.5.3")
```

  *See all historical releases [here](https://github.com/ABS-dev/DiagTestKit/releases)*

2. Installing work-in-progress towards v0.5.4

Travis: [![Build Status](https://travis-ci.org/ABS-dev/DiagTestKit.svg?branch=master)](https://travis-ci.org/ABS-dev/DiagTestKit)

```
require(devtools)
install_github("ABS-dev/DiagTestKit")
```

### Package Vignettes:

[Getting Started](https://github.com/ABS-dev/DiagTestKit/blob/master/inst/doc/GettingStarted.pdf)

[Examples](https://github.com/ABS-dev/DiagTestKit/blob/master/inst/doc/ExamplesForFallibleReferenceTests.pdf)

[Manual](https://github.com/ABS-dev/DiagTestKit/blob/master/inst/doc/DiagTestKit-manual.pdf)

From **within R**

```
vignette('GettingStarted', package = 'DiagTestKit')
vignette('ExamplesForFallibleReferenceTests', package = 'DiagTestKit')

```
