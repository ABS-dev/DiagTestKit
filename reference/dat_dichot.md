# Assay Validation Sensitivity and Specificity (Diagnostic Kit Format) example from CVB Data Guide.

Results from testing by diagnostic test kits with a dichotomous response
for assay validation. See CVB Data Guide Appendix 1.8.
[DiagnosticKitDichotomous.zip/Diagnostic Kit Dichotomous SenSpec
Example/dichotomoussenspec_deviceinfo.csv](https://www.aphis.usda.gov/animal_health/vet_biologics/publications/DiagnosticKitDichotomous.zip)

## Usage

``` r
dat_dichot
```

## Format

A data frame with 202 rows and 14 variables:

- `deviceID`: Device identifier; unique for each row.

- `serialID`: Identifier of the preparation used.

- `tech`: Identifier of the technician performing testing.

- `sampleID`: Unique identifier for the sample being tested.

- `animalID`: Unique identifier for an animal.

- `specimen`: Type of specimen (wholeblood, serum, plasma).

- `species`: Animal species.

- `mfg_date`: Date of preparation manufacturer.

- `date`: Test date.

- `visual_read`: Test interpretation by visual reading.

- `instr_read`: Test interpretation by instrument reading.

- `control_read`: Test interpretation of the control.

- `ref_result`: Test interpretation of the reference.

- `prod_code`: Veterinary Services Product Code

## Source

<https://www.aphis.usda.gov/animal_health/vet_biologics/publications/DiagnosticKitDichotomous.zip>
