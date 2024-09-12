#' Data set for Example 1 in vignette "DiagTestKit Examples"
#'
#' Samples randomly selected from a single population are tested by one 2-state
#' fallible reference test and a 2-state experimental test.
#'
#' @format A data frame with four rows and three variables:
#'
#' * `exp_result`: result of experimental test (positive, negative).
#'
#' * `ref1_result`: result of reference test (positive, negative).
#'
#' * `count`: number of samples with the unique testing combination.
#'
"data1"

#' Data set for Example 2 in vignette "DiagTestKit Examples"
#'
#' Samples randomly selected from 3 populations are tested by one 2-state
#' fallible reference test and a 2-state experimental test.
#'
#' @format A data frame with 12 rows and four variables:
#'
#' * `population`: population identifier (A, B, C).
#'
#' * `exp_result`: result of experimental test (positive, negative).
#'
#' * `ref_result`: result of reference test (positive, negative).
#'
#' * `count`: number of samples with the unique testing combination for the
#'   specific population.
#'
"data2"


#' Data set for Example 3 in vignette "DiagTestKit Examples"
#'
#' Samples randomly selected from single population tested by one 3-state
#' fallible reference test and a 2-state experimental test.
#'
#' @format A data frame with six rows and three variables:
#'
#' * `exp_result`: result of experimental test (positive, negative).
#'
#' * `ref_result`: result of reference test (positive, negative, suspect).
#'
#' * `count`: number of samples with the unique testing combination.
"data3"


#' Data set for Example 4 in vignette "DiagTestKit Examples"
#'
#' Samples randomly selected from 3 populations tested by one fallible 3-state
#' reference test and a 2-state experimental test.
#'
#' @format A data frame with 18 rows and four variables:
#'
#' * `population`: population identifier (A, B, C).
#'
#' * `exp_result`: result of experimental test (positive, negative).
#'
#' * `ref_result`: result of reference test (positive, negative, suspect).
#'
#' * `count`: number of samples with the unique testing combination for the
#'   specific population.
"data4"


#' Data set for Example 5 in vignette "DiagTestKit Examples"
#'
#' Samples randomly selected from 2 populations tested by one 2-state
#' reference test and a 3-state experimental test.
#'
#' @format A data frame with 11 rows and four variables:
#'
#' * `Population`: population identifier (A, B).
#'
#' * `exp_result`: result of experimental test
#'   (positive, negative, suspect).
#'
#' * `ref_result`: result of reference test (positive, negative).
#'
#' * `count`: number of samples with the unique testing combination for the
#'   specific population.
"data5"


#' Data set for Example 6 in vignette "DiagTestKit Examples"
#'
#' Samples randomly selected from a single population tested with two 2-state
#'  reference test and a 2-state experimental test.
#'
#' @format A data frame with 8 rows and four variables:
#'
#' * `exp_result`: result of experimental test (positive, negative).
#'
#' * `ref1_result`: result of first reference test (positive, negative).
#'
#' * `ref2_result`: result of second reference test (positive, negative).
#'
#' * `count`: number of samples with the unique testing combination.
"data6"

#' Data set for Example 7 in vignette "DiagTestKit Examples"
#'
#' Samples randomly selected from 2 populations tested with two 3-state
#'  reference test and a 2-state experimental test.
#'
#' @format A data frame with 39 rows and five variables:
#'
#' * `Population`: population identifier (A, B).
#'
#' * `exp_result`: result of experimental test (positive, negative).
#'
#' * `ref1_result`: result of first reference test
#'   (positive, negative, suspect).
#'
#' * `ref2_result`: result of second reference test
#'    (positive, negative, suspect).
#'
#' * `count`: number of samples with the unique testing combination for the
#'   specific population.
"data7"

#' Data set for Example 8 in vignette "DiagTestKit Examples"
#'
#' Samples randomly selected from 3 populations tested by three 2-state
#'  reference tests and a 3-state experimental test.
#'
#' @format A data frame with 72 rows and six variables:
#'
#' * `Population`: population identifier (A, B, C).
#'
#' * `exp_result`: result of experimental test
#'    (positive, negative, suspect).
#'
#' * `ref1_result`: result of first reference test
#'    (positive, negative).
#'
#' * `ref2_result`: result of second reference test
#'   (positive, negative).
#'
#' * `ref3_result`: result of third reference test
#'    (positive, negative).
#'
#' * `count`: number of samples with the unique testing combination for the
#'   specific population.
"data8"


#' Assay Validation Sensitivity and Specificity (Diagnostic Kit Format)
#' example from CVB Data Guide.
#'
#' Results from testing by diagnostic test kits with a dichotomous response for
#' assay validation. See CVB Data Guide Appendix 1.8.
#' \href{https://www.aphis.usda.gov/animal_health/vet_biologics/publications/DiagnosticKitDichotomous.zip}{DiagnosticKitDichotomous.zip/Diagnostic Kit Dichotomous SenSpec Example/dichotomoussenspec_deviceinfo.csv}
#'
#' @format A data frame with 202 rows and 14 variables:
#'
#' * `deviceID`: Device identifier; unique for each row.
#'
#' * `serialID`: Identifier of the preparation used.
#'
#' * `tech`: Identifier of the technician performing testing.
#'
#' * `sampleID`: Unique identifier for the sample being tested.
#'
#' * `animalID`: Unique identifier for an animal.
#'
#' * `specimen`: Type of specimen (wholeblood, serum, plasma).
#'
#' * `species`: Animal species.
#'
#' * `mfg_date`: Date of preparation manufacturer.
#'
#' * `date`: Test date.
#'
#' * `visual_read`: Test interpretation by visual reading.
#'
#' * `instr_read`: Test interpretation by instrument reading.
#'
#' * `control_read`: Test interpretation of the control.
#'
#' * `ref_result`: Test interpretation of the reference.
#'
#' * `prod_code`: Veterinary Services Product Code
#' @source \url{https://www.aphis.usda.gov/animal_health/vet_biologics/publications/DiagnosticKitDichotomous.zip}
"dat_dichot"

#' Counts data used in vignette "DiagTestKit GettingStarted" section 4
#'
#' Counts of the positive and negative results for experimental and reference
#' tests. A 2-state experimental test when an infallible reference test has been
#' used to determine the true disease status of each sample.
#'
#' @format A data frame with 4 rows and 3 variables:
#'
#' * `Experimental`: Result of the experimental test
#'    (positive or negative).
#'
#' * `Reference`: Result of the reference test
#'    (positive or negative).
#'
#' * `Count`: Number of samples observed with the unique
#'    testing combination.
"dat_infal"
