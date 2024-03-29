% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{dat_dichot}
\alias{dat_dichot}
\title{Assay Validation Sensitivity and Specificity (Diagnostic Kit Format)
example from CVB Data Guide.}
\format{
A data frame with 202 rows and 14 variables:
\describe{
\item{deviceID}{Device identifier; unique for each row.}
\item{serialID}{Identifier of the preparation used.}
\item{tech}{Identifier of the technician performing testing.}
\item{sampleID}{Unique identifier for the sample being tested.}
\item{animalID}{Unique identifier for an animal.}
\item{specimen}{Type of specimen (wholeblood, serum, plasma).}
\item{species}{Animal species.}
\item{mfg_date}{Date of preparation manufacturer.}
\item{date}{Test date.}
\item{visual_read}{Test interpretation by visual reading.}
\item{instr_read}{Test interpretation by instrument reading.}
\item{control_read}{Test interpretation of the control.}
\item{ref_result}{Test interpretation of the reference.}
\item{prod_code}{Veterinary Services Product Code}
}
}
\source{
\url{https://www.aphis.usda.gov/animal_health/vet_biologics/publications/DiagnosticKitDichotomous.zip}
}
\usage{
dat_dichot
}
\description{
Results from testing by diagnostic test kits with a dichotomous response for
assay validation. See CVB Data Guide Appendix 1.8.
\href{https://www.aphis.usda.gov/animal_health/vet_biologics/publications/DiagnosticKitDichotomous.zip}{DiagnosticKitDichotomous.zip/Diagnostic Kit Dichotomous SenSpec Example/dichotomoussenspec_deviceinfo.csv}
}
\keyword{datasets}
