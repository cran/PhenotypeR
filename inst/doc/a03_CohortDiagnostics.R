## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", message = FALSE, warning = FALSE,
  fig.width = 7
)

library(CDMConnector)
if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
if (!eunomia_is_available()) downloadEunomiaData()

## -----------------------------------------------------------------------------
library(CDMConnector)
library(CohortConstructor)
library(CodelistGenerator)
library(PatientProfiles)
library(CohortCharacteristics)
library(PhenotypeR)
library(dplyr)
library(ggplot2)

con <- DBI::dbConnect(duckdb::duckdb(),
  dbdir = CDMConnector::eunomiaDir()
)
cdm <- CDMConnector::cdmFromCon(con,
  cdmSchema = "main",
  writeSchema = "main",
  cdmName = "Eunomia"
)

cdm$injuries <- conceptCohort(cdm = cdm,
  conceptSet = list(
    "ankle_sprain" = 81151,
    "ankle_fracture" = 4059173,
    "forearm_fracture" = 4278672,
    "hip_fracture" = 4230399
  ),
  name = "injuries")

## -----------------------------------------------------------------------------
cohort_diag <- cohortDiagnostics(cdm$injuries)

## -----------------------------------------------------------------------------
plotCohortOverlap(cohort_diag, uniqueCombinations = TRUE)

## -----------------------------------------------------------------------------
tableCharacteristics(cohort_diag, groupColumn = c("age_group", "sex"))

## -----------------------------------------------------------------------------
tableCharacteristics(cohort_diag, groupColumn = c("age_group", "sex"))

