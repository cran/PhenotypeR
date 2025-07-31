## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", message = FALSE, warning = FALSE,
  fig.width = 7
)

library(CDMConnector)
if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
if (!eunomiaIsAvailable()) downloadEunomiaData(datasetName = "synpuf-1k")

## -----------------------------------------------------------------------------
library(CDMConnector)
library(CohortConstructor)
library(CodelistGenerator)
library(PhenotypeR)
library(MeasurementDiagnostics)
library(dplyr)
library(ggplot2)

con <- DBI::dbConnect(duckdb::duckdb(), 
                      CDMConnector::eunomiaDir("synpuf-1k", "5.3"))
cdm <- CDMConnector::cdmFromCon(con = con, 
                                cdmName = "Eunomia Synpuf",
                                cdmSchema   = "main",
                                writeSchema = "main", 
                                achillesSchema = "main")

cdm$injuries <- conceptCohort(cdm = cdm,
  conceptSet = list(
    "ankle_sprain" = 81151,
    "ankle_fracture" = 4059173,
    "forearm_fracture" = 4278672,
    "hip_fracture" = 4230399,
    "measurements_cohort" = c(40660437L, 2617206L, 4034850L,  2617239L, 4098179L)
  ),
  name = "injuries")
cdm$injuries |> 
  glimpse()

## -----------------------------------------------------------------------------
code_diag <- codelistDiagnostics(cdm$injuries)

## -----------------------------------------------------------------------------
cohortCodelist(cdm$injuries, cohortId = 1)
cdm$injuries <- cdm$injuries |>
  addCodelistAttribute(codelist = list(new_codelist = c(1L, 2L)), cohortName = "ankle_fracture")
cohortCodelist(cdm$injuries, cohortId = 1)

## -----------------------------------------------------------------------------
tableAchillesCodeUse(code_diag)

## -----------------------------------------------------------------------------
tableOrphanCodes(code_diag)

## -----------------------------------------------------------------------------
tableCohortCodeUse(code_diag)

## -----------------------------------------------------------------------------
tableMeasurementTimings(code_diag)

## -----------------------------------------------------------------------------
plotMeasurementTimings(code_diag)

## -----------------------------------------------------------------------------
tableMeasurementValueAsConcept(code_diag)

## -----------------------------------------------------------------------------
plotMeasurementValueAsConcept(code_diag)

## -----------------------------------------------------------------------------
tableMeasurementValueAsNumeric(code_diag)

## -----------------------------------------------------------------------------
plotMeasurementValueAsNumeric(code_diag)

