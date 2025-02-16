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

## ----message=FALSE, warning=FALSE---------------------------------------------
library(CDMConnector)
library(CohortConstructor)
library(CodelistGenerator)
library(PatientProfiles)
library(IncidencePrevalence)
library(PhenotypeR)


con <- DBI::dbConnect(duckdb::duckdb(), 
                      CDMConnector::eunomiaDir("synpuf-1k", "5.3"))
cdm <- CDMConnector::cdmFromCon(con = con, 
                                cdmName = "Eunomia Synpuf",
                                cdmSchema   = "main",
                                writeSchema = "main", 
                                achillesSchema = "main")

cdm$injuries <- conceptCohort(cdm = cdm,
  conceptSet = list(
    "ankle_sprain" = 81151
  ),
  name = "injuries")

## -----------------------------------------------------------------------------
pop_diag <- populationDiagnostics(cdm$injuries)

## -----------------------------------------------------------------------------
tableIncidence(pop_diag,     
               groupColumn = c("cdm_name", "outcome_cohort_name"),
               hide = "denominator_cohort_name",
               settingsColumn = c("denominator_age_group",
                         "denominator_sex",
                         "denominator_days_prior_observation",
                         "outcome_cohort_name"))

## -----------------------------------------------------------------------------
tablePrevalence(pop_diag,     
               groupColumn = c("cdm_name", "outcome_cohort_name"),
               hide = "denominator_cohort_name",
               settingsColumn = c("denominator_age_group",
                         "denominator_sex",
                         "denominator_days_prior_observation",
                         "outcome_cohort_name"))

