## ----include = FALSE----------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")

knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
eval = NOT_CRAN
)

## ----include = FALSE----------------------------------------------------------
# knitr::opts_chunk$set(
#   collapse = TRUE,
#   comment = "#>"
# )
# 
# library(CDMConnector)
# if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
# if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
# if (!eunomiaIsAvailable()) downloadEunomiaData(datasetName = "synpuf-1k", cdmVersion = "5.3")

## ----message=FALSE, warning=FALSE---------------------------------------------
# library(CohortConstructor)
# library(OmopSketch)
# library(PhenotypeR)
# library(dplyr)
# library(DBI)
# library(duckdb)
# library(CDMConnector)
# 
# con <- dbConnect(duckdb(),
#                  eunomiaDir("synpuf-1k", "5.3"))
# 
# cdm <- cdmFromCon(con = con,
#                   cdmName = "Eunomia Synpuf",
#                   cdmSchema   = "main",
#                   writeSchema = "main",
#                   achillesSchema = "main")
# cdm

## ----message=FALSE, warning=FALSE---------------------------------------------
# # Create codelists
# codes <- list("warfarin" = c(1310149, 40163554),
#               "acetaminophen" = c(1125315, 1127078, 1127433, 40229134, 40231925, 40162522, 19133768),
#               "morphine" = c(1110410, 35605858, 40169988))
# 
# # Instantiate cohorts with CohortConstructor
# cdm$my_cohort <- conceptCohort(cdm = cdm,
#                                conceptSet = codes,
#                                exit = "event_end_date",
#                                overlap = "merge",
#                                name = "my_cohort")

## ----warning = FALSE, message = FALSE-----------------------------------------
# diagnostics <- phenotypeDiagnostics(cdm$my_cohort,
#                                 databaseDiagnostics = list(),
#                                 codelistDiagnostics = list(),
#                                 cohortDiagnostics = list(),
#                                 populationDiagnostics = list(),
#                                 stagingDirectory = NULL)

## ----eval = FALSE-------------------------------------------------------------
# phenotypeDiagnostics(cdm$my_cohort,
#                      databaseDiagnostics = list(),
#                      codelistDiagnostics = NULL,
#                      cohortDiagnostics = list(),
#                      populationDiagnostics = NULL)

## ----eval = FALSE-------------------------------------------------------------
# diagnostics <- phenotypeDiagnostics(cdm$my_cohort,
#                                 databaseDiagnostics = list(),
#                                 codelistDiagnostics = list(),
#                                 cohortDiagnostics = list("cohortSurvival" = TRUE),
#                                 populationDiagnostics = list())

## -----------------------------------------------------------------------------
# options("PhenotypeR_summariseLargeScaleCharacteristics_window" = list(c(0, 0)))

## -----------------------------------------------------------------------------
# options("PhenotypeR_summariseLargeScaleCharacteristics_eventInWindow"   = c("condition_occurrence"))
# options("PhenotypeR_summariseLargeScaleCharacteristics_episodeInWindow" = c("drug_exposure"))

## ----eval=FALSE---------------------------------------------------------------
# exportSummarisedResult(diagnostics, path = here::here(), minCellCount = 5)

## ----eval=FALSE---------------------------------------------------------------
# shinyDiagnostics(diagnostics,
#                  directory = tempdir(),
#                  minCellCount = 5,
#                  open = TRUE)

