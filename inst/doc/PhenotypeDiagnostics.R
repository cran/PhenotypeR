## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(CDMConnector)
if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
if (!eunomiaIsAvailable()) downloadEunomiaData(datasetName = "synpuf-1k", cdmVersion = "5.3")

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
# library(CohortConstructor)
# library(PhenotypeR)
# library(dplyr)
# 
# con <- DBI::dbConnect(duckdb::duckdb(),
#                       CDMConnector::eunomiaDir("synpuf-1k", "5.3"))
# cdm <- CDMConnector::cdmFromCon(con = con,
#                                 cdmName = "Eunomia Synpuf",
#                                 cdmSchema   = "main",
#                                 writeSchema = "main",
#                                 achillesSchema = "main")
# cdm

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
# # Create a codelist
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

## ----eval=FALSE---------------------------------------------------------------
# result <- phenotypeDiagnostics(
#   cohort = cdm$my_cohort,
#   diagnostics = c("databaseDiagnostics", "codelistDiagnostics",
#                   "cohortDiagnostics", "populationDiagnostics"),
#   matchedSample = 1000
#   populationSample = 1e+06,
#   populationDateRange = as.Date(c(NA, NA))
#   )
# result |> glimpse()

## ----eval=FALSE---------------------------------------------------------------
# exportSummarisedResult(result, directory = here::here(), minCellCount = 5)

## ----eval=FALSE---------------------------------------------------------------
# result <- shinyDiagnostics(result,
#                            directory = tempdir(),
#                            minCellCount = 5,
#                            open = TRUE)

