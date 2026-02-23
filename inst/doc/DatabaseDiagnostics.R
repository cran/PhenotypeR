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
#   comment = "#>", message = FALSE, warning = FALSE,
#   fig.width = 7
# )
# 
# library(CDMConnector)
# if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
# if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
# if (!eunomiaIsAvailable()) downloadEunomiaData(datasetName = "synpuf-1k")

## -----------------------------------------------------------------------------
# library(CDMConnector)
# library(OmopSketch)
# library(PhenotypeR)
# library(dplyr)
# library(CohortConstructor)
# 
# con <- DBI::dbConnect(duckdb::duckdb(),
#                       CDMConnector::eunomiaDir("synpuf-1k", "5.3"))
# cdm <- CDMConnector::cdmFromCon(con = con,
#                                 cdmName = "Eunomia Synpuf",
#                                 cdmSchema   = "main",
#                                 writeSchema = "main",
#                                 achillesSchema = "main")
# 
# cdm$injuries <- conceptCohort(cdm = cdm,
#   conceptSet = list(
#     "ankle_sprain" = 81151L,
#     "ankle_fracture" = 4059173L,
#     "forearm_fracture" = 4278672L,
#     "hip_fracture" = 4230399L
#   ),
#   name = "injuries")

## -----------------------------------------------------------------------------
# db_diagnostics <- databaseDiagnostics(cdm$injuries)

## -----------------------------------------------------------------------------
# tableOmopSnapshot(db_diagnostics)

## -----------------------------------------------------------------------------
# tableObservationPeriod(db_diagnostics)

## -----------------------------------------------------------------------------
# tablePerson(db_diagnostics)

## -----------------------------------------------------------------------------
# tableClinicalRecords(db_diagnostics)

