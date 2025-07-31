## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

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
# 
# # Create a code lists
# codes <- list("user_of_warfarin" = c(1310149L, 40163554L),
#               "user_of_acetaminophen" = c(1125315L, 1127078L, 1127433L, 40229134L,
#                                           40231925L, 40162522L, 19133768L),
#               "user_of_morphine" = c(1110410L, 35605858L, 40169988L),
#               "measurements_cohort" = c(40660437L, 2617206L, 4034850L,  2617239L,
#                                         4098179L))
# 
# # Instantiate cohorts with CohortConstructor
# cdm$my_cohort <- conceptCohort(cdm = cdm,
#                                conceptSet = codes,
#                                exit = "event_end_date",
#                                overlap = "merge",
#                                name = "my_cohort")
# 
# # Run PhenotypeDiagnostics including all diagnostics
# result <- phenotypeDiagnostics(cdm$my_cohort, survival = TRUE)
# 
# # Generate expectations
# chat <- chat("google_gemini")
# 
# expectations <- getCohortExpectations(chat = chat,
#                       phenotypes = result)
# 
# # Create the shiny app based on PhenotypeDiagnostics results, suppressing all
# # cell counts smaller than 2, saved in a temporary directory, and with the
# # expectations created using "gemini".
# shinyDiagnostics(result = result, minCellCount = 2, directory = tempdir(), expectations = expectations)

