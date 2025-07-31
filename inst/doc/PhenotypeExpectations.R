## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  warning=FALSE, 
  message=FALSE, 
  eval = TRUE
)

## -----------------------------------------------------------------------------
library(dplyr)
library(PhenotypeR)

knee_oa <- tibble(cohort_name = "knee_osteoarthritis",
                  estimate = c("Median age", "Proportion male"),
                  value = c("60 to 65", "45%"),
                  source = "Clinician")
knee_replacement <- tibble(cohort_name = "knee_replacement",
                           estimate = c("Median age", "Proportion male"),
                           value = c("65 to 70", "50%"),
                           source = "Clinician")

expectations <- bind_rows(knee_oa, knee_replacement)

## ----warning=FALSE, message=FALSE---------------------------------------------
tableCohortExpectations(expectations)

## -----------------------------------------------------------------------------
tibble(cohort_name = "knee_osteoarthritis",
                  estimate = c("Commonly seen subsequent procedures"),
                  value = c("Knee replacement"),
                  source = "Expert opinion") |> 
  tableCohortExpectations()

## ----eval=FALSE---------------------------------------------------------------
# usethis::edit_r_environ()
# 
# # Add your API in your R environment:
# GEMINI_API_KEY = "your API"
# 
# # Restrart R

## ----eval=FALSE---------------------------------------------------------------
# library(ellmer)
# 
# chat <- chat("google_gemini")
# llm_expectation <- chat$chat(
#     interpolate("What are the typical characteristics we can expect to see in our real-world data for a cohort of people with an ankle sprain (average age, proportion male vs female, subsequent medications, etc)? Be brief and provide summar with a few sentences."))
# 
# tibble(cohort_name = "diagnosis_of_ankle_sprain",
#        estimate = "General summary",
#        value = llm_expectation,
#        source = "llm") |>
#   tableCohortExpectations()

## ----echo=FALSE---------------------------------------------------------------
readr::read_csv("vignette_phenotype_expectations/expectations_1.csv") |>
    tableCohortExpectations()

## ----eval=FALSE---------------------------------------------------------------
# getCohortExpectations(chat = chat,
#                       phenotypes = c("diagnosis_of_ankle_sprain",
#                                      "diagnosis_of_prostate_cancer",
#                                      "new_user_of_morphine")) |>
#   tableCohortExpectations()

## ----echo=FALSE---------------------------------------------------------------
readr::read_csv("vignette_phenotype_expectations/expectations_2.csv") |>
    tableCohortExpectations()

## ----eval=FALSE---------------------------------------------------------------
# library(DBI)
# library(duckdb)
# library(CDMConnector)
# library(CohortConstructor)
# 
# con <- dbConnect(duckdb(), dbdir = eunomiaDir())
# cdm <- cdmFromCon(
#     con = con, cdmSchema = "main", writeSchema = "main", cdmName = "Eunomia"
#   )
# 
# codes <- list("diagnosis_of_ankle_sprain" = 81151,
#               "diagnosis_of_prostate_cancer" = 4163261,
#               "new_user_of_morphine" = c(1110410L, 35605858L, 40169988L))
# 
# cdm$my_cohort <- conceptCohort(cdm = cdm,
#                                  conceptSet = codes,
#                                  exit = "event_end_date",
#                                  name = "my_cohort")
# 
# diag_results <- phenotypeDiagnostics(cdm$my_cohort)
# 
# getCohortExpectations(chat = chat,
#                       phenotypes = diag_results) |>
#   tableCohortExpectations()

## ----echo=FALSE---------------------------------------------------------------
readr::read_csv("vignette_phenotype_expectations/expectations_2.csv") |>
    tableCohortExpectations()

## ----eval=FALSE---------------------------------------------------------------
# chat <- ellmer::chat("mistral")
# diag_results <- phenotypeDiagnostics(cdm$my_cohort)
# getCohortExpectations(chat = chat,
#                       phenotypes = diag_results) |>
#   tableCohortExpectations()

## ----echo=FALSE---------------------------------------------------------------
readr::read_csv("vignette_phenotype_expectations/expectations_3.csv") |>
    tableCohortExpectations()

