## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# library(PhenotypeR)
# library(here)
# downloadDatabaseDescriptionTemplate(directory = here(),
#                                      name = "GiBleed")

## ----eval = FALSE-------------------------------------------------------------
# library(PhenotypeR)
# library(here)
# downloadClinicalDescriptionTemplate(directory = here(),
#                                     name = "acetaminophen")

## ----eval=FALSE---------------------------------------------------------------
# usethis::edit_r_environ()
# 
# # Add your API in your R environment:
# GEMINI_API_KEY = "your API"
# 
# # Restrart R

## ----eval=FALSE---------------------------------------------------------------
# library(ellmer)
# chat <- ellmer::chat("mistral")
# getClinicalDescription(chat,
#                        name = "acetaminophen_users",
#                        outputDir = here())

