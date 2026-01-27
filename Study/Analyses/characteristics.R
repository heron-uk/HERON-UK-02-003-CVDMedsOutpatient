# Cohort Counts + Attrition

results[["cohort_count_7"]] <- cdm$cardio_drugs_7 |>
  summariseCohortCount()

results[["cohort_attrition_7"]] <- cdm$cardio_drugs_7 |>
  summariseCohortAttrition()

# Cohort Characteristics

cdm$cardio_drugs_7_chars <- cdm$cardio_drugs_7 |>
  PatientProfiles::addDemographics(
    sex = TRUE,
    age = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "cardio_drugs_7_chars"
  )

cdm$cardio_drugs_7_chars <- cdm$cardio_drugs_7_chars |>
  mutate(
    age_group_broad = case_when(
      age >= 18 & age <= 64 ~ '18 to 64',
      age >= 65 & age <= 150 ~ '65 to 150',
      TRUE ~ 'None'  
    )
  )
#cond_cl <- CodelistGenerator::codesFromConceptSet(here("Cohorts", "conditions_cohorts"), cdm)

char <- summariseCharacteristics(cdm$cardio_drugs_7_chars,
                                 # conceptIntersectFlag = list(
                                 #   "Conditions" = list(
                                 #     conceptSet = cond_cl,
                                 #     window = list(
                                 #       c(-Inf, -1)
                                 #   )
                                 # )),
                                 strata = list("age_group_broad", "sex",
                                               c("age_group_broad", "sex")))


results[["summmarise_characteristics"]] <- char

### Large Scale Characteristics

# lsc <- summariseLargeScaleCharacteristics(
#   cohort = cdm$cardiovascular_drugs,
#   eventInWindow = c("condition_occurrence",
#                     "observation",
#                     "measurement",
#                     "procedure_occurrence",
#                     "drug_exposure")
# )
# 
# # tableTopLargeScaleCharacteristics(lsc |> filter(group_level == "aspirin_1112807"))
# 
# results[["summarise_lsc"]] <- lsc
