# Cohort Counts + Attrition

results[["cohort_count_bb"]] <- cdm$bb_first |>
  summariseCohortCount()

results[["cohort_count_mis"]] <- cdm$acute_mi_first |>
  summariseCohortCount()

results[["cohort_code_use_bb"]] <- summariseCohortCodeUse(
  cohortTable = "bb_first",
    cdm = cdm,
    timing = "entry"
  )

results[["cohort_code_use_hf"]] <-summariseCohortCodeUse(
  cohortTable = "heart_failure",
  cdm = cdm,
  timing = "entry"
)

results[["cohort_code_use_mi"]] <- summariseCohortCodeUse(
  cohortTable = "acute_mi_first",
  cdm = cdm,
  timing = "entry"
)

results[["cohort_attrition_bb"]] <- cdm$bb_first |>
  summariseCohortAttrition()

results[["cohort_attrition_hf"]] <- cdm$heart_failure|>
  summariseCohortAttrition()

results[["cohort_attrition_mi"]] <- cdm$acute_mi_first |>
  summariseCohortAttrition()

## Comorbidity Codelists

comorbidities_cl <- CodelistGenerator::importCodelist(
  path = here::here("Cohorts", "comorbidities"),
  type = "csv"
)

cdm$comorbs <- conceptCohort(
  cdm = cdm,
  conceptSet = comorbidities_cl,
  name = "comorbs"
)

cdm <- omopgenerics::bind(
  cdm$comorbs,
  cdm$obesity,
  name = "comorbs"
)
  
drugs_cl <- CodelistGenerator::importCodelist(
  path = here::here("Cohorts", "table1_drugs"),
  type = "csv"
)

cdm$mi_drugs <- conceptCohort(
  cdm = cdm,
  conceptSet = drugs_cl,
  name = "mi_drugs"
)
# Cohort Characteristics - MI

cdm$bb_chars <- cdm$bb_first |>
  addDemographics(
    sex = TRUE,
    age = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "bb_chars"
  )

if(db_name == "GOLD" | db_name == "GOLD_100k"){
cdm$bb_chars <- cdm$bb_chars |>
  addCKDStage() |>
  addEthnicity() |>
  addSES() |>
  addCountry()

strata_list <- list(c("country"), c("age_group"), c("sex"), c("ses"),
               c("country", "age_group"),
               c("country", "sex"),
               c("country", "ses"))
} else {
  cdm$bb_chars <- cdm$bb_chars |>
    addCKDStage() |>
    addEthnicity() |>
    addSES()
  
  strata_list <- list(c("age_group"), c("sex"), c("ses"))
}

cdm$bb_chars <- cdm$bb_chars |>
  mutate(
    age_group = case_when(
      age >= 18 & age <= 39 ~ '18 to 39',
      age >= 40 & age <= 49 ~ '40 to 49',
      age >= 50 & age <= 59 ~ '50 to 59',
      age >= 60 & age <= 69 ~ '60 to 69',
      age >= 70 & age <= 79 ~ '70 to 79',
      age >= 80 & age <= 89 ~ '80 to 89',
      age >= 90 & age <= 150 ~ '90 to 150',
      TRUE ~ 'None'  
    )
  )

char_bb <- summariseCharacteristics(cdm$bb_chars,
                                    ageGroup = list(
                                      "18 to 39" = c(18, 39),
                                      "40 to 49" = c(40, 49),
                                      "50 to 59" = c(50, 59),
                                      "60 to 69" = c(60, 69),
                                      "70 to 79" = c(70, 79),
                                      "80 to 89" = c(80, 89),
                                      "90+" = c(90, 150)),
                                    cohortIntersectFlag = list(
                                      "Prevalent users (-28 to -1)" = list(
                                        targetCohortTable = "mi_drugs",
                                        window = list(
                                          c(-28, -1)
                                        )
                                      ),
                                      "Prior comorbidities (-Inf, -1)" = list(
                                        targetCohortTable = "comorbs",
                                        window = list(
                                          c(-Inf, -1)
                                        )
                                      ) 
                                      ),
                                    strata = strata_list,
                                    otherVariables = c("ses", "ethnicity"))


results[["summmarise_characteristics_bb"]] <- char_bb
