# Cohort Counts + Attrition

results[["cohort_count_mi_drugs"]] <- cdm$mi_drugs_final |>
  summariseCohortCount()

results[["cohort_count_stroke_drugs"]] <- cdm$stroke_drugs_final |>
  summariseCohortCount()

results[["cohort_count_mis"]] <- cdm$acute_mi_crm |>
  summariseCohortCount()

results[["cohort_count_stroke"]] <- cdm$ischemic_stroke_crm |>
  summariseCohortCount()

results[["cohort_code_use_mi_drugs"]] <- summariseCohortCodeUse(
  cohortTable = "mi_drugs_final",
    cdm = cdm,
    timing = "entry"
  )

results[["cohort_code_use_stroke_drugs"]] <-summariseCohortCodeUse(
  cohortTable = "stroke_drugs_final",
  cdm = cdm,
  timing = "entry"
)

results[["cohort_code_use_mi"]] <- summariseCohortCodeUse(
  cohortTable = "acute_mi_crm",
  cdm = cdm,
  timing = "entry"
)

results[["cohort_code_use_stroke"]] <-summariseCohortCodeUse(
  cohortTable = "ischemic_stroke_crm",
  cdm = cdm,
  timing = "entry"
)

results[["cohort_attrition_mi_drugs"]] <- cdm$mi_drugs_final |>
  summariseCohortAttrition()

results[["cohort_attrition_stroke_drugs"]] <- cdm$stroke_drugs_final |>
  summariseCohortAttrition()

results[["cohort_attrition_mi"]] <- cdm$acute_mi_crm |>
  summariseCohortAttrition()

results[["cohort_attrition_stroke"]] <- cdm$ischemic_stroke_crm |>
  summariseCohortAttrition()

## Comorbidity Codelists

comorbidities_cl <- CodelistGenerator::importCodelist(
  path = here::here("Cohorts", "comorbidities"),
  type = "csv"
)
  
# Cohort Characteristics - MI

cdm <- omopgenerics::bind(
  cdm$acute_mi_crm,
  cdm$mi_drugs_first,
  name = "mi_chars"
)

cdm$mi_chars <- cdm$mi_chars |>
  addDemographics(
    sex = TRUE,
    age = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "mi_chars"
  )

if(db_name == "GOLD" | db_name == "GOLD_100k"){
cdm$mi_chars <- cdm$mi_chars |>
  addCKDStage() |>
  addEthnicity() |>
  addSES() |>
  addCountry()

strata_list <- list(c("country"), c("age_group"), c("sex"), c("ses"),
               c("country", "age_group"),
               c("country", "sex"),
               c("country", "ses"))
} else {
  cdm$mi_chars <- cdm$mi_chars |>
    addCKDStage() |>
    addEthnicity() |>
    addSES()
  
  strata_list <- list(c("age_group"), c("sex"), c("ses"))
}

cdm$mi_chars <- cdm$mi_chars |>
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

char_mi <- summariseCharacteristics(cdm$mi_chars,
                                    ageGroup = list(
                                      "18 to 39" = c(18, 39),
                                      "40 to 49" = c(40, 49),
                                      "50 to 59" = c(50, 59),
                                      "60 to 69" = c(60, 69),
                                      "70 to 79" = c(70, 79),
                                      "80 to 89" = c(80, 89),
                                      "90+" = c(90, 150)),
                                    cohortIntersectFlag = list(
                                      "Prevalent drug use (-28 to -1)" = list(
                                        targetCohortTable = "mi_drugs",
                                        window = list(
                                          c(-28, -1)
                                        )
                                      ),
                                      "Prior ischemic stroke (-Inf to -1)" = list(
                                        targetCohortTable = "stroke",
                                        window = list(
                                          c(-Inf, -1)
                                        )
                                      ),
                                      "Prior comorbidities (-Inf, -1)" = list(
                                        targetCohortTable = "obesity",
                                        window = list(
                                          c(-Inf, -1)
                                        )
                                      ) 
                                      ),
                                    conceptIntersectFlag = list(
                                      "Prior comorbidities (-Inf, -1)" = list(
                                        conceptSet = comorbidities_cl,
                                        window = list(
                                          c(-Inf, -1)
                                        )
                                      )
                                      ),
                                    strata = strata_list,
                                    otherVariables = c("ses", "ethnicity", "ckd_stage"))


results[["summmarise_characteristics_mi"]] <- char_mi


# Cohort Characteristics - Stroke

cdm <- omopgenerics::bind(
  cdm$ischemic_stroke_crm,
  cdm$stroke_drugs_first,
  name = "stroke_chars"
)
cdm$stroke_chars <- cdm$stroke_chars |>
  addDemographics(
    sex = TRUE,
    age = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "stroke_chars"
  )

if(db_name == "GOLD" | db_name == "GOLD_100k"){
cdm$stroke_chars <- cdm$stroke_chars |>
  addCKDStage() |>
  addEthnicity() |>
  addSES() |>
  addCountry()

} else {
  cdm$stroke_chars <- cdm$stroke_chars |>
    addCKDStage() |>
    addEthnicity() |>
    addSES()
  
}
cdm$stroke_chars <- cdm$stroke_chars |>
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

char_stroke <- summariseCharacteristics(cdm$stroke_chars,
                                        ageGroup = list(
                                          "18 to 39" = c(18, 39),
                                          "40 to 49" = c(40, 49),
                                          "50 to 59" = c(50, 59),
                                          "60 to 69" = c(60, 69),
                                          "70 to 79" = c(70, 79),
                                          "80 to 89" = c(80, 89),
                                          "90+" = c(90, 150)),
                                        cohortIntersectFlag = list(
                                          "Prevalent drug use (-28 to -1)" = list(
                                            targetCohortTable = "stroke_drugs",
                                            window = list(
                                              c(-28, -1)
                                            )
                                          ),
                                          "Prior myocardial infarction (-Inf to -1)" = list(
                                            targetCohortTable = "acute_mi",
                                            window = list(
                                              c(-Inf, -1)
                                            )
                                          ),
                                          "Prior comorbidities (-Inf, -1)" = list(
                                            targetCohortTable = "obesity",
                                            window = list(
                                              c(-Inf, -1)
                                            )
                                          ) 
                                        ),
                                        conceptIntersectFlag = list(
                                          "Prior comorbidities (-Inf, -1)" = list(
                                            conceptSet = comorbidities_cl,
                                            window = list(
                                              c(-Inf, -1)
                                            )
                                          )
                                        ),
                                        strata = strata_list,
                                        otherVariables = c("ses", "ethnicity", "ckd_stage"))

results[["summmarise_characteristics_stroke"]] <- char_stroke