# Cohort Counts + Attrition

mi_results[["cohort_count_mi"]] <- cdm$mi_drugs_final |>
  summariseCohortCount()

stroke_results[["cohort_count_stroke"]] <- cdm$stroke_drugs_final |>
  summariseCohortCount()

mi_results[["cohort_attrition_mi"]] <- cdm$mi_drugs_final |>
  summariseCohortAttrition()

stroke_results[["cohort_attrition_stroke"]] <- cdm$stroke_drugs_final |>
  summariseCohortAttrition()

## Comorbidity Codelists

comorbidities_cl <- CodelistGenerator::importCodelist(
  path = here::here("Cohorts", "comorbidities"),
  type = "csv"
)

cdm$ckd_latest <- cdm$ckd_stage |>
  requireIsLastEntry(
    name = "ckd_latest"
  )

cdm$ckd_char <- cdm$ckd_latest|>
  group_by(subject_id) |>
  filter(cohort_start_date == max(cohort_start_date),
         cohort_end_date == max(cohort_end_date)) |>
  ungroup() |>
  compute(name = "ckd_char", temporary = FALSE)
  
# Cohort Characteristics - MI

cdm$mi_drugs_chars <- cdm$mi_drugs_first |>
  PatientProfiles::addDemographics(
    sex = TRUE,
    age = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "mi_drugs_chars"
  ) |>
  addSES() |>
  addEthnicity()

cdm$mi_drugs_chars <- cdm$mi_drugs_chars |>
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

char_mi <- summariseCharacteristics(cdm$mi_drugs_chars,
                                    ageGroup = list(
                                      "18 to 39" = c(18, 39),
                                      "40 to 49" = c(40, 49),
                                      "50 to 59" = c(50, 59),
                                      "60 to 69" = c(60, 69),
                                      "70 to 79" = c(70, 79),
                                      "80 to 89" = c(80, 89),
                                      "90+" = c(90, 150)),
                                    conceptIntersectFlag = list(
                                      "Prior drug use (-30 to -1)" = list(
                                        conceptSet = mi_drugs_cl,
                                        window = list(
                                          c(-30, -1)
                                        )
                                      ),
                                      "Prior ischemic stroke (-30 to -1)" = list(
                                        conceptSet = stroke_cl,
                                        window = list(
                                          c(-30, -1)
                                        )
                                      ),
                                      "Prior comorbidities (-Inf, -1)" = list(
                                        conceptSet = comorbidities_cl,
                                        window = list(
                                          c(-Inf, -1)
                                        )
                                      )
                                      ),
                                    cohortIntersectFlag = list(
                                      "Prior comorbidities (-Inf, -1)" = list(
                                        targetCohortTable = "obesity",
                                        window = list(
                                          c(-Inf, -1)
                                        )
                                      ),
                                      "Prior comorbidities (-Inf, -1)" = list(
                                        targetCohortTable = "ckd_char",
                                        window = list(
                                          c(-Inf, -1)
                                        )
                                      ) 
                                    ),
                                    strata = list(c("age_group"), c("sex"), c("ses")
                                    ),
                                    otherVariables = c("ses", "ethnicity"))


mi_results[["summmarise_characteristics_mi"]] <- char_mi


# Cohort Characteristics - Stroke
cdm$stroke_drugs_chars <- cdm$stroke_drugs_first |>
  PatientProfiles::addDemographics(
    sex = TRUE,
    age = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "stroke_drugs_chars"
  ) |>
  addSES() |>
  addEthnicity()

cdm$stroke_drugs_chars <- cdm$stroke_drugs_chars |>
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

char_stroke <- summariseCharacteristics(cdm$stroke_drugs_chars,
                                        ageGroup = list(
                                          "18 to 39" = c(18, 39),
                                          "40 to 49" = c(40, 49),
                                          "50 to 59" = c(50, 59),
                                          "60 to 69" = c(60, 69),
                                          "70 to 79" = c(70, 79),
                                          "80 to 89" = c(80, 89),
                                          "90+" = c(90, 150)),
                                        conceptIntersectFlag = list(
                                          "Prior drug use (-30 to -1)" = list(
                                            conceptSet = stroke_drugs_cl,
                                            window = list(
                                              c(-30, -1)
                                            )
                                          ),
                                          "Prior MI (-30 to -1)" = list(
                                            conceptSet = acute_mi_cl,
                                            window = list(
                                              c(-30, -1)
                                            )
                                          ),
                                          "Prior comorbidities (-Inf, -1)" = list(
                                            conceptSet = comorbidities_cl,
                                            window = list(
                                              c(-Inf, -1)
                                            )
                                          )
                                        ),
                                        cohortIntersectFlag = list(
                                          "Prior comorbidities (-Inf, -1)" = list(
                                            targetCohortTable = "obesity",
                                            window = list(
                                              c(-Inf, -1)
                                            )
                                          ),
                                          "Prior comorbidities (-Inf, -1)" = list(
                                            targetCohortTable = "ckd_char",
                                            window = list(
                                              c(-Inf, -1)
                                            )
                                          ) 
                                        ),
                                        strata = list(c("age_group"), c("sex"), c("ses")
                                        ),
                                        otherVariables = c("ses", "ethnicity"))

stroke_results[["summmarise_characteristics_stroke"]] <- char_stroke