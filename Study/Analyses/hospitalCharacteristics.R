# Cohort Counts + Attrition

results[["cohort_count_mi"]] <- cdm$mi_inpatient_first |>
  summariseCohortCount()

results[["cohort_count_stroke"]] <- cdm$stroke_inpatient_first|>
  summariseCohortCount()

results[["cohort_code_use_mi"]] <- summariseCohortCodeUse(
  cohortTable = "mi_inpatient_first",
    cdm = cdm,
    timing = "entry"
  )

results[["cohort_code_use_stroke"]] <-summariseCohortCodeUse(
  cohortTable = "stroke_inpatient_first",
  cdm = cdm,
  timing = "entry"
)

results[["cohort_attrition_mi"]] <- cdm$mi_inpatient_first |>
  summariseCohortAttrition()

results[["cohort_attrition_stroke"]] <- cdm$stroke_inpatient_first |>
  summariseCohortAttrition()
  
# Cohort Characteristics - MI

cdm$mi_inpatient_chars <- cdm$mi_inpatient_first |>
  addDemographics(
    sex = TRUE,
    age = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "mi_inpatient_chars"
  ) |>
  addEthnicity() |>
  addSES()
  

cdm$mi_inpatient_chars <- cdm$mi_inpatient_chars |>
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

# MI Drug Treatment

drugs_cl <- importCodelist(
  here("Cohorts", "Hospital", "drugs"),
       type = "csv")

# MI Procedures

mi_proc <- importCodelist(
  here("Cohorts", "Hospital", "miProcedures"),
  type = "csv")

char_mi <- summariseCharacteristics(cdm$mi_inpatient_chars,
                                    ageGroup = list(
                                      "18 to 39" = c(18, 39),
                                      "40 to 49" = c(40, 49),
                                      "50 to 59" = c(50, 59),
                                      "60 to 69" = c(60, 69),
                                      "70 to 79" = c(70, 79),
                                      "80 to 89" = c(80, 89),
                                      "90+" = c(90, 150)),
                                    conceptIntersectFlag = list(
                                      "Drug Treatment (0, 14)" = list(
                                        conceptSet = drugs_cl,
                                        window = list(
                                          c(0,14)
                                        )
                                      ),
                                      "Procedures (0, 14)" = list(
                                        conceptSet = mi_proc,
                                        window = list(
                                          c(0,14)
                                        )
                                      ),
                                      "Prior ischemic stroke (-30 to -1)" = list(
                                        conceptSet = stroke_cl,
                                        window = list(
                                          c(-30, -1)
                                        )
                                      )
                                      ),
                                    strata = list(c("age_group"), c("sex"), c("ses")
                                    ),
                                    otherVariables = c("ses", "ethnicity"))


results[["summmarise_characteristics_mi"]] <- char_mi


# Cohort Characteristics - Stroke
cdm$stroke_inpatient_chars <- cdm$stroke_inpatient_first |>
  addDemographics(
    sex = TRUE,
    age = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "stroke_inpatient_chars"
  ) |>
  addEthnicity() |>
  addSES()

cdm$stroke_inpatient_chars <- cdm$stroke_inpatient_chars |>
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

# Stroke Procedures

stroke_proc <- importCodelist(
  here("Cohorts", "Hospital", "strokeProcedures"),
  type = "csv")

char_stroke <- summariseCharacteristics(cdm$stroke_inpatient_chars,
                                        ageGroup = list(
                                          "18 to 39" = c(18, 39),
                                          "40 to 49" = c(40, 49),
                                          "50 to 59" = c(50, 59),
                                          "60 to 69" = c(60, 69),
                                          "70 to 79" = c(70, 79),
                                          "80 to 89" = c(80, 89),
                                          "90+" = c(90, 150)),
                                        conceptIntersectFlag = list(
                                          "Drug Treatment (0, 14)" = list(
                                            conceptSet = drugs_cl,
                                            window = list(
                                              c(0,14)
                                            )
                                          ),
                                          "Procedures (0, 14)" = list(
                                            conceptSet = stroke_proc,
                                            window = list(
                                              c(0,14)
                                            )
                                          ),
                                          "Prior MI (-30 to -1)" = list(
                                            conceptSet = acute_mi_cl,
                                            window = list(
                                              c(-30, -1)
                                            )
                                          )
                                        ),

                                        strata = list(c("age_group"), c("sex"), c("ses")
                                        ),
                                        otherVariables = c("ses", "ethnicity"))

results[["summmarise_characteristics_stroke"]] <- char_stroke