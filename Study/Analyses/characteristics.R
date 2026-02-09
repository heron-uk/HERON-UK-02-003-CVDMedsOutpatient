# Cohort Counts + Attrition

results[["cohort_count_mi"]] <- cdm$mi_drugs_final |>
  summariseCohortCount()

results[["cohort_count_stroke"]] <- cdm$stroke_drugs_final |>
  summariseCohortCount()

results[["cohort_attrition_mi"]] <- cdm$mi_drugs_final |>
  summariseCohortAttrition()

results[["cohort_attrition_stroke"]] <- cdm$stroke_drugs_final |>
  summariseCohortAttrition()

# Cohort Characteristics - MI

cdm$mi_drugs_chars <- cdm$mi_drugs_first |>
  PatientProfiles::addDemographics(
    sex = TRUE,
    age = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "mi_drugs_chars"
  )

cdm$mi_drugs_chars <- cdm$mi_drugs_chars |>
  mutate(
    age_group_broad = case_when(
      age >= 18 & age <= 49 ~ '18 to 49',
      age >= 50 & age <= 59 ~ '50 to 59',
      age >= 60 & age <= 69 ~ '60 to 69',
      age >= 70 & age <= 79 ~ '70 to 79',
      age >= 80 & age <= 89 ~ '80 to 89',
      age >= 90 & age <= 150 ~ '90 to 150',
      TRUE ~ 'None'  
    )
  )

char_mi <- summariseCharacteristics(cdm$mi_drugs_chars,
                                    conceptIntersectFlag = list(
                                      "Prior drug use (-30 to -1)" = list(
                                        conceptSet = mi_drugs_cl,
                                        window = list(
                                          c(-30, -1)
                                        )
                                      )),
                                    strata = list("age_group_broad", "sex",
                                                  c("age_group_broad", "sex")))


results[["summmarise_characteristics_mi"]] <- char_mi


# Cohort Characteristics - Stroke
cdm$stroke_drugs_chars <- cdm$stroke_drugs_first |>
  PatientProfiles::addDemographics(
    sex = TRUE,
    age = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "stroke_drugs_chars"
  )

cdm$stroke_drugs_chars <- cdm$stroke_drugs_chars |>
  mutate(
    age_group_broad = case_when(
      age >= 18 & age <= 49 ~ '18 to 49',
      age >= 50 & age <= 59 ~ '50 to 59',
      age >= 60 & age <= 69 ~ '60 to 69',
      age >= 70 & age <= 79 ~ '70 to 79',
      age >= 80 & age <= 89 ~ '80 to 89',
      age >= 90 & age <= 150 ~ '90 to 150',
      TRUE ~ 'None'  
    )
  )

char_stroke <- summariseCharacteristics(cdm$stroke_drugs_chars,
                                        conceptIntersectFlag = list(
                                          "Prior drug use (-30 to -1)" = list(
                                            conceptSet = stroke_drugs_cl,
                                            window = list(
                                              c(-30, -1)
                                            )
                                          )),
                                        strata = list("age_group_broad", "sex",
                                                      c("age_group_broad", "sex")))


results[["summmarise_characteristics_stroke"]] <- char_stroke