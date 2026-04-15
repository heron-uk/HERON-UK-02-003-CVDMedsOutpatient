# Cohort Counts + Attrition

results[["cohort_count_mi_drugs"]] <- cdm$mi_drugs_final |>
  summariseCohortCount()

results[["cohort_count_stroke_drugs"]] <- cdm$stroke_drugs_final |>
  summariseCohortCount()

results[["cohort_count_mis"]] <- cdm$acute_mi_first |>
  summariseCohortCount()

results[["cohort_count_stroke"]] <- cdm$stroke_first |>
  summariseCohortCount()

results[["cohort_code_use_mi_drugs"]] <- summariseCohortCodeUse(
  cohortTable = "mi_drugs_first",
    cdm = cdm,
    timing = "entry"
  )

results[["cohort_code_use_stroke_drugs"]] <-summariseCohortCodeUse(
  cohortTable = "stroke_drugs_first",
  cdm = cdm,
  timing = "entry"
)

results[["cohort_code_use_mi"]] <- summariseCohortCodeUse(
  cohortTable = "acute_mi_first",
  cdm = cdm,
  timing = "entry"
)

results[["cohort_code_use_stroke"]] <-summariseCohortCodeUse(
  cohortTable = "stroke_first",
  cdm = cdm,
  timing = "entry"
)

results[["cohort_attrition_mi_drugs"]] <- cdm$mi_drugs_first |>
  summariseCohortAttrition()

results[["cohort_attrition_stroke_drugs"]] <- cdm$stroke_drugs_first|>
  summariseCohortAttrition()

results[["cohort_attrition_mi"]] <- cdm$acute_mi_first |>
  summariseCohortAttrition()

results[["cohort_attrition_stroke"]] <- cdm$stroke_first |>
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
  
# Cohort Characteristics - MI

cdm <- omopgenerics::bind(
  cdm$acute_mi_first,
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


cdm$char_anticoag <- cdm$mi_drugs |>
  subsetCohorts(
    cohortId = c("warfarin_mi", "doacs_mi"),
    name = "char_anticoag"
  ) |>
  unionCohorts(
    cohortId = c("warfarin_mi", "doacs_mi"),
    gap = 28,
    keepOriginalCohorts = FALSE,
    cohortName = "any_anticoagulant_mi",
  )

cdm$char_antip <- cdm$mi_drugs |>
  subsetCohorts(
    cohortId = c("aspirin_mi", "dipyridamole_mi", "p2y12_inhibitors_mi"),
    name = "char_antip"
  ) |>
  unionCohorts(
    cohortId = c("aspirin_mi", "dipyridamole_mi", "p2y12_inhibitors_mi"),
    gap = 28,
    keepOriginalCohorts = FALSE,
    cohortName = "any_antiplatelet_mi"
  )

cdm$char_antihyp <- cdm$mi_drugs |>
  subsetCohorts(
    cohortId = c("acei_arbs_mi", "beta_blockers_mi", "calcium_channel_blockers_mi", "thiazide_diuretics_mi"),
    name = "char_antihyp"
  ) |>
  unionCohorts(
    cohortId = c("acei_arbs_mi", "beta_blockers_mi", "calcium_channel_blockers_mi", "thiazide_diuretics_mi"),
    gap = 28,
    keepOriginalCohorts = FALSE,
    cohortName = "any_antihypertensives_mi"
  )

cdm$char_lip_lowering <- cdm$mi_drugs |>
  subsetCohorts(
    cohortId = c("statin_mi", "pcsk9_inhibitors_mi", "ezetimibe_mi"),
    name = "char_lip_lowering"
  ) |>
  unionCohorts(
    cohortId = c("statin_mi", "pcsk9_inhibitors_mi", "ezetimibe_mi"),
    gap = 28,
    keepOriginalCohorts = FALSE,
    cohortName = "any_lipid_lowering_mi"
  )

cdm <- omopgenerics::bind(
  cdm$char_anticoag,
  cdm$char_antihyp,
  cdm$char_antip,
  cdm$char_lip_lowering,
  name = "mi_drug_char"
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
                                        targetCohortTable = "mi_drug_char",
                                        window = list(
                                          c(-28, -1)
                                        )
                                      ),
                                      "Future drug use (0 to 28)" = list(
                                        targetCohortTable = "mi_drug_char",
                                        window = list(
                                          c(0, 28)
                                        )
                                      ),
                                      "Prior stroke (-Inf to -1)" = list(
                                        targetCohortTable = "stroke",
                                        window = list(
                                          c(-Inf, -1)
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
                                    otherVariables = c("ses", "ethnicity", "ckd_stage"))


results[["summmarise_characteristics_mi"]] <- char_mi


# Cohort Characteristics - Stroke

cdm <- omopgenerics::bind(
  cdm$stroke_first,
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

cdm$char_anticoag <- cdm$stroke_drugs |>
  subsetCohorts(
    cohortId = c("warfarin_stroke", "doacs_stroke"),
    name = "char_anticoag"
  ) |>
  unionCohorts(
    cohortId = c("warfarin_stroke", "doacs_stroke"),
    gap = 28,
    keepOriginalCohorts = FALSE,
    cohortName = "any_anticoagulant_stroke",
  )

cdm$char_antip <- cdm$stroke_drugs |>
  subsetCohorts(
    cohortId = c("aspirin_stroke", "dipyridamole_stroke", "p2y12_inhibitors_stroke"),
    name = "char_antip"
  ) |>
  unionCohorts(
    cohortId = c("aspirin_stroke", "dipyridamole_stroke", "p2y12_inhibitors_stroke"),
    gap = 28,
    keepOriginalCohorts = FALSE,
    cohortName = "any_antiplatelet_stroke"
  )

cdm$char_antihyp <- cdm$stroke_drugs |>
  subsetCohorts(
    cohortId = c("acei_arbs_stroke", "beta_blockers_stroke", "calcium_channel_blockers_stroke", "thiazide_diuretics_stroke"),
    name = "char_antihyp"
  ) |>
  unionCohorts(
    cohortId = c("acei_arbs_stroke", "beta_blockers_stroke", "calcium_channel_blockers_stroke", "thiazide_diuretics_stroke"),
    gap = 28,
    keepOriginalCohorts = FALSE,
    cohortName = "any_antihypertensives_stroke"
  )

cdm$char_lip_lowering <- cdm$stroke_drugs |>
  subsetCohorts(
    cohortId = c("statin_stroke", "pcsk9_inhibitors_stroke", "ezetimibe_stroke"),
    name = "char_lip_lowering"
  ) |>
  unionCohorts(
    cohortId = c("statin_stroke", "pcsk9_inhibitors_stroke", "ezetimibe_stroke"),
    gap = 28,
    keepOriginalCohorts = FALSE,
    cohortName = "any_lipid_lowering_stroke"
  )

cdm <- omopgenerics::bind(
  cdm$char_anticoag,
  cdm$char_antihyp,
  cdm$char_antip,
  cdm$char_lip_lowering,
  name = "stroke_drug_char"
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
                                            targetCohortTable = "stroke_drug_char",
                                            window = list(
                                              c(-28, -1)
                                            )
                                          ),
                                          "Future drug use (0 to 28)" = list(
                                            targetCohortTable = "stroke_drug_char",
                                            window = list(
                                              c(0, 28)
                                            )
                                          ),
                                          "Prior myocardial infarction (-Inf to -1)" = list(
                                            targetCohortTable = "acute_mi",
                                            window = list(
                                              c(-Inf, -1)
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
                                        otherVariables = c("ses", "ethnicity", "ckd_stage"))

results[["summmarise_characteristics_stroke"]] <- char_stroke