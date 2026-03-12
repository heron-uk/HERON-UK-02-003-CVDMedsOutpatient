# parametrisation
procWindow <- c(-28, 28)
procNm <- "Procedures c(-28, 28)"

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

comorb <- importCodelist(
  here("Cohorts", "comorbidities"),
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
                                      list(
                                        conceptSet = mi_proc,
                                        window = procWindow
                                      ) |>
                                        rlang::set_names(procNm),
                                      "Prior Comorbidities (-Inf to 0)" = list(
                                        conceptSet = stroke_cl,
                                        window = list(
                                          c(-Inf, 0)
                                        )
                                      ),
                                      "Prior Comorbidities (-Inf to 0)" = list(
                                        conceptSet = comorb,
                                        window = list(
                                          c(-Inf, 0)
                                        )
                                      )
                                    ),

                                    tableIntersectFlag = list(
                                      "30-day mortality" = list(
                                        tableName = "death",
                                        window = c(0, 30)
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
                                          list(
                                            conceptSet = stroke_proc,
                                            window = procWindow
                                          ) |>
                                            rlang::set_names(procNm),
                                          "Prior Comorbidities (-Inf to 0)" = list(
                                            conceptSet = acute_mi_cl,
                                            window = list(
                                              c(-Inf, 0)
                                            )
                                          ),
                                          "Prior Comorbidities (-Inf to 0)" = list(
                                            conceptSet = comorb,
                                            window = list(
                                              c(-Inf, 0)
                                            )
                                          )
                                        ),
                                        tableIntersectFlag = list(
                                          "30-day mortality" = list(
                                            tableName = "death",
                                            window = c(0, 30)
                                          )
                                        ),

                                        strata = list(c("age_group"), c("sex"), c("ses")
                                        ),
                                        otherVariables = c("ses", "ethnicity"))

results[["summmarise_characteristics_stroke"]] <- char_stroke

if (omopgenerics::cdmVersion(cdm) == "5.3") {
  colsAD <- c("admit" = "admitting_source_concept_id", "discharge" = "discharge_to_concept_id")
} else {
  colsAD <- c("admit" = "admitted_from_concept_id", "discharge" = "discharged_to_concept_id")
}

# timing between admission fro Stroke and medication/procedures
nm <- omopgenerics::uniqueTableName()
strokeAdmission <- cdm$visit_occurrence |>
  dplyr::inner_join(
    cdm$stroke_inpatient |>
      dplyr::select(
        "person_id" = "subject_id", "visit_start_date" = "cohort_start_date",
        "stroke_date"
      ),
    by = c("person_id", "visit_start_date")
  ) |>
  dplyr::filter(
    .data$visit_concept_id %in% c(9201, 262, 9203) &
      .data$visit_start_date <= .data$stroke_date &
      .data$visit_end_date >= .data$stroke_date
  ) |>
  dplyr::select(
    "person_id", "visit_start_datetime", dplyr::all_of(colsAD), "visit_end_date"
  ) |>
  dplyr::compute(name = nm)
  
codes <- drugs_cl[c("thrombolytics_alteplase", "thrombolytics_tenecteplase")] |>
  dplyr::as_tibble() |>
  dplyr::rename("drug" = "codelist_name", "drug_concept_id" = "concept_id")

nm <- omopgenerics::uniqueTableName()
cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = codes)

drugOfInterest <- cdm$drug_exposure |>
  dplyr::select("person_id", "drug_concept_id", "drug_exposure_start_date",
                "drug_exposure_start_datetime") |>
  dplyr::inner_join(cdm[[nm]], by = "drug_concept_id") |>
  dplyr::inner_join(
    strokeAdmission |>
      dplyr::select("person_id", "visit_start_datetime", "visit_end_date"),
    by = "person_id"
  ) |>
  dplyr::filter(
    drug_exposure_start_date <= visit_end_date &
      visit_start_datetime <= drug_exposure_start_datetime
  ) |>
  dplyr::group_by(person_id, drug) |>
  dplyr::summarise(drug_exposure_start_datetime = min(drug_exposure_start_datetime)) |>
  dplyr::collect() |>
  tidyr::pivot_wider(
    names_from = "drug",
    values_from = "drug_exposure_start_datetime"
  )

codes <- stroke_proc |>
  dplyr::as_tibble() |>
  dplyr::rename("procedure" = "codelist_name", "procedure_concept_id" = "concept_id")
nm <- omopgenerics::uniqueTableName()
cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = codes)

proceduresOfInterest <- cdm$procedure_occurrence |>
  dplyr::select("person_id", "procedure_concept_id", "procedure_date",
                "procedure_datetime") |>
  dplyr::inner_join(cdm[[nm]], by = "procedure_concept_id") |>
  dplyr::inner_join(
    strokeAdmission |>
      dplyr::select("person_id", "visit_start_datetime", "visit_end_date"),
    by = "person_id"
  ) |>
  dplyr::filter(
    procedure_date <= visit_end_date &
      visit_start_datetime <= procedure_datetime
  ) |>
  dplyr::group_by(person_id, procedure) |>
  dplyr::summarise(procedure_datetime = min(procedure_datetime)) |>
  dplyr::collect() |>
  tidyr::pivot_wider(
    names_from = "procedure",
    values_from = "procedure_datetime"
  )

x <- strokeAdmission |>
  dplyr::select("person_id", "visit_start_datetime", "admit", "discharge") |>
  dplyr::collect() |>
  dplyr::inner_join(drugOfInterest, by = "person_id") |>
  dplyr::inner_join(proceduresOfInterest, by = "person_id")

cols <- c(names(stroke_proc), "thrombolytics_alteplase", "thrombolytics_tenecteplase")
for (col in cols) {
  if (col %in% colnames(x)) {
    x[[col]] <- as.numeric(difftime(time1 = x[[col]], time2 = x$visit_start_datetime, units = "mins"))
  } else {
    x[[col]] <- as.numeric(NA)
  }
}

x$admit <- sprintf("%i", x$admit)
x$discharge <- sprintf("%i", x$discharge)
x$cohort_name <- "stroke"

results$extra_stroke <- PatientProfiles::summariseResult(
  table = x,
  group = "cohort_name", 
  variables = list(
    c("discharge", "admit"),
    cols
  ),
  estimates = list(
    c("count", "percentage"),
    c("percentage_missing", "count_missing", "min", "max", "q05", "q25", 
      "median", "q75", "q95")
  )
)

# timing between admission from MI and medication/procedures
nm <- omopgenerics::uniqueTableName()
miAdmission <- cdm$visit_occurrence |>
  dplyr::inner_join(
    cdm$mi_inpatient |>
      dplyr::select(
        "person_id" = "subject_id", "visit_start_date" = "cohort_start_date",
        "mi_date"
      ),
    by = c("person_id", "visit_start_date")
  ) |>
  dplyr::filter(
    .data$visit_concept_id %in% c(9201, 262, 9203) &
      .data$visit_start_date <= .data$mi_date &
      .data$visit_end_date >= .data$mi_date
  ) |>
  dplyr::select(
    "person_id", "visit_start_datetime", dplyr::all_of(colsAD), "visit_end_date"
  ) |>
  dplyr::compute(name = nm)

codes <- drugs_cl[c("thrombolytics_alteplase", "thrombolytics_tenecteplase")] |>
  dplyr::as_tibble() |>
  dplyr::rename("drug" = "codelist_name", "drug_concept_id" = "concept_id")
nm <- omopgenerics::uniqueTableName()
cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = codes)

drugOfInterest <- cdm$drug_exposure |>
  dplyr::select("person_id", "drug_concept_id", "drug_exposure_start_date",
                "drug_exposure_start_datetime") |>
  dplyr::inner_join(cdm[[nm]], by = "drug_concept_id") |>
  dplyr::inner_join(
    miAdmission |>
      dplyr::select("person_id", "visit_start_datetime", "visit_end_date"),
    by = "person_id"
  ) |>
  dplyr::filter(
    drug_exposure_start_date <= visit_end_date &
      visit_start_datetime <= drug_exposure_start_datetime
  ) |>
  dplyr::group_by(person_id, drug) |>
  dplyr::summarise(drug_exposure_start_datetime = min(drug_exposure_start_datetime)) |>
  dplyr::collect() |>
  tidyr::pivot_wider(
    names_from = "drug",
    values_from = "drug_exposure_start_datetime"
  )

codes <- mi_proc |>
  dplyr::as_tibble() |>
  dplyr::rename("procedure" = "codelist_name", "procedure_concept_id" = "concept_id")
nm <- omopgenerics::uniqueTableName()
cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = codes)

proceduresOfInterest <- cdm$procedure_occurrence |>
  dplyr::select("person_id", "procedure_concept_id", "procedure_date",
                "procedure_datetime") |>
  dplyr::inner_join(cdm[[nm]], by = "procedure_concept_id") |>
  dplyr::inner_join(
    miAdmission |>
      dplyr::select("person_id", "visit_start_datetime", "visit_end_date"),
    by = "person_id"
  ) |>
  dplyr::filter(
    procedure_date <= visit_end_date &
      visit_start_datetime <= procedure_datetime
  ) |>
  dplyr::group_by(person_id, procedure) |>
  dplyr::summarise(procedure_datetime = min(procedure_datetime)) |>
  dplyr::collect() |>
  tidyr::pivot_wider(
    names_from = "procedure",
    values_from = "procedure_datetime"
  )

x <- miAdmission |>
  dplyr::select("person_id", "visit_start_datetime", "admit", "discharge") |>
  dplyr::collect() |>
  dplyr::inner_join(drugOfInterest, by = "person_id") |>
  dplyr::inner_join(proceduresOfInterest, by = "person_id")

cols <- c(names(mi_proc), "thrombolytics_alteplase", "thrombolytics_tenecteplase")
for (col in cols) {
  if (col %in% colnames(x)) {
    x[[col]] <- as.numeric(difftime(time1 = x[[col]], time2 = x$visit_start_datetime, units = "mins"))
  } else {
    x[[col]] <- as.numeric(NA)
  }
}

x$admit <- sprintf("%i", x$admit)
x$discharge <- sprintf("%i", x$discharge)
x$cohort_name <- "acute_mi"

results$extra_mi <- PatientProfiles::summariseResult(
  table = x,
  group = "cohort_name", 
  variables = list(
    c("discharge", "admit"),
    cols
  ),
  estimates = list(
    c("count", "percentage"),
    c("percentage_missing", "count_missing", "min", "max", "q05", "q25", 
      "median", "q75", "q95")
  )
)
