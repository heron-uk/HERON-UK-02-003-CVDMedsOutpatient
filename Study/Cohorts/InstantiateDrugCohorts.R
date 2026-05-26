### cardio drugs (excl. bb)
logMessage("INSTANTIATING CARDIOVASCULAR DRUGS COHORT")

bb_drugs_cl <- importCodelist(here("Cohorts", "beta_blockers", "codelists"), type = "csv")

cdm$beta_blockers <- conceptCohort(
  cdm = cdm,
  conceptSet = bb_drugs_cl,
  table = "drug_exposure",
  name = "beta_blockers"
)

# collapse records that are within 14 days of each other
cdm$bb_first <- cdm$beta_blockers |>
  collapseCohorts(gap = 28,
                  name = "bb_first") |>
  PatientProfiles::addCohortIntersectDate(
    window = c(-28, Inf),
    censorDate = "cohort_end_date",
    targetCohortTable = "acute_mi_first",
    nameStyle = "mi_date",
    name = "bb_first"
  )|>
  dplyr::filter(!is.na(mi_date)) |>
  dplyr::compute(name = "bb_first")  |>
  omopgenerics::recordCohortAttrition("Mi record within treatment or before") |>
  dplyr::filter(
    !cohort_end_date <= mi_date
  ) |>
  CohortConstructor::entryAtLastDate(dateColumns = c("cohort_start_date", "mi_date")) |>
  CohortConstructor::requireDuration(daysInCohort = c(2, Inf)) |> #days in cohort = 1 would include those who enter and exit the cohort on the same day. #TO DISCUSS
  requireInDateRange(study_period) |>
  requireAge(ageRange = c(18,150)) |>
  dplyr::select(-mi_date)

cdm$bb_after_event <- cdm$beta_blockers |>
  inner_join(cdm$bb_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
             by = c("subject_id", "cohort_definition_id")) |>
  filter(cohort_start_date >= start_date) |>
  select(-c(start_date)) |>
  compute(name = "bb_after_event", temporary = FALSE) |>
  collapseCohorts(gap = 28,
                  name = "bb_after_event")


logMessage("INSTANTIATED BETA BLOCKERS COHORT")

########
logMessage("INSTANTIATE BETA BLOCKERS AND HF COHORTS - MI")
### beta blockers


cdm$bb_hf <- cdm$bb_after_event |>
  requireCohortIntersect(
    targetCohortTable = "heart_failure",
    window = c(-Inf, -1),
    atFirst = TRUE,
    name = "bb_hf"
  ) |>
  renameCohort(
    newCohortName = "bb_hf"
  )

cdm$bb_no_hf <- cdm$bb_after_event |>
  requireCohortIntersect(
    targetCohortTable = "heart_failure",
    window = c(-Inf, -1),
    intersections = 0,
    atFirst = TRUE,
    name = "bb_no_hf"
  ) |>
  renameCohort(
    newCohortName = "bb_no_hf"
  )

cdm$bb_hf_first <- cdm$bb_hf |>
  requireIsFirstEntry(name = "bb_hf_first")

cdm$bb_no_hf_first <- cdm$bb_no_hf |>
  requireIsFirstEntry(name = "bb_no_hf_first")

logMessage("INSTANTIATED BETA BLOCKERS AND HF COHORTS - MI")