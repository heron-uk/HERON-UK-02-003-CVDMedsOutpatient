info(logger, "GET INPATIENT COHORT")

cdm$inpatient_visit <- conceptCohort(
  cdm = cdm,
  conceptSet = list(inpatient = c(9201,262,9203)),
  name = "inpatient_visit"
) 

cdm$inpatient_visit <- cdm$inpatient_visit |>
  collapseCohorts(gap = 1,
                  name = "inpatient_visit") 

info(logger, "GOT INPATIENT COHORT")

info(logger, "INSTANTIATING HOSPITAL CARDIOVASCULAR DRUGS COHORT")

mi_drugs_cl <- importCodelist(here("Cohorts", "Hospital", "mi_treatment"), type = "csv")

names(mi_drugs_cl) <- paste0(names(mi_drugs_cl), "_mi")

cdm$mi_drugs <- conceptCohort(
  cdm = cdm,
  conceptSet = mi_drugs_cl,
  name = "mi_drugs"
)

# collapse records that are within 14 days of each other
cdm$mi_drugs <- cdm$mi_drugs |>
  collapseCohorts(gap = 7,
                  name = "mi_drugs") 

cdm$mi_drugs_first <- cdm$mi_drugs |>
  requireCohortIntersect(
    targetCohortTable = "acute_mi",
    window = c(-7,0),
    name = "mi_drugs_first"
  ) |>
  requireIsFirstEntry() |>
  requireInDateRange(study_period) |>
  requireAge(ageRange = c(18,150))

cdm$inpatient_start_date <- cdm$mi_drugs_first |>
  PatientProfiles::addCohortIntersectDate(targetCohortTable = "inpatient_visit",
                                          name = "inpatient_start_date") |>
  filter(cohort_start_date >= inpatient_0_to_inf)

cdm$inpatient_end_date <- cdm$inpatient_start_date |>
  select(-c(inpatient_0_to_inf)) |>
  PatientProfiles::addCohortIntersectDate(targetCohortTable = "inpatient_visit",
                                          targetDate = "cohort_end_date",
                                          name = "inpatient_end_date") |>
  filter(cohort_start_date <= inpatient_0_to_inf) |>
  select(-c(inpatient_0_to_inf))

cdm$mi_drugs_after_event <- cdm$mi_drugs |>
  inner_join(cdm$inpatient_end_date |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
             by = c("subject_id", "cohort_definition_id")) |>
  filter(cohort_start_date >= start_date) |>
  select(-c(start_date)) |>
  compute(name = "mi_drugs_after_event", temporary = FALSE)

drug_count_after <- cdm$mi_drugs_after_event |>
  collect() |>
  group_by(cohort_definition_id) |>
  distinct(subject_id) |>
  tally() |>
  filter(n >= 100)

cdm$mi_drugs_final <- cdm$mi_drugs_after_event |>
  subsetCohorts(cohortId = drug_count_after$cohort_definition_id,
                name = "mi_drugs_final")

info(logger, "INSTANTIATED HOSPITAL CARDIOVASCULAR DRUGS COHORT")

info(logger, "INSTANTIATING STROKE DRUGS COHORT")

stroke_drugs_cl <- importCodelist(here("Cohorts", "Hospital", "stroke_treatment"), type = "csv")

names(stroke_drugs_cl) <- paste0(names(stroke_drugs_cl), "_stroke")

cdm$stroke_drugs <- conceptCohort(
  cdm = cdm,
  conceptSet = stroke_drugs_cl,
  name = "stroke_drugs"
)

# collapse records that are within 14 days of each other

cdm$stroke_drugs <- cdm$stroke_drugs |>
  collapseCohorts(gap = 7,
                  name = "stroke_drugs") 

cdm$stroke_drugs_first <- cdm$stroke_drugs |>
  requireCohortIntersect(
    targetCohortTable = "stroke",
    window = c(-7,0),
    name = "stroke_drugs_first"
  ) |>
  requireIsFirstEntry() |>
  requireInDateRange(study_period) |>
  requireAge(ageRange = c(18,150))

cdm$inpatient_start_date_stroke <- cdm$stroke_drugs_first |>
  PatientProfiles::addCohortIntersectDate(targetCohortTable = "inpatient_visit",
                                          name = "inpatient_start_date_stroke") |>
  filter(cohort_start_date >= inpatient_0_to_inf)

cdm$inpatient_end_date_stroke <- cdm$inpatient_start_date |>
  select(-c(inpatient_0_to_inf)) |>
  PatientProfiles::addCohortIntersectDate(targetCohortTable = "inpatient_visit",
                                          targetDate = "cohort_end_date",
                                          name = "inpatient_end_date_stroke") |>
  filter(cohort_start_date <= inpatient_0_to_inf) |>
  select(-c(inpatient_0_to_inf))

cdm$stroke_drugs_after_event <- cdm$stroke_drugs |>
  inner_join(cdm$inpatient_end_date_stroke |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
             by = c("subject_id", "cohort_definition_id")) |>
  filter(cohort_start_date >= start_date) |>
  select(-c(start_date)) |>
  compute(name = "stroke_drugs_after_event", temporary = FALSE)

drug_count_after_stroke <- cdm$stroke_drugs_after_event |>
  collect() |>
  group_by(cohort_definition_id) |>
  distinct(subject_id) |>
  tally() |>
  filter(n >= 100)

cdm$stroke_drugs_final <- cdm$stroke_drugs_after_event |>
  subsetCohorts(cohortId = drug_count_after_stroke$cohort_definition_id,
                name = "stroke_drugs_final")

info(logger, "INSTANTIATED STROKE DRUGS COHORT")