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
info(logger, "INSTANTIATING HOSPITAL MI COHORT")

cdm$hospital_mi_first <- cdm$acute_mi |>
  requireInDateRange(
    dateRange = study_period
  ) |>
  requireAge(
    ageRange = c(18, 150)
  ) |>
  requireIsFirstEntry(
    name = "hospital_mi_first"
  )

cdm$mi_inpatient <- cdm$inpatient_visit |>
  addCohortIntersectDate(targetCohortTable = "hospital_mi_first",
                         window = c(-Inf,Inf)
                         ) |>
  rename(mi_date = acute_mi_minf_to_inf) |>
  filter(!is.na(mi_date),
         mi_date >= cohort_start_date & mi_date <= cohort_end_date) |>
  compute(name = "mi_inpatient", temporary = FALSE)

cdm$mi_inpatient_first <- cdm$hospital_mi_first |>
  inner_join(cdm$mi_inpatient |> select(subject_id, 
                                        inpatient_start = cohort_start_date,
                                        inpatient_end = cohort_end_date),
             by = "subject_id") |>
  filter(cohort_start_date >= inpatient_start & cohort_end_date <= inpatient_end) |>
  compute(name = "mi_inpatient_first", temporary = FALSE)

info(logger, "INSTANTIATED HOSPITAL MI COHORT")

info(logger, "INSTANTIATING HOSPITAL STROKE COHORT")

cdm$hospital_stroke_first <- cdm$stroke |>
  requireInDateRange(
    dateRange = study_period
  ) |>
  requireAge(
    ageRange = c(18, 150)
  ) |>
  requireIsFirstEntry(
    name = "hospital_stroke_first"
  )

cdm$stroke_inpatient <- cdm$inpatient_visit |>
  addCohortIntersectDate(targetCohortTable = "hospital_stroke_first",
                         window = c(-Inf,Inf)
  ) |>
  rename(stroke_date = ischemic_stroke_minf_to_inf) |>
  filter(!is.na(stroke_date),
         stroke_date >= cohort_start_date & stroke_date <= cohort_end_date) |>
  compute(name = "stroke_inpatient", temporary = FALSE)

cdm$stroke_inpatient_first <- cdm$hospital_stroke_first |>
  inner_join(cdm$stroke_inpatient |> select(subject_id, 
                                        inpatient_start = cohort_start_date,
                                        inpatient_end = cohort_end_date),
             by = "subject_id") |>
  filter(cohort_start_date >= inpatient_start & cohort_end_date <= inpatient_end) |>
  compute(name = "stroke_inpatient_first", temporary = FALSE)

info(logger, "INSTANTIATED HOSPITAL STROKE COHORT")