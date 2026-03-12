info(logger, "GET INPATIENT COHORT")

stroke_broad <- importCodelist(path = here("Cohorts", "Hospital"), type = "csv")
cdm$stroke_broad <- conceptCohort(
  cdm = cdm,
  conceptSet = stroke_broad,
  name = "stroke_broad"
)

cdm <- bind(cdm$stroke, cdm$stroke_broad, name = "stroke")


cdm$inpatient_visit <- conceptCohort(
  cdm = cdm,
  conceptSet = list(inpatient = c(9201,262,9203)),
  name = "inpatient_visit"
) 

cdm$inpatient_visit <- cdm$inpatient_visit |>
  collapseCohorts(gap = 1, name = "inpatient_visit") 

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

cdm$mi_inpatient_first <- cdm$hospital_mi_first |>
  irequireCohortIntersect(
    targetCohortTable = "inpatient_visit",
    window = c(0, 0),
    name = "mi_inpatient_first"
  )

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

cdm$stroke_inpatient_first <- cdm$hospital_stroke_first |>
  requireCohortIntersect(
    cohortId = "ischemic_stroke",
    targetCohortTable = "inpatient_visit",
    window = c(0, 0),
    name = "stroke_inpatient_first"
  ) |>
  requireCohortIntersect(
    cohortId = "stroke_broad",
    targetCohortTable = "inpatient_visit",
    window = c(0, 0)
  )

info(logger, "INSTANTIATED HOSPITAL STROKE COHORT")
