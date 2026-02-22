# Create Acute MI Cohort
info(logger, "INSTANTIATING MI COHORT")

acute_mi_cl <- CodelistGenerator::importCodelist(
  path = here::here("Cohorts", "conditions", "acute_mi.csv"),
  type = "csv"
)

cdm$acute_mi <- conceptCohort(
  cdm = cdm,
  conceptSet = acute_mi_cl,
  name = "acute_mi"
)

cdm$acute_mi <- cdm$acute_mi |>
  requireIsFirstEntry() # first ever MI
  
info(logger, "INSTANTIATED MI COHORT")

info(logger, "INSTANTIATING STROKE COHORT")
stroke_cl <- CodelistGenerator::importCodelist(
  path = here::here("Cohorts", "conditions", "ischaemic_stroke_narrow_v3.csv"),
  type = "csv"
)

cdm$stroke <- conceptCohort(
  cdm = cdm,
  conceptSet = stroke_cl,
  name = "stroke"
)

cdm$stroke <- cdm$stroke |>
  requireIsFirstEntry() # first ever stroke

info(logger, "INSTANTIATED STROKE COHORT")

info(logger, "INSTANTIATING HEART FAILURE COHORT")
hf_cl <- CodelistGenerator::importCodelist(
  path = here::here("Cohorts", "conditions", "heart_failure.csv"),
  type = "csv"
)

cdm$heart_failure <- conceptCohort(
  cdm = cdm,
  conceptSet = hf_cl,
  name = "heart_failure"
)

cdm$heart_failure <- cdm$heart_failure |>
  requireIsFirstEntry() # first ever HF

info(logger, "INSTANTIATED HEART FAILURE COHORT")

info(logger, "INSTANTIATING ATRIAL FIBRILLATION COHORT")
af_cl <- CodelistGenerator::importCodelist(
  path = here::here("Cohorts", "conditions", "atrial_fibrillation.csv"),
  type = "csv"
)

cdm$atrial_fibrillation <- conceptCohort(
  cdm = cdm,
  conceptSet = af_cl,
  name = "atrial_fibrillation"
)

cdm$atrial_fibrillation <- cdm$atrial_fibrillation |>
  requireIsFirstEntry() # first ever AF

info(logger, "INSTANTIATED ATRIAL FIBRILLATION COHORT")

info(logger, "INSTANTIATING CAROTID DISEASE COHORT")
cd_cl <- CodelistGenerator::importCodelist(
  path = here::here("Cohorts", "conditions", "carotid_disease.csv"),
  type = "csv"
)

cdm$carotid_disease <- conceptCohort(
  cdm = cdm,
  conceptSet = cd_cl,
  name = "carotid_disease"
)

cdm$carotid_disease <- cdm$carotid_disease |>
  requireIsFirstEntry() # first ever AF

info(logger, "INSTANTIATED CAROTID DISEASE COHORT")
