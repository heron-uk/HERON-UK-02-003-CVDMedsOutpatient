info(logger, "INSTANTIATING CARDIOVASCULAR DRUGS COHORT - STROKE")

stroke_drugs_cl <- importCodelist(here("Cohorts", "Primary", "drugs"), type = "csv")

names(stroke_drugs_cl) <- paste0(names(stroke_drugs_cl), "_stroke")

cdm$stroke_drugs <- conceptCohort(
  cdm = cdm,
  conceptSet = stroke_drugs_cl,
  name = "stroke_drugs"
)

# collapse records that are within 14 days of each other
cdm$stroke_drugs <- cdm$stroke_drugs |>
  collapseCohorts(gap = 14,
                  name = "stroke_drugs") 

cdm$stroke_drugs_first <- cdm$stroke_drugs |>
  requireCohortIntersect(
    targetCohortTable = "stroke",
    window = c(-30,0),
    name = "stroke_drugs_first"
  ) |>
  requireIsFirstEntry() |>
  requireInDateRange(study_period) |>
  requireAge(ageRange = c(18,150))

cdm$stroke_drugs_after_event <- cdm$stroke_drugs |>
  inner_join(cdm$stroke_drugs_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
             by = c("subject_id", "cohort_definition_id")) |>
  filter(cohort_start_date >= start_date) |>
  select(-c(start_date)) |>
  compute(name = "stroke_drugs_after_event", temporary = FALSE)

info(logger, "INSTANTIATE DUAL ANTIPLATELETS COHORT - STROKE")
### antiplatelets

cdm$dual_antiplatelet_stroke <- cdm$stroke_drugs_after_event |>
  intersectCohorts(
    cohortId = c("aspirin_stroke", "p2y12_inhibitors_stroke"),
    gap = 14,
    name = "dual_antiplatelet_stroke"
  ) |>
  renameCohort(
    newCohortName = "dual_antiplatelet_stroke"
  )

info(logger, "INSTANTIATED DUAL ANTIPLATELETS COHORTS - STROKE")

########
info(logger, "INSTANTIATE DUAL ANTIPLATELETS COHORT FOR CAROTID STENOSIS - STROKE")
### antiplatelets

cdm$dual_antiplatelet_stroke_cs <- cdm$dual_antiplatelet_stroke|>
  requireCohortIntersect(
    targetCohortTable = "carotid_disease",
    window = c(-Inf, -1),
    atFirst = TRUE,
    name = "dual_antiplatelet_stroke_cs"
  ) |>
  renameCohort(
    newCohortName = "dual_antiplatelet_stroke_cs"
  )

info(logger, "INSTANTIATED DUAL ANTIPLATELETS COHORTS FOR CAROTID STENOSIS - STROKE")

info(logger, "INSTANTIATE ANTICOAGULANT COHORT - STROKE")
### anticoagulents

cdm$anticoagulants_stroke <- unionCohorts(
  cohort = cdm$stroke_drugs_after_event,
  cohortId = c("warfarin_stroke", "doacs_stroke"),
  gap = 14,
  keepOriginalCohorts = FALSE,
  name = "anticoagulants_stroke"
) |>
  renameCohort(
    newCohortName = "anticoagulants_stroke"
  )

info(logger, "INSTANTIATED ANTICOAGULANT COHORT - STROKE")

info(logger, "INSTANTIATE ANTICOAGULANT WITH AF COHORT - STROKE")

cdm$anticoagulants_stroke_af <- cdm$anticoagulants_stroke |>
  requireCohortIntersect(
    targetCohortTable = "atrial_fibrillation",
    window = c(-Inf, -1),
    atFirst = TRUE,
    name = "anticoagulants_stroke_af"
  ) |>
  renameCohort(
    newCohortName = "anticoagulants_stroke_af"
  )

info(logger, "INSTANTIATED ANTICOAGULANT WITH AF COHORT - STROKE")

info(logger, "INSTANTIATE LIPID-LOWERING COHORT - STROKE")
### antiplatelets

cdm$lipid_lowering_stroke <- unionCohorts(
  cohort = cdm$stroke_drugs_after_event,
  cohortId = c("statin_stroke", "pcsk9_inhibitors_stroke", "ezetimibe_stroke"),
  gap = 14,
  keepOriginalCohorts = FALSE,
  name = "lipid_lowering_stroke"
) |>
  renameCohort(
    newCohortName = "lipid_lowering_stroke"
  )

info(logger, "INSTANTIATED LIPID-LOWERING COHORT - STROKE")

info(logger, "INSTANTIATE ANTIHYPERTENSIVE COHORT - STROKE")
### antiplatelets

cdm$antihypertensive_stroke <- unionCohorts(
  cohort = cdm$stroke_drugs_after_event,
  cohortId = c("acei_arbs_stroke", "calcium_channel_blockers_stroke", "thiazide_diuretics_stroke"),
  gap = 14,
  keepOriginalCohorts = FALSE,
  name = "antihypertensive_stroke"
) |>
  renameCohort(
    newCohortName = "antihypertensive_stroke"
  )

info(logger, "INSTANTIATED ANTIHYPERTENSIVE COHORT - STROKE")


### combine into one table

cdm <- omopgenerics::bind(
  cdm$stroke_drugs_after_event,
  cdm$dual_antiplatelet_stroke,
  cdm$dual_antiplatelet_stroke_cs,
  cdm$anticoagulants_stroke,
  cdm$anticoagulants_stroke_af,
  cdm$lipid_lowering_stroke,
  cdm$antihypertensive_stroke,
  name = "stroke_drugs_final"
)

info(logger, "INSTANTIATED CARDIOVASCULAR DRUGS COHORT - STROKE")
