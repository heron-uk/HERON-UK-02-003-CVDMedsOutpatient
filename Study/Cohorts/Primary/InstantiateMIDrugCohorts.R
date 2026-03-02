### cardio drugs (excl. bb)
info(logger, "INSTANTIATING CARDIOVASCULAR DRUGS COHORT")

mi_drugs_cl <- importCodelist(here("Cohorts", "Primary", "drugs"), type = "csv")

names(mi_drugs_cl) <- paste0(names(mi_drugs_cl), "_mi")

cdm$mi_drugs <- conceptCohort(
  cdm = cdm,
  conceptSet = mi_drugs_cl,
  name = "mi_drugs"
)

# collapse records that are within 14 days of each other
cdm$mi_drugs <- cdm$mi_drugs |>
  collapseCohorts(gap = 14,
                  name = "mi_drugs") 

cdm$mi_drugs_first <- cdm$mi_drugs |>
  requireCohortIntersect(
    targetCohortTable = "acute_mi",
    window = c(-30,0),
    name = "mi_drugs_first"
  ) |>
  requireIsFirstEntry() |>
  requireInDateRange(study_period) |>
  requireAge(ageRange = c(18,150))

cdm$mi_drugs_after_event <- cdm$mi_drugs |>
  inner_join(cdm$mi_drugs_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
             by = c("subject_id", "cohort_definition_id")) |>
  filter(cohort_start_date >= start_date) |>
  select(-c(start_date)) |>
  compute(name = "mi_drugs_after_event", temporary = FALSE)


info(logger, "INSTANTIATED CARDIOVASCULAR DRUGS COHORT")

########
info(logger, "INSTANTIATE BETA BLOCKERS AND HF COHORTS")
### beta blockers

cdm$beta_blockers_after_event <- cdm$mi_drugs_after_event |>
  subsetCohorts(
    cohortId = "beta_blockers_mi",
    name = "beta_blockers_after_event"
  )

cdm$beta_blockers_hf <- cdm$beta_blockers_after_event |>
  requireCohortIntersect(
    targetCohortTable = "heart_failure",
    window = c(-Inf, -1),
    atFirst = TRUE,
    name = "beta_blockers_hf"
  ) |>
  renameCohort(
    newCohortName = "beta_blockers_hf"
  )

cdm$beta_blockers_no_hf <- cdm$beta_blockers_after_event |>
  requireCohortIntersect(
    targetCohortTable = "heart_failure",
    window = c(-Inf, -1),
    intersections = 0,
    atFirst = TRUE,
    name = "beta_blockers_no_hf"
  ) |>
  renameCohort(
    newCohortName = "beta_blockers_no_hf"
  )

info(logger, "INSTANTIATED BETA BLOCKERS AND HF COHORTS")

########
info(logger, "INSTANTIATE DUAL ANTIPLATELETS COHORT")
### antiplatelets

cdm$dual_antiplatelet_mi <- cdm$mi_drugs_after_event |>
  intersectCohorts(
    cohortId = c("aspirin_mi", "p2y12_inhibitors_mi"),
    gap = 14,
    name = "dual_antiplatelet_mi"
  ) |>
  renameCohort(
    newCohortName = "dual_antiplatelet_mi"
  )

info(logger, "INSTANTIATED DUAL ANTIPLATELETS COHORTS")

########
info(logger, "INSTANTIATE DUAL ANTIPLATELETS COHORT FOR CAROTID STENOSIS")
### antiplatelets

cdm$dual_antiplatelet_mi_cs <- cdm$dual_antiplatelet_mi|>
  requireCohortIntersect(
    targetCohortTable = "carotid_disease",
    window = c(-Inf, -1),
    atFirst = TRUE,
    name = "dual_antiplatelet_mi_cs"
  ) |>
  renameCohort(
    newCohortName = "dual_antiplatelet_mi_cs"
  )

info(logger, "INSTANTIATED DUAL ANTIPLATELETS COHORTS FOR CAROTID STENOSIS")

info(logger, "INSTANTIATE ANTICOAGULANT COHORT")
### anticoagulents

cdm$anticoagulants_mi <- unionCohorts(
    cohort = cdm$mi_drugs_after_event,
    cohortId = c("warfarin_mi", "doacs_mi"),
    gap = 14,
    keepOriginalCohorts = FALSE,
    name = "anticoagulants_mi"
  ) |>
  renameCohort(
    newCohortName = "anticoagulants_mi"
  )

info(logger, "INSTANTIATED ANTICOAGULANT COHORT")

info(logger, "INSTANTIATE ANTICOAGULANT WITH AF COHORT")
### anticoagulents

cdm$anticoagulants_mi_af <- cdm$anticoagulants_mi |>
  requireCohortIntersect(
    targetCohortTable = "atrial_fibrillation",
    window = c(-Inf, -1),
    atFirst = TRUE,
    name = "anticoagulants_mi_af"
  ) |>
  renameCohort(
    newCohortName = "anticoagulants_mi_af"
  )

info(logger, "INSTANTIATED ANTICOAGULANT WITH AF COHORT")

info(logger, "INSTANTIATE LIPID-LOWERING COHORT")
### antiplatelets
  
  cdm$lipid_lowering_mi <- unionCohorts(
    cohort = cdm$mi_drugs_after_event,
    cohortId = c("statin_mi", "pcsk9_inhibitors_mi", "ezetimibe_mi"),
    gap = 14,
    keepOriginalCohorts = FALSE,
    name = "lipid_lowering_mi"
  ) |>
  renameCohort(
    newCohortName = "lipid_lowering_mi"
  )

info(logger, "INSTANTIATED LIPID-LOWERING COHORT")


### combine into one table
  
cdm <- omopgenerics::bind(
  cdm$mi_drugs_after_event,
  cdm$beta_blockers_hf,
  cdm$beta_blockers_no_hf,
  cdm$dual_antiplatelet_mi,
  cdm$p2y12_and_aspirin_mi_cs,
  cdm$anticoagulants_mi,
  cdm$lipid_lowering_mi,
  name = "mi_drugs_final"
)

drug_count_after <- cdm$mi_drugs_final |>
  collect() |>
  group_by(cohort_definition_id) |>
  distinct(subject_id) |>
  tally() |>
  filter(n >= 100)

cdm$mi_drugs_final <- cdm$mi_drugs_final |>
  subsetCohorts(cohortId = drug_count_after$cohort_definition_id,
                name = "mi_drugs_final")

info(logger, "INSTANTIATED CARDIOVASCULAR DRUGS COHORT")
