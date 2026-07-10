### cardio drugs (excl. bb)
logMessage("INSTANTIATING CARDIOVASCULAR DRUGS COHORT")

if(isTRUE(beta_blockers)){

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
    window = c(-Inf, 0),
    atFirst = TRUE,
    name = "bb_hf"
  ) |>
  renameCohort(
    newCohortName = "{cohort_name}_hf"
  )

cdm$bb_no_hf <- cdm$bb_after_event |>
  requireCohortIntersect(
    targetCohortTable = "heart_failure",
    window = c(-Inf, 0),
    intersections = 0,
    atFirst = TRUE,
    name = "bb_no_hf"
  ) |>
  renameCohort(
    newCohortName = "{cohort_name}_no_hf"
  )

cdm$bb_hf_first <- cdm$bb_hf |>
  requireIsFirstEntry(name = "bb_hf_first")

cdm$bb_no_hf_first <- cdm$bb_no_hf |>
  requireIsFirstEntry(name = "bb_no_hf_first")

logMessage("INSTANTIATED BETA BLOCKERS AND HF COHORTS - MI")


cdm <- omopgenerics::bind(
  cdm$bb_after_event,
  cdm$bb_hf,
  cdm$bb_no_hf,
  name = "bb_final"
)

cdm <- omopgenerics::bind(
  cdm$bb_hf_first,
  cdm$bb_no_hf_first,
  cdm$bb_first,
  name = "bb_first"
)

} else {
  cdm <- omopgenerics::emptyCohortTable(
    cdm,
    "bb_final"
  )
  
  cdm <- omopgenerics::emptyCohortTable(
    cdm,
    "bb_first"
  )
}

#### dual antiplatelets

if(isTRUE(dual_antiplatelets)){

dat_cl <- importCodelist(here("Cohorts", "dual_antiplatelets", "codelists"), type = "csv")

cdm$dat <- conceptCohort(
  cdm = cdm,
  conceptSet = dat_cl,
  table = "drug_exposure",
  name = "dat"
)

cdm$dat_first <- cdm$dat |>
  collapseCohorts(gap = 28,
                  name = "dat_first") |>
  PatientProfiles::addCohortIntersectDate(
    window = c(-28, Inf),
    censorDate = "cohort_end_date",
    targetCohortTable = "acute_mi_first",
    nameStyle = "mi_date",
    name = "mi_drugs_first"
  )|>
  dplyr::filter(!is.na(mi_date)) |>
  dplyr::compute(name = "dat_first")  |>
  omopgenerics::recordCohortAttrition("Mi record within treatment or before") |>
  dplyr::filter(
    !cohort_end_date <= mi_date
  ) |>
  CohortConstructor::entryAtLastDate(dateColumns = c("cohort_start_date", "mi_date")) |>
  CohortConstructor::requireDuration(daysInCohort = c(2, Inf)) |> #days in cohort = 1 would include those who enter and exit the cohort on the same day. #TO DISCUSS
  requireInDateRange(study_period) |>
  requireAge(ageRange = c(18,150)) |>
  dplyr::select(-mi_date)

cdm$dual_antiplatelet_1 <- cdm$dat |>
  intersectCohorts(
    cohortId = c("aspirin", "p2y12_inhibitors"),
    gap = 28,
    name = "dual_antiplatelet_1"
  ) |>
  renameCohort(
    newCohortName = "dual_antiplatelet_1"
  )


cdm$dual_antiplatelet_1_first <- cdm$dat_first |>
  intersectCohorts(
    cohortId = c("aspirin", "p2y12_inhibitors"),
    gap = 28,
    name = "dual_antiplatelet_1_first"
  ) |>
  renameCohort(
    newCohortName = "dual_antiplatelet_1"
  )


cdm$dat_after_event_1 <- cdm$dual_antiplatelet_1 |>
  inner_join(cdm$dual_antiplatelet_1_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
             by = c("subject_id", "cohort_definition_id")) |>
  filter(cohort_start_date >= start_date) |>
  select(-c(start_date)) |>
  compute(name = "dat_after_event_1", temporary = FALSE) |>
  collapseCohorts(gap = 28)

######

cdm$dual_antiplatelet_2 <- cdm$dat |>
  intersectCohorts(
    cohortId = c("aspirin", "dipyridamole"),
    gap = 28,
    name = "dual_antiplatelet_2"
  ) |>
  renameCohort(
    newCohortName = "dual_antiplatelet_2"
  )


cdm$dual_antiplatelet_2_first <- cdm$dat_first |>
  intersectCohorts(
    cohortId = c("aspirin", "dipyridamole"),
    gap = 28,
    name = "dual_antiplatelet_2_first"
  ) |>
  renameCohort(
    newCohortName = "dual_antiplatelet_2"
  )


cdm$dat_after_event_2 <- cdm$dual_antiplatelet_2 |>
  inner_join(cdm$dual_antiplatelet_2_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
             by = c("subject_id", "cohort_definition_id")) |>
  filter(cohort_start_date >= start_date) |>
  select(-c(start_date)) |>
  compute(name = "dat_after_event_2", temporary = FALSE) |>
  collapseCohorts(gap = 28)

cdm <- omopgenerics::bind(
  cdm$dat_after_event_1,
  cdm$dat_after_event_2,
  name = "dat_final"
)

cdm$dat_final <- unionCohorts(
  cohort = cdm$dat_final,
  gap = 28,
  cohortName = "dual_antiplatelet",
  name = "dat_final"
)

cdm <- omopgenerics::bind(
  cdm$dual_antiplatelet_1_first,
  cdm$dual_antiplatelet_2_first,
  name = "dat_first"
)

} else {
  cdm <- omopgenerics::emptyCohortTable(
    cdm,
    "dat_final"
  )
  
  cdm <- omopgenerics::emptyCohortTable(
    cdm,
    "dat_first"
  )
}


### ACEi / ARBs

if(isTRUE(ace_inhibitors)){
  
  ace_cl <- importCodelist(here("Cohorts", "ace_arb"), type = "csv")
  
  cdm$ace_arbs <- conceptCohort(
    cdm = cdm,
    conceptSet = ace_cl,
    table = "drug_exposure",
    name = "ace_arbs"
  )
  
  # collapse records that are within 14 days of each other
  cdm$ace_arbs_first <- cdm$ace_arbs |>
    collapseCohorts(gap = 28,
                    name = "ace_arbs_first") |>
    PatientProfiles::addCohortIntersectDate(
      window = c(-28, Inf),
      censorDate = "cohort_end_date",
      targetCohortTable = "acute_mi_first",
      nameStyle = "mi_date",
      name = "ace_arbs_first"
    )|>
    dplyr::filter(!is.na(mi_date)) |>
    dplyr::compute(name = "ace_arbs_first")  |>
    omopgenerics::recordCohortAttrition("Mi record within treatment or before") |>
    dplyr::filter(
      !cohort_end_date <= mi_date
    ) |>
    CohortConstructor::entryAtLastDate(dateColumns = c("cohort_start_date", "mi_date")) |>
    CohortConstructor::requireDuration(daysInCohort = c(2, Inf)) |> #days in cohort = 1 would include those who enter and exit the cohort on the same day. #TO DISCUSS
    requireInDateRange(study_period) |>
    requireAge(ageRange = c(18,150)) |>
    dplyr::select(-mi_date)
  
  cdm$ace_arbs_final <- cdm$ace_arbs |>
    inner_join(cdm$ace_arbs_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
               by = c("subject_id", "cohort_definition_id")) |>
    filter(cohort_start_date >= start_date) |>
    select(-c(start_date)) |>
    compute(name = "ace_arbs_final", temporary = FALSE) |>
    collapseCohorts(gap = 28,
                    name = "ace_arbs_final")
  
} else {
  cdm <- omopgenerics::emptyCohortTable(
    cdm,
    "ace_arbs_final"
  )
  
  cdm <- omopgenerics::emptyCohortTable(
    cdm,
    "ace_arbs_first"
  )
}

### Statins

if(isTRUE(statins)){
  
  statins_cl <- importCodelist(here("Cohorts", "statins"), type = "csv")
  
  cdm$statins <- conceptCohort(
    cdm = cdm,
    conceptSet = statins_cl,
    table = "drug_exposure",
    name = "statins"
  )
  
  # collapse records that are within 14 days of each other
  cdm$statins_first <- cdm$statins |>
    collapseCohorts(gap = 28,
                    name = "statins_first") |>
    PatientProfiles::addCohortIntersectDate(
      window = c(-28, Inf),
      censorDate = "cohort_end_date",
      targetCohortTable = "acute_mi_first",
      nameStyle = "mi_date",
      name = "statins_first"
    )|>
    dplyr::filter(!is.na(mi_date)) |>
    dplyr::compute(name = "statins_first")  |>
    omopgenerics::recordCohortAttrition("Mi record within treatment or before") |>
    dplyr::filter(
      !cohort_end_date <= mi_date
    ) |>
    CohortConstructor::entryAtLastDate(dateColumns = c("cohort_start_date", "mi_date")) |>
    CohortConstructor::requireDuration(daysInCohort = c(2, Inf)) |> #days in cohort = 1 would include those who enter and exit the cohort on the same day. #TO DISCUSS
    requireInDateRange(study_period) |>
    requireAge(ageRange = c(18,150)) |>
    dplyr::select(-mi_date)
  
  cdm$statins_final <- cdm$statins |>
    inner_join(cdm$statins_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
               by = c("subject_id", "cohort_definition_id")) |>
    filter(cohort_start_date >= start_date) |>
    select(-c(start_date)) |>
    compute(name = "statins_final", temporary = FALSE) |>
    collapseCohorts(gap = 28,
                    name = "statins_final")
  
} else {
  cdm <- omopgenerics::emptyCohortTable(
    cdm,
    "statins_final"
  )
  
  cdm <- omopgenerics::emptyCohortTable(
    cdm,
    "statins_first"
  )
}


cdm <- omopgenerics::bind(
  cdm$bb_final,
  cdm$dat_final,
  cdm$ace_arbs_final,
  cdm$statins_final,
  name = "study_final"
)

cdm <-omopgenerics::bind(
  cdm$bb_first,
  cdm$dat_first,
  cdm$ace_arbs_first,
  cdm$statins_first,
  name = "study_first"
)