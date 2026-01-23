### get acute MI

acute_mi_cl <- CodelistGenerator::codesFromCohort(
  path = here::here("Cohorts", "outcomes"),
  cdm = cdm
)

cdm$acute_mi <- conceptCohort(
  cdm = cdm,
  conceptSet = acute_mi_cl,
  name = "acute_mi"
)


# get beta blockers

drugs_cl <- importCodelist(here("Cohorts", "drugs"), type = "csv")


cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "cardio_drugs",
  conceptSet = drugs_cl,
  gapEra = 7
)

cdm$cardio_drugs <- cdm$cardio_drugs |>
  requireIsFirstDrugEntry() |>
  requireTableIntersect(
    tableName = "acute_mi",
    window = c(-30, 0),
    intersections = c(1,Inf)
  ) |>
  requireInDateRange(study_period) |>
  requireAge(ageRange = c(18,150)) |>
  requirePriorObservation(minPriorObservation = 365)


drug_count <- cohortCount(cdm$cardio_drugs) |>
  filter(number_subjects >= min_cell_count)

cdm$cardio_drugs |>
  subsetCohorts(cohortId = drug_count$cohort_definition_id)

### Cohort Code Use

if(isTRUE(run_code_use)){

# code use here
  
}

