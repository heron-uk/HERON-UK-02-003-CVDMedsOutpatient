# get cardiovascular drugs

card_drugs_cl <- importCodelist(here("Cohorts", "drugs"), type = "csv")

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm, 
  name = "cardiovascular_drugs",
  conceptSet = card_drugs_cl,
  gapEra = 7
)

cdm$cardiovascular_drugs <- cdm$cardiovascular_drugs |>
  requirePriorDrugWashout(days = 30) |>
  requireInDateRange(study_period)

# identify cohorts that meet the min_cell_count requirements
drug_count <- cohortCount(cdm$cardiovascular_drugs) |>
  filter(number_subjects > min_cell_count)

# restrict to these cohorts
cdm$cardiovascular_drugs |>
  subsetCohorts(cohortId = drug_count$cohort_definition_id)
