# Create Acute MI Cohort
acute_mi_cl <- CodelistGenerator::codesFromCohort(
  path = here::here("Cohorts", "outcomes"),
  cdm = cdm
)

cdm$acute_mi <- conceptCohort(
  cdm = cdm,
  conceptSet = acute_mi_cl,
  name = "acute_mi"
)

cdm$acute_mi <- cdm$acute_mi |>
  requireIsFirstEntry() |> # first ever MI
  requireInDateRange(study_period) #recorded during study period

###

drugs_cl <- importCodelist(here("Cohorts", "drugs"), type = "csv")

cdm$cardio_drugs <- conceptCohort(
  cdm = cdm,
  conceptSet = drugs_cl,
  name = "cardio_drugs"
)


# collapse records that are within 7 days of each other
cdm$cardio_drugs_7 <- cdm$cardio_drugs |>
  collapseCohorts(gap = 7,
                  name = "cardio_drugs_7") 

cdm$cardio_drugs_7_mi_first <- cdm$cardio_drugs_7 |>
  requireCohortIntersect(
    targetCohortTable = "acute_mi",
    window = c(-30,0),
    name = "cardio_drugs_7_mi_first"
  ) |>
  requireIsFirstEntry() |>
  requireAge(ageRange = c(18,150))

cdm$cardio_drugs_7 <- cdm$cardio_drugs_7 |>
  inner_join(cdm$cardio_drugs_7_mi_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
             by = c("subject_id", "cohort_definition_id")) |>
  filter(cohort_start_date >= start_date) |>
  select(-c(start_date)) |>
  compute(name = "cardio_drugs_7", temporary = FALSE)

drug_count_7 <- cdm$cardio_drugs_7 |>
  collect() |>
  group_by(cohort_definition_id) |>
  distinct(subject_id) |>
  tally() |>
  filter(n >= 100)

cdm$cardio_drugs_7 |>
  subsetCohorts(cohortId = drug_count_7$cohort_definition_id,
                name = "cardio_drugs_7")


# collapse records that are within 14 days of each other
# cdm$cardio_drugs_14 <- cdm$cardio_drugs |>
#   collapseCohorts(gap = 14,
#                   name = "cardio_drugs_14") 
# 
# cdm$cardio_drugs_14_mi_first <- cdm$cardio_drugs_14 |>
#   requireCohortIntersect(
#     targetCohortTable = "acute_mi",
#     window = c(-30,0),
#     name = "cardio_drugs_14_mi_first"
#   ) |>
#   requireIsFirstEntry()  |>
#   requireAge(ageRange = c(18,150))
# 
# cdm$cardio_drugs_14 <- cdm$cardio_drugs_14 |>
#   inner_join(cdm$cardio_drugs_14_mi_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
#              by = c("subject_id", "cohort_definition_id")) |>
#   filter(cohort_start_date >= start_date) |>
#   select(-c(start_date))
# 
# drug_count_14 <- cdm$cardio_drugs_14 |>
#   collect() |>
#   group_by(cohort_definition_id) |>
#   distinct(subject_id) |>
#   tally() |>
#   filter(n >= 100)
# 
# cdm$cardio_drugs_14 |>
#   subsetCohorts(cohortId = drug_count_14$cohort_definition_id,
#                 name = "cardio_drugs_14")
  