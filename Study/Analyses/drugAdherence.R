ad_365 <- list()
ad_730 <- list()

set <- settings(cdm$cardio_drugs)

cdm$cardio_drugs_ad <- cdm$cardio_drugs |>
  PatientProfiles::addDemographics(
    ageGroup = list(c(18, 64), c(65, 150)),
    name = "cardio_drugs_ad"
  )

for(i in set$cohort_definition_id){
ad_365[[i]] <- estimateSingleEventSurvival(
  cdm = cdm,
  strata = list(c("age_group"), c("sex")),
  targetCohortTable = "cardio_drugs_ad",
  outcomeCohortTable = "cardio_drugs_ad",
  targetCohortId = i,
  outcomeCohortId = i,
  outcomeDateVariable = "cohort_end_date",
  outcomeWashout = Inf,
  followUpDays = 365,
  eventGap = 7
)

ad_730[[i]] <- estimateSingleEventSurvival(
  cdm = cdm,
  strata = list(c("age_group"), c("sex")),
  targetCohortTable = "cardio_drugs_ad",
  outcomeCohortTable = "cardio_drugs_ad",
  targetCohortId = i,
  outcomeCohortId = i,
  outcomeDateVariable = "cohort_end_date",
  outcomeWashout = Inf,
  followUpDays = 730,
  eventGap = 7
)

}

results[["adherence_365"]] <- omopgenerics::bind(ad_365)

results[["adherence_730"]] <- omopgenerics::bind(ad_730)

# PPC
results[["ppc_365"]] <- cdm$cardio_drugs_ad |>
summariseProportionOfPatientsCovered(followUpDays = 365,
                                     strata = list(c("age_group"), c("sex")))

results[["ppc_730"]] <- cdm$cardio_drugs_ad |>
  summariseProportionOfPatientsCovered(followUpDays = 730,
                                       strata = list(c("age_group"), c("sex")))
