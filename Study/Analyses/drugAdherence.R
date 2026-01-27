ad_7 <- list()

set_7 <- settings(cdm$cardio_drugs_7)

cdm$cardio_drugs_7_ad <- cdm$cardio_drugs_7 |>
  PatientProfiles::addDemographics(
    ageGroup = list(c(18, 64), c(65, 150)),
    name = "cardio_drugs_7_ad"
  )

for(i in set_7$cohort_definition_id){
ad_7[[i]] <- estimateSingleEventSurvival(
  cdm = cdm,
  strata = list(c("age_group"), c("sex")),
  targetCohortTable = "cardio_drugs_7_ad",
  outcomeCohortTable = "cardio_drugs_7_ad",
  targetCohortId = i,
  outcomeCohortId = i,
  outcomeDateVariable = "cohort_end_date",
  outcomeWashout = Inf,
  followUpDays = 730,
  eventGap = 30
)

}

results[["adherence_7"]] <- omopgenerics::bind(ad_7)

###
# ad_14 <- list()
# 
# set_14 <- settings(cdm$cardio_drugs_14)
# 
# cdm$cardio_drugs_14_ad <- cdm$cardio_drugs_14 |>
#   PatientProfiles::addDemographics(
#     ageGroup = list(c(18, 64), c(65, 150)),
#     name = "cardio_drugs_14_ad"
#   )
# 
# for(i in set_14$cohort_definition_id){
#   ad_14[[i]] <- estimateSingleEventSurvival(
#     cdm = cdm,
#     strata = list(c("age_group"), c("sex")),
#     targetCohortTable = "cardio_drugs_14_ad",
#     outcomeCohortTable = "cardio_drugs_14_ad",
#     targetCohortId = i,
#     outcomeCohortId = i,
#     outcomeDateVariable = "cohort_end_date",
#     outcomeWashout = Inf,
#     followUpDays = 730,
#     eventGap = 30
#   )
#   
# }
# 
# results[["adherence_14"]] <- omopgenerics::bind(ad_14)

# PPC

results[["ppc_7"]] <- cdm$cardio_drugs_7_ad |>
  summariseProportionOfPatientsCovered(followUpDays = 730,
                                       strata = list(c("age_group"), c("sex")))
