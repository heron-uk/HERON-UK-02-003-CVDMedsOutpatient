# Create outcome cohorts
logMessage("INSTANTIATING MI COHORT")

acute_mi_cl <- CodelistGenerator::importCodelist(
  path = here::here("Cohorts", "conditions", "acute_mi.csv"),
  type = "csv"
)

acute_mi_cl <- acute_mi_cl |>
  addConcepts(cdm = cdm,
              concepts = c(4198141, 4121477, 4119953, 4096808, 4108220),
              codelistName = "acute_mi")

cdm$acute_mi <- conceptCohort(
  cdm = cdm,
  conceptSet = acute_mi_cl,
  name = "acute_mi"
) |> 
  collapseCohorts(gap = 28) 

cdm$acute_mi_first <- cdm$acute_mi |>
  requireIsFirstEntry(
    name = "acute_mi_first"
  ) |> 
  requireDemographics(
    ageRange = c(18, 150),
    minPriorObservation = 365,
    atFirst = TRUE
  ) |>
  requireInDateRange(study_period)
  
cdm$acute_mi_second <- cdm$acute_mi |>
  requireIsEntry(
    entryRange = c(2,2),
    name = "acute_mi_second"
  ) |> 
  requireDemographics(
    ageRange = c(18, 150),
    minPriorObservation = 365,
    atFirst = TRUE
  ) |>
  requireInDateRange(study_period)

logMessage("INSTANTIATED MI COHORT")

logMessage("INSTANTIATING HEART FAILURE COHORT")
hf_cl <- CodelistGenerator::importCodelist(
  path = here::here("Cohorts", "conditions", "heart_failure.csv"),
  type = "csv"
)

cdm$heart_failure <- conceptCohort(
  cdm = cdm,
  conceptSet = hf_cl,
  name = "heart_failure"
)

logMessage("INSTANTIATED HEART FAILURE COHORT")

logMessage("INSTANTIATE OBESITY COHORTS")

obesity_diag <- list(obesity = c(
  604591, 4271317, 4171972,  4270189, 4079899,  4235799,
  4087487,  40481140, 36713437,  36678790,  45763687,  4097929,  4097996,  4182506,
  4100857,  4160821,  4029277,  4029276,  37166819,  4029900,  36717154,  4005991,
  4163032,  4185912,  4171147,  4177337,  4220527,  4203289,  35622038,  36674490,
  36674893,  4171317,  438731,  37208175,  37164247,  42872398,  4216214,  36716144,
  37110069,  434005,  37395980,  433736,  4212443,  4215969,  4189665,  36716555,
  36717199,  37204685,  37206117,  37397209,
  37162364,  36716151,  37204815,  37311904,  45757112,  4183240,
  4093860,  37163354, 36674827,  3199162,
  45771307,  36676689,  37204691,  37018860,  42539192,  37164244,
  4217557,  37166818,  4211019,  36714072, 36714548,  37165655
))
cdm$obesity <- conceptCohort(
  cdm = cdm, conceptSet = obesity_diag, exit = "event_start_date", name = "obesity"
)

cdm$obesity_bmi <- measurementCohort(
  cdm = cdm, 
  conceptSet = list("bmi_measurement" = c(3038553, 36304833)), 
  valueAsNumber = list("bmi_measurement" = list(c(30, 60))),   
  name = "obesity_bmi"
)
# body weight cohort
cdm$obesity_body_weight <- measurementCohort(
  cdm = cdm, conceptSet = list("body_weight"= c(3025315, 4099154, 3013762,
                                                3023166, 3027492)), 
  valueAsNumber = list("body_weight"= list("9529" = c(120, 200), 
                                           "3195625" = c(265, 440))),
  name = "obesity_body_weight"
)
# bind and union
cdm <- omopgenerics::bind(cdm$obesity, 
                          cdm$obesity_bmi, 
                          cdm$obesity_body_weight, 
                          name = "obesity")
cdm$obesity <- cdm$obesity |>
  unionCohorts(cohortName = "obesity") |> 
  exitAtObservationEnd()

logMessage("INSTANTIATED OBESITY COHORTS")
