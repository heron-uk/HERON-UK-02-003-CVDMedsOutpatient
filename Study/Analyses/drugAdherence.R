cdm$mi_drugs_ad <- cdm$mi_drugs_final |>
  PatientProfiles::addDemographics(
    ageGroup = list(c(18, 49), 
                    c(50, 59),
                    c(60, 69),
                    c(70, 79),
                    c(80, 89),
                    c(90, 150)),
    name = "mi_drugs_ad"
  )

cdm$stroke_drugs_ad <- cdm$stroke_drugs_final |>
  PatientProfiles::addDemographics(
    ageGroup = list(c(18, 49), 
                    c(50, 59),
                    c(60, 69),
                    c(70, 79),
                    c(80, 89),
                    c(90, 150)),
    name = "stroke_drugs_ad"
  )

# PPC

results[["ppc_mi"]] <- cdm$mi_drugs_ad |>
  summariseProportionOfPatientsCovered(followUpDays = 1825,
                                       strata = list(c("age_group"), c("sex"), c("age_group", "sex")))

results[["ppc_stroke"]] <- cdm$stroke_drugs_ad |>
  summariseProportionOfPatientsCovered(followUpDays = 1825,
                                       strata = list(c("age_group"), c("sex"), c("age_group", "sex")))


