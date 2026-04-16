mi_cohorts <- settings(cdm$mi_drugs_first) |>
  pull(cohort_name)


for(coh in mi_cohorts){
  name = paste0("crm_", coh)
cdm[[name]] <- cdm$acute_mi_first|>
  PatientProfiles::addCohortIntersectDays(
    targetCohortTable = "mi_drugs_first",
    targetCohortId = paste0(coh),
    targetDate = "cohort_start_date",
    nameStyle = "days_to_treatment",
    order = "first",
    window = list(c(0, 28))
  ) |>
  PatientProfiles::addDeathDays() |>
  PatientProfiles::addFutureObservation() |>
  compute(name = name, temporary = FALSE) |>
  renameCohort(newCohortName = name) 
}

cdm <- omopgenerics::bind(
  cdm$crm_antihypertensive_mi,
  cdm$crm_acei_arbs_mi,
  cdm$crm_aspirin_mi,
  cdm$crm_beta_blockers_mi,
  cdm$crm_calcium_channel_blockers_mi,
  cdm$crm_dipyridamole_mi,
  cdm$crm_doacs_mi,
  cdm$crm_ezetimibe_mi,
  cdm$crm_p2y12_inhibitors_mi,
  cdm$crm_pcsk9_inhibitors_mi,
  cdm$crm_statin_mi,
  cdm$crm_thiazide_diuretics_mi,
  cdm$crm_warfarin_mi,
  cdm$crm_beta_blockers_hf,
  cdm$crm_beta_blockers_no_hf,
  cdm$crm_antiplatelets_mi,
  cdm$crm_dual_antiplatelet_mi,
  cdm$crm_anticoagulants_mi,
  cdm$crm_anticoagulants_mi_af,
  cdm$crm_lipid_lowering_mi,
  name = "mi_crm"
)

if(db_name == "GOLD" | db_name == "GOLD_100k"){
cdm$mi_crm <- cdm$mi_crm |>
  addDemographics(
    sex = TRUE,
    age = FALSE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "mi_crm",
    ageGroup = list(
      "18 to 39" = c(18, 39),
      "40 to 49" = c(40, 49),
      "50 to 59" = c(50, 59),
      "60 to 69" = c(60, 69),
      "70 to 79" = c(70, 79),
      "80 to 89" = c(80, 89),
      "90+" = c(90, 150))
  ) |>
  addSES() |>
  addCountry()

x_mi <- cdm$mi_crm |> 
  addCohortName() |>
  collect() |>
  mutate(
    days_to_treatment = coalesce(days_to_treatment, 9999),
    days_to_death = coalesce(days_to_death, 9999),
    future_observation = pmin(days_to_death, future_observation)
  ) |>
  mutate(
    time = pmin(days_to_treatment, days_to_death, future_observation, na.rm = TRUE),
    event = case_when(
      days_to_treatment <= days_to_death & days_to_treatment <= future_observation ~ "treatment",
      days_to_death <= days_to_treatment & days_to_death <= future_observation ~ "death",
      TRUE ~ "censor"
    ),
    event = factor(event, levels = c("censor", "treatment", "death"))) |>
  mutate(
    sex = factor(sex),
    age_group = factor(age_group),
    ses = factor(ses),
    country = factor(country)
  )

} else {
  cdm$mi_crm <- cdm$mi_crm |>
    addDemographics(
      sex = TRUE,
      age = FALSE,
      priorObservation = FALSE,
      futureObservation = FALSE,
      name = "mi_crm",
      ageGroup = list(
        "18 to 39" = c(18, 39),
        "40 to 49" = c(40, 49),
        "50 to 59" = c(50, 59),
        "60 to 69" = c(60, 69),
        "70 to 79" = c(70, 79),
        "80 to 89" = c(80, 89),
        "90+" = c(90, 150))
    ) |>
    addSES() 

x_mi <- cdm$mi_crm |> 
  addCohortName() |>
  collect() |>
  mutate(
    days_to_treatment = coalesce(days_to_treatment, 9999),
    days_to_death = coalesce(days_to_death, 9999),
    future_observation = pmin(days_to_death, future_observation)
  ) |>
  mutate(
    time = pmin(days_to_treatment, days_to_death, future_observation, na.rm = TRUE),
    event = case_when(
      days_to_treatment <= days_to_death & days_to_treatment <= future_observation ~ "treatment",
      days_to_death <= days_to_treatment & days_to_death <= future_observation ~ "death",
      TRUE ~ "censor"
    ),
    event = factor(event, levels = c("censor", "treatment", "death"))) |>
  mutate(
    sex = factor(sex),
    age_group = factor(age_group),
    ses = factor(ses)
  )
}

#### Stroke

stroke_cohorts <- settings(cdm$stroke_drugs_first) |>
  filter(!cohort_name %in% c("beta_blockers_hf",
                             "beta_blockers_no_hf")) |>
  pull(cohort_name)


for(coh in stroke_cohorts){
  name = paste0("crm_", coh)
  cdm[[name]] <- cdm$stroke_first |>
    PatientProfiles::addCohortIntersectDays(
      targetCohortTable = "stroke_drugs_first",
      targetCohortId = coh,
      targetDate = "cohort_start_date",
      nameStyle = "days_to_treatment",
      order = "first",
      window = list(c(0, 28))
    ) |>
    PatientProfiles::addDeathDays() |>
    PatientProfiles::addFutureObservation() |>
    compute(name = name, temporary = FALSE) |>
    renameCohort(newCohortName = name) 
}

cdm <- omopgenerics::bind(
  cdm$crm_antihypertensive_stroke,
  cdm$crm_acei_arbs_stroke,
  cdm$crm_aspirin_stroke,
  cdm$crm_beta_blockers_stroke,
  cdm$crm_calcium_channel_blockers_stroke,
  cdm$crm_dipyridamole_stroke,
  cdm$crm_doacs_stroke,
  cdm$crm_ezetimibe_mi,
  cdm$crm_p2y12_inhibitors_mi,
  cdm$crm_pcsk9_inhibitors_mi,
  cdm$crm_statin_stroke,
  cdm$crm_thiazide_diuretics_stroke,
  cdm$crm_warfarin_stroke,
  cdm$crm_antiplatelets_stroke,
  cdm$crm_dual_antiplatelet_stroke,
  cdm$crm_anticoagulants_stroke,
  cdm$crm_anticoagulants_stroke_af,
  cdm$crm_lipid_lowering_stroke,
  name = "stroke_crm"
)

if(db_name == "GOLD" | db_name == "GOLD_100k"){
cdm$stroke_crm <- cdm$stroke_crm |>
  addDemographics(
    sex = TRUE,
    age = FALSE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "stroke_crm",
    ageGroup = list(
      "18 to 39" = c(18, 39),
      "40 to 49" = c(40, 49),
      "50 to 59" = c(50, 59),
      "60 to 69" = c(60, 69),
      "70 to 79" = c(70, 79),
      "80 to 89" = c(80, 89),
      "90+" = c(90, 150))
  ) |>
  addSES() |>
  addCountry()

x_stroke <- cdm$stroke_crm |> 
  addCohortName() |>
  collect() |>
  mutate(
    days_to_treatment = coalesce(days_to_treatment, 9999),
    days_to_death = coalesce(days_to_death, 9999),
    future_observation = pmin(days_to_death, future_observation)
  ) |>
  mutate(
    time = pmin(days_to_treatment, days_to_death, future_observation, na.rm = TRUE),
    event = case_when(
    days_to_treatment <= days_to_death & days_to_treatment <= future_observation ~ "treatment",
    days_to_death <= days_to_treatment & days_to_death <= future_observation ~ "death",
    TRUE ~ "censor"
  ),
  event = factor(event, levels = c("censor", "treatment", "death"))) |>
  mutate(
    sex = factor(sex),
    age_group = factor(age_group),
    ses = factor(ses),
    country = factor(country)
  )

} else {
  cdm$stroke_crm <- cdm$stroke_crm |>
    addDemographics(
      sex = TRUE,
      age = FALSE,
      priorObservation = FALSE,
      futureObservation = FALSE,
      name = "stroke_crm",
      ageGroup = list(
        "18 to 39" = c(18, 39),
        "40 to 49" = c(40, 49),
        "50 to 59" = c(50, 59),
        "60 to 69" = c(60, 69),
        "70 to 79" = c(70, 79),
        "80 to 89" = c(80, 89),
        "90+" = c(90, 150))
    )

x_stroke <- cdm$stroke_crm |> 
  addCohortName() |>
  collect() |>
  mutate(
    days_to_treatment = coalesce(days_to_treatment, 9999),
    days_to_death = coalesce(days_to_death, 9999),
    future_observation = pmin(days_to_death, future_observation)
  ) |>
  mutate(
    time = pmin(days_to_treatment, days_to_death, future_observation, na.rm = TRUE),
    event = case_when(
      days_to_treatment <= days_to_death & days_to_treatment <= future_observation ~ "treatment",
      days_to_death <= days_to_treatment & days_to_death <= future_observation ~ "death",
      TRUE ~ "censor"
    ),
    event = factor(event, levels = c("censor", "treatment", "death"))) |>
  mutate(
    sex = factor(sex),
    age_group = factor(age_group),
    ses = factor(ses)
  )
}

x <- rbind(x_mi, x_stroke)

### CR Model

crm_results <- list()

cohorts <- unique(x$cohort_name)

for(coh in cohorts){
  
msdata <- x |>
    filter(cohort_name == coh) |>
  mutate(
    cohort_name = coh,
    sex = relevel(factor(sex), ref = "Female"),
    age_group = relevel(factor(age_group), ref = "50 to 59"),
    ses = relevel(factor(ses), ref = "5")
  )

event_count <- length(unique(msdata$event))

if(event_count < 3){
  cli::cli_alert_info("Missing event for cohort {.pkg {coh}}. Skipping adjusted CR model.")
} else {

cli::cli_inform(c(i = "Fitting CR model for {.pkg {coh}}"))

cif <- cuminc(Surv(time, event) ~ 1, data = msdata)

cif_df <- cif |>
  broom::tidy() |>
  filter(time <= 28) |>
  select(time, outcome, estimate) |>
  pivot_wider(names_from = outcome, values_from = estimate, values_fill = 0) |>
  pivot_longer(
    cols = c(treatment, death),
    names_to = "state",
    values_to = "prob"
  ) |>
  mutate(cohort_name = coh,
         result_type = "crm_probabilities")

cif_sr <- omopgenerics::transformToSummarisedResult(
  x = cif_df,
  group = c("cohort_name"),
  estimates = c("prob"),
  additional = c("time", "state"),
  settings = c("result_type")
) |>
  mutate(cdm_name = omopgenerics::cdmName(cdm))

crm_results[[paste0("prob_",coh)]] <- cif_sr
###

sex_count <- length(unique(msdata$sex))
ses_count <- length(unique(msdata$ses))
age_group_count <- length(unique(msdata$age_group))

if (sex_count < 2 & ses_count < 2 & age_group_count < 2 ) {
  cli::cli_alert_info("Insufficient levels in strata for cohort {.pkg {coh}}. Skipping adjusted CR model.")
} else {
  
cr_model <- crr(
  Surv(time, event) ~ age_group + sex + ses,
  data = msdata,
  failcode = "treatment"
)

tidy_adj_res <- tidy(cr_model) |>
  select("variable_level" = "term", "coef" = "estimate", "se" = "std.error") |>
  mutate(
    cohort_name = coh,
    cdm_name = omopgenerics::cdmName(cdm),
    variable_name = "Competing risk coefficients",
    result_type = "crm_coefficients"
  ) |>
  omopgenerics::transformToSummarisedResult(
    group = c("cohort_name"),
    estimates = c("coef", "se"),
    settings = c("result_type")
  )

crm_results[[paste0("coef_",coh, "_mi")]] <- tidy_adj_res

}
}
}

results[["crm"]] <- omopgenerics::bind(crm_results)

### CRM by Country

### CR Model

if(db_name == "GOLD" | db_name == "GOLD_100k"){

crm_results_country <- list()

cohorts <- unique(x$cohort_name)
country <- unique(x$country)

for(coh in cohorts){
  for(cou in country){
  
  msdata <- x |>
    filter(cohort_name == coh,
           country == cou) |>
    mutate(
      sex = relevel(factor(sex), ref = "Female"),
      age_group = relevel(factor(age_group), ref = "50 to 59"),
      ses = relevel(factor(ses), ref = "5"),
      cohort_name = paste0(coh, "_", cou)
    )
  
  event_count <- length(unique(msdata$event))
  if(event_count < 3){
    cli::cli_alert_info("Missing event for cohort {.pkg {coh} {cou}}. Skipping adjusted CR model.")
  } else {
  
  cli::cli_inform(c(i = "Fitting CR model for {.pkg {coh} {cou}}"))
  
  cif <- cuminc(Surv(time, event) ~ 1, data = msdata)
  
  cif_df <- cif |>
    broom::tidy() |>
    filter(time <= 28) |>
    select(time, outcome, estimate) |>
    pivot_wider(names_from = outcome, values_from = estimate, values_fill = 0) |>
    pivot_longer(
      cols = c(treatment, death),
      names_to = "state",
      values_to = "prob"
    ) |>
    mutate(cohort_name = paste0(coh, "_", cou),
           result_type = "crm_probabilities_by_country")
  
  cif_sr <- omopgenerics::transformToSummarisedResult(
    x = cif_df,
    group = c("cohort_name"),
    estimates = c("prob"),
    additional = c("time", "state"),
    settings = c("result_type")
  ) |>
    mutate(cdm_name = omopgenerics::cdmName(cdm))
  
  crm_results_country[[paste0("crm_prob_",coh, "_", cou)]] <- cif_sr
  ###
  
  sex_count <- length(unique(msdata$sex))
  ses_count <- length(unique(msdata$ses))
  age_group_count <- length(unique(msdata$age_group))
  
  if (sex_count < 2 & ses_count < 2 & age_group_count < 2 ) {
    cli::cli_alert_info("Insufficient levels in strata for cohort {.pkg {coh}}. Skipping adjusted CR model.")
  } else {
    
    cr_model <- crr(
      Surv(time, event) ~ age_group + sex + ses,
      data = msdata,
      failcode = "treatment"
    )
    
    tidy_adj_res <- tidy(cr_model) |>
      select("variable_level" = "term", "coef" = "estimate", "se" = "std.error") |>
      mutate(
        cohort_name = paste0(coh, "_", cou),
        cdm_name = omopgenerics::cdmName(cdm),
        variable_name = "Competing risk coefficients",
        result_type = "crm_coefficients_by_country"
      ) |>
      omopgenerics::transformToSummarisedResult(
        group = c("cohort_name"),
        estimates = c("coef", "se"),
        settings = c("result_type")
      )
    
    crm_results_country[[paste0("coef_",coh,"_",cou)]] <- tidy_adj_res
  }
  }
  }
}

results[["crm_by_country"]] <- omopgenerics::bind(crm_results_country)
}
