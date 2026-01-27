# instanstiate using cohortconstructor
# no requirements at this stage

# conditions but not procedures
if (isTRUE(run_conditions)) {
condition_codes <- read_csv(here::here("Cohorts", "conditions", "concept_base_condition_cohorts.csv")) %>% 
                              group_by(cohort_name) %>%
                              summarise(concept_id = list(concept_id)) %>%
                              deframe() %>%
                              newCodelist() 

cdm[["cv_conditions"]] <- conceptCohort(cdm,
                                     conceptSet = condition_codes,
                                     exit = "event_start_date",
                                     name = "cv_conditions") 
}


# measurements
if(isTRUE(run_measurments)){
measurement_codes <- omopgenerics::importCodelist(here::here("Cohorts", "measurements"), type = "csv")

cdm[["measurements"]] <- conceptCohort(cdm,
                                     conceptSet = measurement_codes,
                                     exit = "event_start_date",
                                     name = "measurements") 
}


# drugs
if(isTRUE(run_drugs)){
drug_codes <- omopgenerics::importCodelist(here::here("Cohorts", "drugs"), type = "csv")

cdm[["drugs"]] <- conceptCohort(cdm,
                                conceptSet = drug_codes,
                                table = "drug_exposure",
                                exit = "event_end_date",
                                name = "drugs") 

}


# procedures on its own
if (isTRUE(run_procedures)) {
procedure_codes <- omopgenerics::importCodelist(here::here("Cohorts", "procedures"), type = "csv")

cdm[["cv_procedures"]] <- conceptCohort(cdm,
                                       conceptSet = procedure_codes,
                                       exit = "event_start_date",
                                       name = "cv_procedures") 

}

# conditions and procedures together
if (isTRUE(run_conditions) && isTRUE(run_procedures)) {
  # combine the conditions and procedure cohorts together and run
  cdm <- bind(cdm$cv_conditions,
              cdm$cv_procedures,
              name = "conditions_procedures"
  )
  
}

