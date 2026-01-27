
drug_codes <- omopgenerics::importCodelist(here::here("Cohorts", "drugs"), 
                                           type = "csv")
cdm[["drugs"]] <- conceptCohort(cdm,
                                conceptSet = drug_codes,
                                table = "drug_exposure",
                                exit = "event_end_date",
                                name = "drugs") 

condition_codes <- omopgenerics::importCodelist(here::here("Cohorts", "conditions"), 
                                           type = "csv")
cdm[["conditions"]] <- conceptCohort(cdm,
                                conceptSet = condition_codes,
                                exit = "event_start_date",
                                name = "conditions") 
cdm[["conditions"]] <- cdm[["conditions"]] |> 
  exitAtObservationEnd()

cdm <- bind(cdm[["drugs"]],
            cdm[["conditions"]],
            name = "study_cohorts")
