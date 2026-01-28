
drug_codes <- omopgenerics::importCodelist(here::here("Cohorts", "drugs"), 
                                           type = "csv")
# for diagnostics, only for high-level groups (not specific ingredients)
drug_codes <- drug_codes[stringr::str_detect(names(drug_codes), 
                                             "acei_arbs_|beta_blocker_|p2y12_inhibitors_|statin_", 
                                             negate = TRUE)]
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
