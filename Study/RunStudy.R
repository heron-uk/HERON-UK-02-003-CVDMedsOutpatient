results_folder <- here("Results", cdmName(cdm))
if (!file.exists(results_folder)) {
  dir.create(results_folder, recursive = TRUE)
}
mi_results <- list()
stroke_results <- list()
logger_name <- gsub(":| |-", "_", paste0("log_01_001_", Sys.time(), ".txt"))
logger <- create.logger()
logfile(logger) <- here(results_folder, logger_name)
level(logger) <- "INFO"
info(logger, "LOG CREATED")

maxObsEnd <- cdm$observation_period |>
  summarise(maxObsEnd = max(observation_period_end_date, na.rm = TRUE)) |>
  dplyr::pull()

study_period <- c(as.Date(study_start), as.Date(maxObsEnd))

if(isTRUE(hospital_care) & isFALSE(primary_care)){
  cdm <- OmopConstructor::buildObservationPeriod(
    cdm,
    collapseDays = 545,
    persistenceDays = 545,
    dateRange = study_period,
    censorAge = 150L,
    recordsFrom = c("visit_occurrence", "condition_occurrence", "drug_exposure")
  )
}

cdm$person <- cdm$person |>
  filter(
    !is.na(gender_concept_id),
    !is.na(year_of_birth),
    gender_concept_id %in% c(8507,8532)
  )

# create and export snapshot
info(logger, "RETRIEVING SNAPSHOT")
cli::cli_text("- GETTING CDM SNAPSHOT ({Sys.time()})")

snap <- summariseOmopSnapshot(cdm)

mi_results[["snap"]] <- snap
stroke_results[["snap"]] <- snap

info(logger, "SNAPSHOT COMPLETED")

# summarise observation periods
info(logger, "RETRIEVING OBSERVATION PERIOD SUMMARY")
cli::cli_text("- GETTING OBSERVATION PERIOD SUMMARY ({Sys.time()})")

obs <- summariseObservationPeriod(cdm$observation_period)

mi_results[["observation_period"]] <- obs
stroke_results[["observation_period"]] <- obs

info(logger, "OBSERVATION PERIOD SUMMARY COMPLETED")

if(isTRUE(hospital_care)){
  info(logger, "INSTANTIATING HOSPITAL COHORTS")
  source(here("Cohorts","Primary", "InstantiateOutcomeCohorts.R"))
  source(here("Cohorts", "InstantiateHospitalCohorts.R"))
  info(logger, "HOSPITAL COHORTS INSTANTIATED")
  
} 

if(isTRUE(primary_care)){
info(logger, "INSTANTIATING PRIMARY CARE COHORTS")
source(here("Cohorts","Primary", "InstantiateOutcomeCohorts.R"))
source(here("Cohorts","Primary", "InstantiateMIDrugCohorts.R"))
source(here("Cohorts","Primary", "InstantiateStrokeDrugCohorts.R"))
info(logger, "PRIMARY CARE COHORTS INSTANTIATED")
}

if(isTRUE(run_drug_adherence) & isTRUE(primary_care)){
info(logger, "RUN DRUG ADHERENCE")
source(here("Analyses", "drugAdherence.R"))
info(logger, "DRUG ADHERENCE FINISHED")
}

if(isTRUE(run_characteristics)){
info(logger, "RUN SUMMARISE CHARACTERISTICS")
source(here("Analyses", "characteristics.R"))
info(logger, "SUMMARISE CHARACTERISTICS FINISHED")
}

# export results ----
info(logger, "EXPORTING RESULTS")

mi_result <- omopgenerics::bind(mi_results)
stroke_result <- omopgenerics::bind(stroke_results)

omopgenerics::exportSummarisedResult(mi_result,
                                     minCellCount = min_cell_count,
                                     path = results_folder,
                                     fileName = "results_mi_{cdm_name}_{date}.csv"
)

omopgenerics::exportSummarisedResult(stroke_result,
                                     minCellCount = min_cell_count,
                                     path = results_folder,
                                     fileName = "results_stroke_{cdm_name}_{date}.csv"
)


info(logger, "RESULTS EXPORTED")

info(logger, "STUDY CODE FINISHED")

