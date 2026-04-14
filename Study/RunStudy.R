results_folder <- here("Results", cdmName(cdm))
if (!file.exists(results_folder)) {
  dir.create(results_folder, recursive = TRUE)
}
results <- list()
createLogFile(logFile = file.path(results_folder, "log_{date}_{time}"))

maxObsEnd <- cdm$observation_period |>
  summarise(maxObsEnd = max(observation_period_end_date, na.rm = TRUE)) |>
  dplyr::pull()

study_period <- c(as.Date(study_start), as.Date(maxObsEnd))

cdm$person <- cdm$person |>
  filter(
    !is.na(gender_concept_id),
    !is.na(year_of_birth),
    gender_concept_id %in% c(8507,8532)
  )

if(db_name == "GOLD" | db_name == "GOLD_100k"){
  location <- cdm$location |>
    collect() |>
    filter(country_source_value %in% c("Wales", "Scotland", "Northern Ireland"))

  care_site <- cdm$care_site |>
    collect() |>
    filter(location_id %in% location$location_id)

  persons <- cdm$person |>
    filter(care_site_id %in% care_site$care_site_id) |>
    distinct(person_id) |>
    pull(person_id)

  persons <- bit64::as.integer64(persons)

  cdm <- cdmSubset(cdm, personId = persons)
}

if(db_name == "GOLD" | db_name == "GOLD_100k"){
  source(here("Analyses", "functionsGOLD.R"))
}

# Load functions
source(here("Analyses","functions.R"))

if(db_name == "DataLoch") {
  cdm$drug_exposure <- cdm$drug_exposure |>
    filter(drug_type_concept_id == 32839)
}

# create and export snapshot
logMessage("RETRIEVING SNAPSHOT")
cli::cli_text("- GETTING CDM SNAPSHOT ({Sys.time()})")

results[["snap"]] <- summariseOmopSnapshot(cdm)

logMessage("SNAPSHOT COMPLETED")

# summarise observation periods
logMessage("RETRIEVING OBSERVATION PERIOD SUMMARY")
results[["observation_period"]] <- summariseObservationPeriod(cdm$observation_period)
logMessage("OBSERVATION PERIOD SUMMARY COMPLETED")

logMessage("INSTANTIATING OUTCOME COHORTS")
source(here("Cohorts","InstantiateOutcomeCohorts.R"))
logMessage("INSTANTIATED OUTCOME COHORTS")

logMessage("INSTANTIATING PRIMARY CARE COHORTS")
source(here("Cohorts","Primary", "InstantiateMIDrugCohorts.R"))
source(here("Cohorts","Primary", "InstantiateStrokeDrugCohorts.R"))
logMessage("PRIMARY CARE COHORTS INSTANTIATED")

logMessage("RUN DRUG ADHERENCE")
source(here("Analyses", "competingRisk.R"))
source(here("Analyses", "multistate.R"))
logMessage("DRUG ADHERENCE FINISHED")

logMessage("RUN SUMMARISE CHARACTERISTICS")
source(here("Analyses", "characteristics.R"))
logMessage("SUMMARISE CHARACTERISTICS FINISHED")

# export results ----
result <- omopgenerics::bind(results)

omopgenerics::exportSummarisedResult(
  result,
  minCellCount = min_cell_count,
  path = results_folder,
  fileName = "primary_results_{cdm_name}_{date}.csv"
)
