# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_observation_period = list(result_type = "summarise_observation_period", observation_period_ordinal = "all"),
  cohort_code_use = list(result_type = "cohort_code_use"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_demographics = list(result_type = "summarise_characteristics", variable_name = c("Number records", "Number subjects", "Cohort start date", "Cohort end date", "Ses", "Ethnicity", "Age", "Age group", "Sex", "Prior observation", "Future observation", "Days in cohort", "Days to next record", "Prior ischemic stroke (-30 to -1)", "Prior mi (-30 to -1)")),
  summarise_treatments = list(result_type = "summarise_characteristics", variable_name = "Drug treatment (0, 14)"),
  summarise_procedures = list(result_type = "summarise_characteristics", variable_name = "Procedures (0, 14)")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "rawData"))
data <- prepareResult(result, resultList)

data$summarise_treatments <- data$summarise_treatments |>
  omopgenerics::splitStrata() |>
  dplyr::mutate(strata = dplyr::case_when(
    age_group != "overall" ~ paste0("Age group: ", age_group),
    ses != "overall" ~ paste0("SES: ", ses),
    sex != "overall" ~ paste0("Sex: ", sex),
    .default = "overall"
  )) |>
  omopgenerics::uniteStrata("strata") |>
  dplyr::select(!c("age_group", "ses", "sex"))

data$summarise_procedures <- data$summarise_procedures |>
  omopgenerics::splitStrata() |>
  dplyr::mutate(strata = dplyr::case_when(
    age_group != "overall" ~ paste0("Age group: ", age_group),
    ses != "overall" ~ paste0("SES: ", ses),
    sex != "overall" ~ paste0("Sex: ", sex),
    .default = "overall"
  )) |>
  omopgenerics::uniteStrata("strata") |>
  dplyr::select(!c("age_group", "ses", "sex"))

values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)
