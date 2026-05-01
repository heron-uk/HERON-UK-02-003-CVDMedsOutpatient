# shiny is prepared to work with this resultList:
resultList <- list(
  cohort_code_use = list(result_type = "cohort_code_use"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_characteristics = list(result_type = "summarise_characteristics"),
  mms_probabilities = list(result_type = "mms_probabilities"),
  cox_coefficients = list(result_type = "cox_coefficients"),
  mms_probabilities_by_country = list(result_type = "mms_probabilities_by_country"),
  cox_coefficients_by_country = list(result_type = "cox_coefficients_by_country"),
  crm_probabilities = list(result_type = "crm_probabilities"),
  crm_coefficients = list(result_type = "crm_coefficients"),
  crm_probabilities_by_country = list(result_type = "crm_probabilities_by_country"),
  crm_coefficients_by_country = list(result_type = "crm_coefficients_by_country")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))
data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)
