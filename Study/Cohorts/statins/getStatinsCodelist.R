statins <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("atorvastatin", "rosuvastatin", "simvastatin", 
           "pravastatin", "fluvastatin"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  unionCodelists()

names(statins) <- "statins"

exportCodelist(statins, path = here::here("Cohorts", "statins"),
               type = "csv")
