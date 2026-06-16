# aspirin -------
aspirin <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("aspirin"),
  nameStyle = "{concept_name}",
  type = "codelist"
) |>
  asCodelist()

exportCodelist(aspirin,
               path = "Cohorts/dual_antiplatelets/codelists",
               type = "csv")


# P2Y12 inhibitors -----
p2y12_inhibitors <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("clopidogrel", "ticagrelor",
           "prasugrel", "cangrelor"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  asCodelist() |>
  unionCodelists()

names(p2y12_inhibitors) <- "p2y12_inhibitors"

exportCodelist(p2y12_inhibitors,
               path = "Cohorts/dual_antiplatelets/codelists",
               type = "csv")

# Dipyridamole

dipyridamole <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("dipyridamole"),
  nameStyle = "{concept_name}",
  type = "codelist"
) |>
  asCodelist()


exportCodelist(dipyridamole,
               path = "Cohorts/dual_antiplatelets/codelists",
               type = "csv")

####

