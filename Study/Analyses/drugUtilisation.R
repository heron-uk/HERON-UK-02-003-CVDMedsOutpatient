## get drug ingredients
drug_ingredient_codes <- cdm$concept %>%
  filter(domain_id == "Drug") %>%
  filter(concept_class_id == "Ingredient") %>%
  filter(standard_concept == "S") %>%
  select(c("concept_id", "concept_name")) %>%
  rename(
    drug_name = concept_name
  ) %>%
  collect()
  
drugs_cl <- importCodelist(here("Cohorts", "drugs"), type = "csv")

drugs_det <- asCodelistWithDetails(drugs_cl, cdm) 

drugs_det <- do.call(rbind, drugs_det)

drug_ing <- drugs_det |>
  filter(concept_id %in% drug_ingredient_codes$concept_id) |>
  distinct() |>
  pull(concept_id)

results[["drug_utilisation"]] <- summariseDrugUtilisation(
  cohort = cdm$cardio_drugs,
  ingredientConceptId = drug_ing,
  gapEra = 7,
  numberExposures = TRUE,
  numberEras = TRUE,
  daysExposed = TRUE,
  daysPrescribed = TRUE,
  timeToExposure = TRUE,
  initialExposureDuration = TRUE,
  initialQuantity = TRUE,
  cumulativeQuantity = TRUE,
  initialDailyDose = TRUE,
  cumulativeDose = TRUE
)

