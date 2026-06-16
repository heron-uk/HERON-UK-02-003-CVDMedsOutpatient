beta_blockers <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("acebutolol", "alprenolol", "atenolol", 
           "bisoprolol", "carvedilol", 
           "metoprolol", "nadolol",
           "oxprenolol", "pindolol", 
           "propranolol", "timolol"),
  nameStyle = "{concept_name}",
  type = "codelist") 

exportCodelist(beta_blockers, here("Cohorts", "beta_blockers", "codelists"), type = "csv")
