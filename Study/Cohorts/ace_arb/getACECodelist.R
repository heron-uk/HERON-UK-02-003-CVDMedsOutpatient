acei_arbs <- getDrugIngredientCodes(
  cdm = cdm,
  name = c(# acei
    "captopril", "enalapril", "fosinopril", 
    "imidapril", "lisinopril", "perindopril", "quinapril", 
    "ramipril", "trandolapril",
    # arbs
    "azilsartan", "candesartan", "eprosartan", "irbesartan",        
    "losartan", "olmesartan", "telmisartan", "valsartan"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  unionCodelists()

names(acei_arbs) <- "acei_arbs"

exportCodelist(acei_arbs, path = here::here("Cohorts", "ace_arb"),
               type = "csv")
