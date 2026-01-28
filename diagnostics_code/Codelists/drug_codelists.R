
# beta blockers -------
beta_blockers <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("acebutolol", "alprenolol", "atenolol", 
           "bisoprolol", "carvedilol", "metoprolol", "nadolol",
           "oxprenolol", "pindolol", "propranolol", "timolol"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(beta_blockers) <- paste0("beta_blocker_", names(beta_blockers))

beta_blockers_all <- beta_blockers %>% 
  unionCodelists()
names(beta_blockers_all) <- "beta_blockers"

beta_blockers <- c(beta_blockers_all, beta_blockers) %>% 
  omopgenerics::newCodelist()

exportCodelist(beta_blockers, path = here::here("Cohorts", "drugs"),
               type = "csv")

# P2Y12 inhibitors -----
p2y12_inhibitors <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("clopidogrel", "ticagrelor", "prasugrel"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(p2y12_inhibitors) <- paste0("p2y12_inhibitors_", names(p2y12_inhibitors))


p2y12_inhibitors_all <- p2y12_inhibitors %>% 
  unionCodelists()
names(p2y12_inhibitors_all) <- "p2y12_inhibitors"

p2y12_inhibitors <- c(p2y12_inhibitors_all, p2y12_inhibitors) %>% 
  omopgenerics::newCodelist()

exportCodelist(p2y12_inhibitors, path = here::here("Cohorts", "drugs"),
               type = "csv")


# aspirin -------
aspirin <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("aspirin"),
  nameStyle = "{concept_name}",
  type = "codelist")
exportCodelist(aspirin, path = here::here("Cohorts", "drugs"),
               type = "csv")


# ACEi and ARBs -------
acei_arbs <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c(# acei
    "captopril", "cilazapril", 
    "enalapril", "fosinopril", "imidapril",    
    "lisinopril", "moexipril", "perindopril", "quinapril",
    "ramipril", "trandolapril",   
    # arbs
    "azilsartan", "candesartan", "eprosartan", "irbesartan",        
    "losartan", "olmesartan", "telmisartan", "valsartan"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(acei_arbs) <- paste0("acei_arbs_", names(acei_arbs))

acei_arbs_all <- acei_arbs %>% 
  unionCodelists()
names(acei_arbs_all) <- "acei_arbs"

acei_arbs <- c(acei_arbs_all, acei_arbs) %>% 
  omopgenerics::newCodelist()

exportCodelist(acei_arbs, path = here::here("Cohorts", "drugs"),
               type = "csv")

# statin -------
statin <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("atorvastatin", "rosuvastatin", "simvastatin", 
           "pravastatin", "Fluvastatin"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(statin) <- paste0("statin_", names(statin))

statin_all <- statin %>% 
  unionCodelists()
names(statin_all) <- "statin"

statin <- c(statin_all, statin) %>% 
  omopgenerics::newCodelist()

exportCodelist(statin, path = here::here("Cohorts", "drugs"),
               type = "csv")
