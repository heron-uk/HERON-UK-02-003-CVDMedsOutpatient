
# aspirin -------
aspirin <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("aspirin"),
  nameStyle = "{concept_name}",
  type = "codelist")
exportCodelist(aspirin, path = here::here("Cohorts", "drugs"),
               type = "csv")

# P2Y12 inhibitors -----
p2y12_inhibitors <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("clopidogrel", "ticagrelor",
           "prasugrel", "cangrelor"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(p2y12_inhibitors) <- paste0("p2y12_inhibitors_", names(p2y12_inhibitors))


p2y12_inhibitors_all <- p2y12_inhibitors |> 
  unionCodelists()
names(p2y12_inhibitors_all) <- "p2y12_inhibitors"

p2y12_inhibitors <- c(p2y12_inhibitors_all, p2y12_inhibitors) |> 
  omopgenerics::newCodelist()

exportCodelist(p2y12_inhibitors, path = here::here("Cohorts", "drugs"),
               type = "csv")


# dipyridamole -------
dipyridamole <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("dipyridamole"),
  nameStyle = "{concept_name}",
  type = "codelist")
exportCodelist(dipyridamole, path = here::here("Cohorts", "drugs"),
               type = "csv")



# ACEi and ARBs -------
acei_arbs <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c(# acei
    "captopril", "enalapril", "fosinopril", 
    "imidapril", "lisinopril", "perindopril", "quinapril", 
    "ramipril", "trandolapril",
    # arbs
    "azilsartan", "candesartan", "eprosartan", "irbesartan",        
    "losartan", "olmesartan", "telmisartan", "valsartan"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(acei_arbs) <- paste0("acei_arbs_", names(acei_arbs))

acei_arbs_all <- acei_arbs |> 
  unionCodelists()
names(acei_arbs_all) <- "acei_arbs"

acei_arbs <- c(acei_arbs_all, acei_arbs) |> 
  omopgenerics::newCodelist()

exportCodelist(acei_arbs, path = here::here("Cohorts", "drugs"),
               type = "csv")


# diuretic (thiazide) ----
thiazide_diuretic <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("indapamide", "chlorthalidone", "metolazone",
           "xipamide", "bendroflumethiazide", 
           "hydrochlorothiazide"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(thiazide_diuretic) <- paste0("thiazide_diuretic_", names(thiazide_diuretic))


thiazide_diuretic_all <- thiazide_diuretic |> 
  unionCodelists()
names(thiazide_diuretic_all) <- "thiazide_diuretics"

thiazide_diuretic <- c(thiazide_diuretic_all, thiazide_diuretic) |> 
  omopgenerics::newCodelist()

exportCodelist(thiazide_diuretic, path = here::here("Cohorts", "drugs"),
               type = "csv")

# calcium-channel blocker ----
calcium_channel_blocker <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("amlodipine", "felodipine", "lacidipine", "lercanidipine", 
           "nicardipine", "nifedipine", "verapamil", "diltiazem"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(calcium_channel_blocker) <- paste0("calcium_channel_blocker_", names(calcium_channel_blocker))

calcium_channel_blocker_all <- calcium_channel_blocker |> 
  unionCodelists()
names(calcium_channel_blocker_all) <- "calcium_channel_blockers"

calcium_channel_blocker <- c(calcium_channel_blocker_all, calcium_channel_blocker) |> 
  omopgenerics::newCodelist()

exportCodelist(calcium_channel_blocker, path = here::here("Cohorts", "drugs"),
               type = "csv")

# beta blockers -------
beta_blockers <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("acebutolol", "alprenolol", "atenolol", 
           "bisoprolol", "carvedilol", 
           "metoprolol", "nadolol",
           "oxprenolol", "pindolol", 
           "propranolol", "timolol"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(beta_blockers) <- paste0("beta_blocker_", names(beta_blockers))

beta_blockers_all <- beta_blockers |> 
  unionCodelists()
names(beta_blockers_all) <- "beta_blockers"

beta_blockers <- c(beta_blockers_all, beta_blockers) |> 
  omopgenerics::newCodelist()

exportCodelist(beta_blockers, path = here::here("Cohorts", "drugs"),
               type = "csv")

# statin -------
statin <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("atorvastatin", "rosuvastatin", "simvastatin", 
           "pravastatin", "fluvastatin"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(statin) <- paste0("statin_", names(statin))

statin_all <- statin |> 
  unionCodelists()
names(statin_all) <- "statin"

statin <- c(statin_all, statin) |> 
  omopgenerics::newCodelist()

exportCodelist(statin, path = here::here("Cohorts", "drugs"),
               type = "csv")

# ezetimibe  -------
ezetimibe <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("ezetimibe"),
  nameStyle = "{concept_name}",
  type = "codelist")
exportCodelist(ezetimibe, path = here::here("Cohorts", "drugs"),
               type = "csv")

# PCSK9 inhibitors ----
pcsk9_inhibitors <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("alirocumab", "evolocumab", 
           "inclisiran"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(pcsk9_inhibitors) <- paste0("pcsk9_inhibitors_", names(pcsk9_inhibitors))


pcsk9_inhibitors_all <- pcsk9_inhibitors |> 
  unionCodelists()
names(pcsk9_inhibitors_all) <- "pcsk9_inhibitors"

pcsk9_inhibitors <- c(pcsk9_inhibitors_all, pcsk9_inhibitors) |> 
  omopgenerics::newCodelist()

exportCodelist(pcsk9_inhibitors, path = here::here("Cohorts", "drugs"),
               type = "csv")

# thrombolytics -------
thrombolytics <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("alteplase", "tenecteplase", 
           "reteplase", "streptokinase"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(thrombolytics) <- paste0("thrombolytics_", names(thrombolytics))

thrombolytics_all <- thrombolytics |> 
  unionCodelists()
names(thrombolytics_all) <- "thrombolytics"

thrombolytics <- c(thrombolytics_all, thrombolytics) |> 
  omopgenerics::newCodelist()

exportCodelist(thrombolytics, path = here::here("Cohorts", "drugs"),
               type = "csv")

# warfarin -------
warfarin <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("warfarin"),
  nameStyle = "{concept_name}",
  type = "codelist")
exportCodelist(warfarin, path = here::here("Cohorts", "drugs"),
               type = "csv")
# direct oral anticoagulants (DOACs) ----
doacs <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("apixaban", "dabigatran", 
           "edoxaban", "rivaroxaban"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(doacs) <- paste0("doacs_", 
                       names(doacs))

doacs_all <- doacs |> 
  unionCodelists()
names(doacs_all) <- "doacs"

doacs <- c(doacs_all, doacs) |> 
  omopgenerics::newCodelist()

exportCodelist(doacs, path = here::here("Cohorts", "drugs"),
               type = "csv")


# GP IIb/ IIIa  -------
gp_iib_iiia <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("eptifibatide", "tirofiban", "abciximab"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(gp_iib_iiia) <- paste0("gp_iib_iiia_", names(gp_iib_iiia))

gp_iib_iiia_all <- gp_iib_iiia |> 
  unionCodelists()
names(gp_iib_iiia_all) <- "gp_iib_iiia"

gp_iib_iiia <- c(gp_iib_iiia_all, gp_iib_iiia) |> 
  omopgenerics::newCodelist()

exportCodelist(gp_iib_iiia, path = here::here("Cohorts", "drugs"),
               type = "csv")

# thrombin inhibitors ----
thrombin_inhibitors <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("heparin", "enoxaparin", 
           "fondaparinux", "bivalirudin"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(thrombin_inhibitors) <- paste0("thrombin_inhibitors_", 
                                names(thrombin_inhibitors))

thrombin_inhibitors_all <- thrombin_inhibitors |> 
  unionCodelists()
names(thrombin_inhibitors_all) <- "thrombin_inhibitors"

thrombin_inhibitors <- c(thrombin_inhibitors_all, thrombin_inhibitors) |> 
  omopgenerics::newCodelist()

exportCodelist(thrombin_inhibitors, path = here::here("Cohorts", "drugs"),
               type = "csv")
