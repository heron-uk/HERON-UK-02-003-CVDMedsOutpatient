phenotypeResults <- phenotypeDiagnostics(
  cdm$cardiovascular_drugs,
  survival = FALSE,
  populationDateRange = study_period
)

PhenotypeR::exportSummarisedResult(phenotypeResults, path = results_folder,
                                   minCellCount = min_cell_count)

phenotypeResults <- PhenotypeR::importSummarisedResult(here("Results", "GOLD_100k", "results_GOLD_100k_2025_12_03.csv"))

PhenotypeR::shinyDiagnostics(
  result = phenotypeResults,
  directory = "/home/AD_NDORMS/rowelin/R/heronCardiovascularDrugs/Report"
)
