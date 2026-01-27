
result <- phenotypeDiagnostics(cdm[["study_cohorts"]])

# export the results
exportSummarisedResult(result, 
                      fileName = "results_drugs_{cdm_name}_{date}.csv",
                      path = here::here("Results"), 
                      minCellCount = minCellCount)
