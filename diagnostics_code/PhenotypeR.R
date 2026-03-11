databaseResult <- databaseDiagnostics(cdm[["study_cohorts"]])

exportSummarisedResult(databaseResult, 
                       fileName = "results_database_{cdm_name}_{date}.csv",
                       path = here::here("Results"), 
                       minCellCount = minCellCount)



codelistResult <- codelistDiagnostics(cdm[["study_cohorts"]], 
                               measurementSample = 0)


exportSummarisedResult(codelistResult, 
                       fileName = "results_codelists_{cdm_name}_{date}.csv",
                       path = here::here("Results"), 
                       minCellCount = minCellCount)

cohortResult <- cohortDiagnostics(cdm[["study_cohorts"]],
                                   matchedSample = 0)
# export the results
exportSummarisedResult(cohortResult, 
                      fileName = "results_cohorts_{cdm_name}_{date}.csv",
                      path = here::here("Results"), 
                      minCellCount = minCellCount)
