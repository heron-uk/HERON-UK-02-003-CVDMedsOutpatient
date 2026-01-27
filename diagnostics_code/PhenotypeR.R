
result_drugs <- phenotypeDiagnostics(
  cdm[["drugs"]],
  diagnostics = c("databaseDiagnostics",
                  "codelistDiagnostics"))

# export the results
exportSummarisedResult(result_drugs, 
                         fileName = "results_drugs_{cdm_name}_{date}.csv",
                         path = here::here("Results"), 
                         minCellCount = minCellCount)
