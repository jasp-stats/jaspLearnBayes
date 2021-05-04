#library(jaspTools)
#setupJaspTools()
#library(testthat)

#jaspTools::runTestsTravis(module = getwd())

## default settings
{
  options <- analysisOptions("LSBuffonsneedlesimulation")
  options[["n"]] <- 100
  options[["a"]] <- 1
  options[["b"]] <- 1
  options[["length"]] <- 0.8
  options[["CI"]] <- 0.95
  
  
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("LSBuffonsneedlesimulation", dataset, options)#, makeTests = TRUE)
  
  
  test_that("Distribution Plot matches", {
    plotName <- results[["results"]][["distPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "distribution-plot", dir="LSBuffonsneedlesimulation")
  })
  
  test_that("Simulation Plot matches", {
    plotName <- results[["results"]][["simulPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "simulation-plot", dir="LSBuffonsneedlesimulation")
  })
  
  test_that("Summary Table results match", {
    table <- results[["results"]][["summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(3.14, 51, 100, 2.64, 3.88))
  })
  
  
  
}




#to avoid version mismatches between plots from different analyses please validate ALL plots

#jaspTools::manageTestPlots("LSBuffonsneedlesimulation")
#jaspTools::testAnalysis("LSBuffonsneedlesimulation")


