#library(jaspTools)
#setupJaspTools()
#library(testthat)

#jaspTools::runTestsTravis(module = getwd())

## default settings
{
  options <- analysisOptions("LSBuffonsneedlemanipulation")
  options[["k"]] <- 50
  options[["n"]] <- 100
  options[["a"]] <- 1
  options[["b"]] <- 1
  options[["length"]] <- 0.8
  options[["CI"]] <- 0.95
  
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("LSBuffonsneedlemanipulation", dataset, options)#, makeTests = TRUE)
  
  
  test_that("Distribution Plot matches", {
    plotName <- results[["results"]][["distPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "distribution-plot", dir="LSBuffonsneedlemanipulation")
  })
  
  test_that("Summary Table results match", {
    table <- results[["results"]][["summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(3.2, 50, 100, 2.68, 3.97))
  })
}




#to avoid version mismatches between plots from different analyses please validate ALL plots

#jaspTools::manageTestPlots("LSBuffonsneedlemanipulation")
#jaspTools::testAnalysis("LSBuffonsneedlemanipulation")


