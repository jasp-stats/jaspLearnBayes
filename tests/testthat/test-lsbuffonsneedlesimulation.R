#library(jaspTools)
#setupJaspTools()
#library(jaspTools)
#library(testthat)

#jaspTools::runTestsTravis(module = getwd())

## default settings
{
  options <- analysisOptions("LSBuffonsneedlesimulation")
  options[["n"]] <- 100
  options[["a"]] <- 1
  options[["b"]] <- 1
  options[["length"]] <- 80
  
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("LSBuffonsneedlesimulation", dataset, options, makeTests = TRUE)
  
  
  test_that("Prior and Posterior distribution plot matches", {
    plotName <- results[["results"]][["distPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-distribution", dir="LSBuffonsneedlesimulation")
  })
  
  test_that("Simulation plot matches", {
    plotName <- results[["results"]][["simulPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "simulation", dir="LSBuffonsneedlesimulation")
  })
  
  test_that("Summary Table results match", {
    table <- results[["results"]][["summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(3.13684322209775, 51, 2.63551070716999, 3.87852472473797))
  })
  
  
  
}




#to avoid version mismatches between plots from different analyses please validate ALL plots
#jaspTools::manageTestPlots()

#jaspTools::testAnalysis("LSBuffonsneedlesimulation")
#jaspTools::manageTestPlots("LSBuffonsneedlesimulation")


