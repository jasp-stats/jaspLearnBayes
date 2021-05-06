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
  options[["length"]] <- 80
  options[["CI"]] <- 0.95
  options[["plot2"]] <- TRUE
  options[["plot3"]] <- TRUE
  options[["CIArrow"]] <- TRUE
  options[["legend1"]] <- TRUE
  options[["legend2"]] <- TRUE
  
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("LSBuffonsneedlemanipulation", dataset, options)#, makeTests = TRUE)
  
  
  test_that("Implied Prior and Posterior for π plot matches", {
    plotName <- results[["results"]][["distPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "implied-prior-and-posterior-for-π", dir="LSBuffonsneedlemanipulation")
  })
  
  test_that("Prior and Posterior for Proportion of Crosses plot matches", {
    plotName <- results[["results"]][["propPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-for-proportion-of-crosses", dir="LSBuffonsneedlemanipulation")
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


