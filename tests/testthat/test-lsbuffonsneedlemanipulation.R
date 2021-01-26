#library(jaspTools)
#setupJaspTools()
#library(jaspTools)
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
  
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("LSBuffonsneedlemanipulation", dataset, options)#, makeTests = TRUE)
  
  
  test_that("Prior and Posterior distribution plot matches", {
    plotName <- results[["results"]][["distPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-distribution", dir="LSBuffonsneedlemanipulation")
  })
  
  test_that("Summary Table results match", {
    table <- results[["results"]][["summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(3.2, 2.67871156729564, 3.97320158382837))
  })
}




#to avoid version mismatches between plots from different analyses please validate ALL plots
#jaspTools::manageTestPlots()

#jaspTools::testAnalysis("LSBuffonsneedlemanipulation")
#jaspTools::manageTestPlots("LSBuffonsneedlemanipulation")


