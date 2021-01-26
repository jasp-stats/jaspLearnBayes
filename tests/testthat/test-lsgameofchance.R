#setupJaspTools()

#library(jaspTools)
#library(testthat)

#jaspTools::runTestsTravis(module = getwd())

## two players
{
  options <- jaspTools::analysisOptions("LSgameofchance")
  options[["players"]] <- list(list("values" = c(1,1)), list("values" = c(2,3)))
  options$winPoints <- 4
  options$nSims <- 500
  options$CI <- TRUE
  
  set.seed(1)
  results <- jaspTools::runAnalysis("LSgameofchance", dataset=NULL, options=options) #, makeTests = TRUE)
  #jaspTools::makeTestTable(table)
  
  test_that("Game of Chance two-player plot matches", {
    plotName <- results[["results"]][["CIPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "probability-of-player-1-winning-two-players", dir = "LSgameofchance")
  })
  
  test_that("Game of Chance summaryTable two-player results match", {
    table <- results[["results"]][["summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.037037037037037, 0.333333333333333, 0.032, 1, 1, 
                                        0.962962962962963, 0.666666666666667, 0.968, 2, 3))
  })
}


## three players
{
  options <- analysisOptions("LSgameofchance")
  options$winPoints <- 4
  options$players <- list(list(values =  c(1,1)), list(values = c(1, 2)), list(values = c(2, 3)))
  
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("LSgameofchance", dataset, options)#, makeTests = TRUE)

  test_that("Probability of Player 1 Winning plot matches", {
    plotName <- results[["results"]][["CIPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "probability-of-player-1-winning-three-players", dir="LSgameofchance")
  })
  
  test_that("Summary Table results match", {
    table <- results[["results"]][["summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.02734375, 0.25, 0.026, 1, 1,
                                        0.10546875, 0.25, 0.116, 2, 2,
                                        0.8671875, 0.5, 0.858, 3, 3))
  })
}
#to avoid version mismatches between plots from different analyses please validate ALL plots
#jaspTools::manageTestPlots()

#jaspTools::testAnalysis("LSgameofchance")
#jaspTools::manageTestPlots("LSgameofchance")

#jaspTools::manageTestPlots("LSgameofchance")

