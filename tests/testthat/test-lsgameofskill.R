#library(jaspTools)
#setupJaspTools()
#library(jaspTools)
#library(testthat)

#jaspTools::runTestsTravis(module = getwd())

## two players
{
  options <- analysisOptions("LSgameofskill")
  options$winPoints <- 4
  options$players <- list(list(values = c(1, 1)), list(values = c(2, 3)))
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("LSgameofskill", dataset, options)


  test_that("Probability of Player 1 Winning plot matches", {
    plotName <- results[["results"]][["CIPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "probability-of-player-1-winning", dir="LSgameofskill")
  })

  test_that("Summary Table results match", {
    table <- results[["results"]][["summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.0476190476190477, 0.042, 1, 1, 1, 0.952380952380952, 0.958,
                                        2, 3, 2))
  })
}



## three players
{
  options <- analysisOptions("LSgameofskill")
  options$winPoints <- 4
  options$players <- list(list(values = c(1, 1, 2)), list(values = c(1, 2, 3)))
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("LSgameofskill", dataset, options)


  test_that("Probability of Player 1 Winning plot matches", {
    plotName <- results[["results"]][["CIPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "probability-of-player-1-winning2", dir="LSgameofskill")
  })

  test_that("Summary Table results match", {
    table <- results[["results"]][["summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.242857142857143, 0.218, 1, 1, 1, 0.757142857142857, 0.782, 2,
                                        2, 1))
  })
}


#to avoid version mismatches between plots from different analyses please validate ALL plots
#jaspTools::manageTestPlots()

#jaspTools::testAnalysis("LSgameofchance")
#jaspTools::manageTestPlots("LSgameofchance")


