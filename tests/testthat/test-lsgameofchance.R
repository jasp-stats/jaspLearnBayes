context("Learn Bayes - Game of Chance")


## two players
{
  options <- jaspTools::analysisOptions("LSgameofchance")
  options$players <- list(list("values" = c(1,1)), list("values" = c(2,3)))
  options$winPoints <- 4
  options$nSims <- 500
  options$CI <- TRUE
  
  set.seed(1)
  results <- jaspTools::runAnalysis("LSgameofchance", dataset=NULL, options=options)#, makeTests = TRUE)
  #jaspTools::makeTestTable(table)
  
  test_that("Game of Chance two-player plot matches", {
    plotName <- results[["results"]][["CIPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "probability-of-player-1-winning-two-players")

  })
  
  test_that("Game of Chance summaryTable two-player results match", {
    table <- results[["results"]][["summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.037037037037037, 0.333333333333333, 0.032, "A", 1, 0.962962962962963,
                                        0.666666666666667, 0.968, "B", 3))  
    })
}


## three players
{
  options <- jaspTools::analysisOptions("LSgameofchance")
  options$winPoints <- 4
  options$players <- list(list(values =  c(1,1)), list(values = c(1, 2)), list(values = c(2, 3)))
  
  set.seed(1)
  dataset <- NULL
  results <- jaspTools::runAnalysis("LSgameofchance", dataset, options)#, makeTests = TRUE)

  test_that("Probability of Player 1 Winning plot matches", {
    plotName <- results[["results"]][["CIPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "probability-of-player-1-winning")
  })
  
  test_that("Summary Table results match", {
    table <- results[["results"]][["summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.02734375, 0.25, 0.024, "A", 1, 0.10546875, 0.25, 0.118, "B",
                                        2, 0.8671875, 0.5, 0.858, "C", 3))
  })
}

