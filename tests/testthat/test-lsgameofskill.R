context("Learn Bayes - Game of Skill")


## two players

options <- jaspTools::analysisOptions("LSgameofskill")
options$winPoints <- 4
options$players <- list(list(values = c(1, 1)), list(values = c(2, 3)))
set.seed(1)
dataset <- NULL

results <- jaspTools::runAnalysis("LSgameofskill", dataset, options)#, makeTests = TRUE)

test_that("Probability of Player A Winning plot matches", {
  plotName <- results[["results"]][["CIPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-of-player-a-winning-2-players")
})

test_that("Summary Table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0476190476190477, 0.042, 1, 1, 1, 0.952380952380952, 0.958,
                                      2, 3, 2))
}) 



## three players

options <- jaspTools::analysisOptions("LSgameofskill")
options$winPoints <- 4
options$players <- list(list(values =  c(1,1)), list(values = c(1, 2)), list(values = c(2, 3)))
set.seed(1)
dataset <- NULL
results <- jaspTools::runAnalysis("LSgameofskill", dataset, options)#, makeTests = TRUE)


test_that("Probability of Player A Winning plot matches", {
  plotName <- results[["results"]][["CIPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-of-player-a-winning-3-players")
})

test_that("Summary Table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0307692307692308, 0.032, 1, 1, 1, 0.158041958041958, 0.144,
                                      2, 2, 1, 0.811188811188807, 0.824, 3, 3, 2))
})




