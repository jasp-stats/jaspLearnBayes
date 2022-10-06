context("Learn Bayes - Buffon's Needle Manipulation")

## default settings
options <- jaspTools::analysisOptions("LSBuffonsneedlemanipulation")
options$k <- 10
options$n <- 100
options$a <- 1
options$b <- 1
options$length <- 80
options$CI <- 0.95
options$min <- 3
options$max <- 3.2
options$highlight <- TRUE
options$showPropDistPlot <- TRUE
options$showPiDistPlot <- TRUE
options$CIPiDistPlot <- TRUE
options$CIPropDistPlot <- TRUE
options$legendPropDistPlot <- TRUE
options$legendPiDistPlot <- TRUE

set.seed(1)
dataset <- NULL
results <- jaspTools::runAnalysis("LSBuffonsneedlemanipulation", dataset, options)#, makeTests = TRUE)


test_that("Implied Prior and Posterior for π plot matches", {
  plotName <- results[["results"]][["piDistPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "implied-prior-and-posterior-for-π")
})

test_that("Prior and Posterior for Proportion of Crosses plot matches", {
  plotName <- results[["results"]][["propDistPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-for-proportion-of-crosses")
})

test_that("Summary Table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(16, 0, 16.4381357547624, 10, 100, 9.66364618026701, 32.3161343887626
                                 ))
})