context("Learn Bayes - Buffon's Needle Simulation")

## default settings
options <- analysisOptions("LSBuffonsneedlesimulation")
options$n <- 100
options$a <- 1
options$b <- 1
options$length <- 80
options$CI <- 0.95
options$showNeedlePlot <- TRUE
options$showPropDistPlot <- TRUE
options$showPiDistPlot <- TRUE
options$color <- TRUE
options$CIArrow <- TRUE
options$legendPropDistPlot <- TRUE
options$legendPiDistPlot <- TRUE

set.seed(1)
dataset <- NULL
results <- runAnalysis("LSBuffonsneedlesimulation", dataset, options)#, makeTests = TRUE)

test_that("Needle Plot matches", {
  plotName <- results[["results"]][["needlePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "needle-plot")
})

test_that("Implied Prior and Posterior for pi plot matches", {
  skip_on_os("windows")
  plotName <- results[["results"]][["piDistPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "implied-prior-and-posterior-for-pi")
})

test_that("Prior and Posterior for Proportion of Crosses plot matches", {
  plotName <- results[["results"]][["propDistPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-for-proportion-of-crosses")
})

test_that("Summary Table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.33, 48, 100, 2.77, 4.18))
})