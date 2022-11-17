context("Learn Bayes - Buffon's Needle Simulation")

## default settings
options <- jaspTools::analysisOptions("LSBuffonsneedlesimulation")
options$numberOfThrows                   <- 100
options$priorAlpha                       <- 1
options$priorBeta                        <- 1
options$lengthToDistanceProportion       <- 80
options$ciLevel                          <- 0.95
options$min                              <- 3
options$max                              <- 3.2
options$highlight                        <- TRUE
options$showNeedlePlot                   <- TRUE
options$priorPosteriorProportion         <- TRUE
options$priorPosteriorPi                 <- TRUE
options$needlePlotCrossingNeedlesColored <- TRUE
options$priorPosteriorPiCi               <- TRUE
options$priorPosteriorProportionLegend   <- TRUE
options$CIPropDistPlot                   <- TRUE
options$priorPosteriorPiLegend           <- TRUE

set.seed(1)
dataset <- NULL
results <- jaspTools::runAnalysis("LSBuffonsneedlesimulation", dataset, options) #, makeTests = TRUE)

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
                                 list(3.33333333333333, 0.201076042285508, 3.33426320785458, 48, 100,
                                      2.77007004360189, 4.17585162520342))
})
