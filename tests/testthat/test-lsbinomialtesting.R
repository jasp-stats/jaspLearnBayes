context("Learn Bayes - Binomial Testing")

# the conditional == individual plots are comprehensivelly tested in estimation part

### output for all default settings (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$priorPredictivePerformanceBfType <- "BF10"
  options$sequentialAnalysisPredictivePerformancePlotBfType <- "BF10"
  options$priorPredictivePerformanceBfComparison <- "inclusion"
  options$sequentialAnalysisPredictivePerformancePlotBfComparison <- "inclusion"
  options$priorPredictivePerformanceBfVsHypothesis <- ""
  options$sequentialAnalysisPredictivePerformancePlotBfVsHypothesis <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataInputType <- "variable"
  options$dataSequenceSequenceOfObservations <- ""
  options$introductoryText <- TRUE
  options$dataSequenceFailures <- list()
  options$dataVariableFailures <- c("2", "3")
  options$dataSequenceSuccesses <- list()
  options$dataVariableSuccesses <- "1"
  options$dataCountsFailures <- 0
  options$dataCountsSuccesses <- 0
  options$priorAndPosteriorDistributionPlot <- TRUE
  options$priorAndPosteriorDistributionPlotObservedProportion <- FALSE
  options$priorAndPosteriorDistributionPlotType <- "conditional"
  options$sequentialAnalysisPredictivePerformancePlot <- TRUE
  options$sequentialAnalysisPredictivePerformancePlotType <- "conditional"
  options$sequentialAnalysisPredictivePerformancePlotUpdatingTable <- FALSE
  options$posteriorDistributionPlot <- TRUE
  options$posteriorDistributionPlotConditionalCiBf <- 1
  options$posteriorDistributionPlotConditionalCi <- FALSE
  options$posteriorDistributionPlotConditionalCiMass <- 0.95
  options$posteriorDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorDistributionPlotJointType <- "overlying"
  options$posteriorDistributionPlotConditionalCiLower <- 0
  options$posteriorDistributionPlotMarginalCiBf <- 1
  options$posteriorDistributionPlotMarginalCi <- FALSE
  options$posteriorDistributionPlotMarginalCiMass <- 0.95
  options$posteriorDistributionPlotMarginalPointEstimate <- FALSE
  options$posteriorDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorDistributionPlotMarginalCiLower <- 0.25
  options$posteriorDistributionPlotMarginalCiType <- "central"
  options$posteriorDistributionPlotMarginalCiUpper <- 0.75
  options$posteriorDistributionPlotObservedProportion <- FALSE
  options$posteriorDistributionPlotType <- "joint"
  options$posteriorDistributionPlotConditionalCiType <- "central"
  options$posteriorDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotConditionalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotJoinType <- "overlying"
  options$priorPredictivePerformanceDistributionPlotConditionalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotMarginalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotMarginalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotConditionalCi <- FALSE
  options$posteriorPredictionDistributionPlotConditionalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotJoinType <- "overlying"
  options$posteriorPredictionDistributionPlotConditionalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCi <- FALSE
  options$posteriorPredictionDistributionPlotMarginalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotMarginalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotMarginalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCiType <- "central"
  options$posteriorPredictionDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotType <- "joint"
  options$posteriorPredictionDistributionPlotConditionalCiType <- "central"
  options$posteriorPredictionDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotType <- "conditional"
  options$priorPredictivePerformanceDistributionPlotConditionalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlot <- TRUE
  options$priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess <- FALSE
  options$posteriorPredictionDistributionPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlotType <- "conditional"
  options$priorDistributionPlot <- TRUE
  options$priorDistributionPlotConditionalCi <- FALSE
  options$priorDistributionPlotConditionalCiMass <- 0.95
  options$priorDistributionPlotConditionalPointEstimate <- FALSE
  options$priorDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorDistributionPlotJointType <- "overlying"
  options$priorDistributionPlotConditionalCiLower <- 0.25
  options$priorDistributionPlotMarginalCi <- FALSE
  options$priorDistributionPlotMarginalCiMass <- 0.95
  options$priorDistributionPlotMarginalPointEstimate <- FALSE
  options$priorDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorDistributionPlotMarginalCiLower <- 0.25
  options$priorDistributionPlotMarginalCiType <- "central"
  options$priorDistributionPlotMarginalCiUpper <- 0.75
  options$priorDistributionPlotType <- "conditional"
  options$priorDistributionPlotConditionalCiType <- "central"
  options$priorDistributionPlotConditionalCiUpper <- 0.75
  options$posteriorPredictionNumberOfFutureTrials <- 1
  options$posteriorPredictionDistributionPlotAsSampleProportion <- FALSE
  options$posteriorPredictionDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTable <- FALSE
  options$priorPredictivePerformanceDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTablePointEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""),
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$dataVariableSelected <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("LSbinomialtesting", "debug", options)


  test_that("Hypothesis Beta plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot_Hypothesis Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-beta-default-1", dir="LSbinomialtesting")
  })

  test_that("Hypothesis Spike plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot_Hypothesis Spike"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-spike-default-2", dir="LSbinomialtesting")
  })

  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-plot-2-default-3", dir="LSbinomialtesting")
  })

  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_priorPredictivePerformanceDistributionPlotPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-prediction-plot-3-default-4", dir="LSbinomialtesting")
  })

  test_that("Hypothesis Beta plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior_Hypothesis Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-beta-default-5", dir="LSbinomialtesting")
  })

  test_that("Hypothesis Spike plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior_Hypothesis Spike"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-spike-default-6", dir="LSbinomialtesting")
  })

  test_that("Hypothesis Beta plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior_Hypothesis Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-beta-default-7", dir="LSbinomialtesting")
  })

  test_that("Hypothesis Spike plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior_Hypothesis Spike"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-spike-default-8", dir="LSbinomialtesting")
  })

  test_that("Predictive Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "predictive-accuracy-plot-8-default-9", dir="LSbinomialtesting")
  })

  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_sequentialAnalysisPredictivePerformancePlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "titleless-plot-9-default-10", dir="LSbinomialtesting")
  })

  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })

  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.22178657769189, "Hypothesis Spike", -5.616913585436, 0.181526448024068,
                                        0.5, 4.50883912997303, "Hypothesis Beta", -4.11087386417331,
                                        0.818473551975932, 0.5))
  })
}
### more options vol. 1 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$priorPredictivePerformanceBfType <- "BF01"
  options$sequentialAnalysisPredictivePerformancePlotBfType <- "BF10"
  options$priorPredictivePerformanceBfComparison <- "best"
  options$sequentialAnalysisPredictivePerformancePlotBfComparison <- "inclusion"
  options$priorPredictivePerformanceBfVsHypothesis <- ""
  options$sequentialAnalysisPredictivePerformancePlotBfVsHypothesis <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataInputType <- "variable"
  options$dataSequenceSequenceOfObservations <- ""
  options$introductoryText <- TRUE
  options$dataSequenceFailures <- list()
  options$dataVariableFailures <- c("2", "3")
  options$dataSequenceSuccesses <- list()
  options$dataVariableSuccesses <- "1"
  options$dataCountsFailures <- 0
  options$dataCountsSuccesses <- 0
  options$priorAndPosteriorDistributionPlot <- TRUE
  options$priorAndPosteriorDistributionPlotObservedProportion <- FALSE
  options$priorAndPosteriorDistributionPlotType <- "joint"
  options$sequentialAnalysisPredictivePerformancePlot <- TRUE
  options$sequentialAnalysisPredictivePerformancePlotType <- "joint"
  options$sequentialAnalysisPredictivePerformancePlotUpdatingTable <- FALSE
  options$posteriorDistributionPlot <- TRUE
  options$posteriorDistributionPlotConditionalCiBf <- 1
  options$posteriorDistributionPlotConditionalCi <- FALSE
  options$posteriorDistributionPlotConditionalCiMass <- 0.95
  options$posteriorDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorDistributionPlotJointType <- "overlying"
  options$posteriorDistributionPlotConditionalCiLower <- 0
  options$posteriorDistributionPlotMarginalCiBf <- 1
  options$posteriorDistributionPlotMarginalCi <- FALSE
  options$posteriorDistributionPlotMarginalCiMass <- 0.95
  options$posteriorDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorDistributionPlotMarginalCiLower <- 0.25
  options$posteriorDistributionPlotMarginalCiType <- "central"
  options$posteriorDistributionPlotMarginalCiUpper <- 0.75
  options$posteriorDistributionPlotObservedProportion <- FALSE
  options$posteriorDistributionPlotType <- "marginal"
  options$posteriorDistributionPlotConditionalCiType <- "central"
  options$posteriorDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotConditionalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotJoinType <- "overlying"
  options$priorPredictivePerformanceDistributionPlotConditionalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotMarginalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotMarginalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotConditionalCi <- FALSE
  options$posteriorPredictionDistributionPlotConditionalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotJoinType <- "overlying"
  options$posteriorPredictionDistributionPlotConditionalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCi <- FALSE
  options$posteriorPredictionDistributionPlotMarginalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorPredictionDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotMarginalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCiType <- "central"
  options$posteriorPredictionDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotType <- "marginal"
  options$posteriorPredictionDistributionPlotConditionalCiType <- "central"
  options$posteriorPredictionDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotType <- "marginal"
  options$priorPredictivePerformanceDistributionPlotConditionalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlot <- TRUE
  options$priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess <- FALSE
  options$posteriorPredictionDistributionPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlotType <- "joint"
  options$priorDistributionPlot <- TRUE
  options$priorDistributionPlotConditionalCi <- FALSE
  options$priorDistributionPlotConditionalCiMass <- 0.95
  options$priorDistributionPlotConditionalPointEstimate <- FALSE
  options$priorDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorDistributionPlotJointType <- "overlying"
  options$priorDistributionPlotConditionalCiLower <- 0.25
  options$priorDistributionPlotMarginalCi <- FALSE
  options$priorDistributionPlotMarginalCiMass <- 0.95
  options$priorDistributionPlotMarginalPointEstimate <- FALSE
  options$priorDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorDistributionPlotMarginalCiLower <- 0.25
  options$priorDistributionPlotMarginalCiType <- "central"
  options$priorDistributionPlotMarginalCiUpper <- 0.75
  options$priorDistributionPlotType <- "marginal"
  options$priorDistributionPlotConditionalCiType <- "central"
  options$priorDistributionPlotConditionalCiUpper <- 0.75
  options$posteriorPredictionNumberOfFutureTrials <- 1
  options$posteriorPredictionDistributionPlotAsSampleProportion <- FALSE
  options$posteriorPredictionDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTable <- FALSE
  options$priorPredictivePerformanceDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTablePointEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""),
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$dataVariableSelected <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("LSbinomialtesting", "debug", options)


  test_that("Hypothesis Spike plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot_plotsBoth_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-spike-vol1-1", dir="LSbinomialtesting")
  })

  test_that("Hypothesis Beta plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot_plotsBoth_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-beta-vol1-2", dir="LSbinomialtesting")
  })

  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-plot-2-vol1-3", dir="LSbinomialtesting")
  })

  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_priorPredictivePerformanceDistributionPlotPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-prediction-plot-3-vol1-4", dir="LSbinomialtesting")
  })

  test_that("Prior Prediction matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-prediction-plot-4-vol1-5", dir="LSbinomialtesting")
  })

  test_that("Prior and Posterio plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-plot-5-vol1-6", dir="LSbinomialtesting")
  })

  test_that("Predictiove Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "predictive-accuracy-plot-6-vol1-7", dir="LSbinomialtesting")
  })

  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_sequentialAnalysisPredictivePerformancePlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "sequential-plot-7-vol1-8", dir="LSbinomialtesting")
  })

  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })

  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(4.50883912997304, "Hypothesis Spike", -5.616913585436, 0.181526448024068,
                                        0.5, 1, "Hypothesis Beta", -4.11087386417331, 0.818473551975932,
                                        0.5))
  })
}
### more options vol. 2 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$priorPredictivePerformanceBfType <- "LogBF10"
  options$sequentialAnalysisPredictivePerformancePlotBfType <- "BF10"
  options$priorPredictivePerformanceBfComparison <- "vs"
  options$sequentialAnalysisPredictivePerformancePlotBfComparison <- "inclusion"
  options$priorPredictivePerformanceBfVsHypothesis <- "Hypothesis Spike"
  options$sequentialAnalysisPredictivePerformancePlotBfVsHypothesis <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataInputType <- "variable"
  options$dataSequenceSequenceOfObservations <- ""
  options$introductoryText <- TRUE
  options$dataSequenceFailures <- list()
  options$dataVariableFailures <- c("2", "3")
  options$dataSequenceSuccesses <- list()
  options$dataVariableSuccesses <- "1"
  options$dataCountsFailures <- 0
  options$dataCountsSuccesses <- 0
  options$priorAndPosteriorDistributionPlot <- TRUE
  options$priorAndPosteriorDistributionPlotObservedProportion <- FALSE
  options$priorAndPosteriorDistributionPlotType <- "joint"
  options$sequentialAnalysisPredictivePerformancePlot <- TRUE
  options$sequentialAnalysisPredictivePerformancePlotType <- "joint"
  options$sequentialAnalysisPredictivePerformancePlotUpdatingTable <- FALSE
  options$posteriorDistributionPlot <- TRUE
  options$posteriorDistributionPlotConditionalCiBf <- 1
  options$posteriorDistributionPlotConditionalCi <- FALSE
  options$posteriorDistributionPlotConditionalCiMass <- 0.95
  options$posteriorDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorDistributionPlotJointType <- "overlying"
  options$posteriorDistributionPlotConditionalCiLower <- 0
  options$posteriorDistributionPlotMarginalCiBf <- 1
  options$posteriorDistributionPlotMarginalCi <- FALSE
  options$posteriorDistributionPlotMarginalCiMass <- 0.95
  options$posteriorDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorDistributionPlotMarginalCiLower <- 0.25
  options$posteriorDistributionPlotMarginalCiType <- "central"
  options$posteriorDistributionPlotMarginalCiUpper <- 0.75
  options$posteriorDistributionPlotObservedProportion <- FALSE
  options$posteriorDistributionPlotType <- "marginal"
  options$posteriorDistributionPlotConditionalCiType <- "central"
  options$posteriorDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotConditionalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotJoinType <- "overlying"
  options$priorPredictivePerformanceDistributionPlotConditionalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCi <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimate <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotMarginalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotConditionalCi <- FALSE
  options$posteriorPredictionDistributionPlotConditionalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotJoinType <- "overlying"
  options$posteriorPredictionDistributionPlotConditionalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCi <- FALSE
  options$posteriorPredictionDistributionPlotMarginalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorPredictionDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotMarginalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCiType <- "central"
  options$posteriorPredictionDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotType <- "marginal"
  options$posteriorPredictionDistributionPlotConditionalCiType <- "central"
  options$posteriorPredictionDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotType <- "marginal"
  options$priorPredictivePerformanceDistributionPlotConditionalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlot <- TRUE
  options$priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess <- FALSE
  options$posteriorPredictionDistributionPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlotType <- "joint"
  options$priorDistributionPlot <- TRUE
  options$priorDistributionPlotConditionalCi <- FALSE
  options$priorDistributionPlotConditionalCiMass <- 0.95
  options$priorDistributionPlotConditionalPointEstimate <- FALSE
  options$priorDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorDistributionPlotJointType <- "overlying"
  options$priorDistributionPlotConditionalCiLower <- 0.25
  options$priorDistributionPlotMarginalCi <- TRUE
  options$priorDistributionPlotMarginalCiMass <- 0.95
  options$priorDistributionPlotMarginalPointEstimate <- TRUE
  options$priorDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorDistributionPlotMarginalCiLower <- 0.25
  options$priorDistributionPlotMarginalCiType <- "central"
  options$priorDistributionPlotMarginalCiUpper <- 0.75
  options$priorDistributionPlotType <- "marginal"
  options$priorDistributionPlotConditionalCiType <- "central"
  options$priorDistributionPlotConditionalCiUpper <- 0.75
  options$posteriorPredictionNumberOfFutureTrials <- 1
  options$posteriorPredictionDistributionPlotAsSampleProportion <- FALSE
  options$posteriorPredictionDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTable <- FALSE
  options$priorPredictivePerformanceDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTablePointEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""),
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$dataVariableSelected <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("LSbinomialtesting", "debug", options)


  test_that("Hypothesis Spike plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot_plotsBoth_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-spike-vol2-1", dir="LSbinomialtesting")
  })

  test_that("Hypothesis Beta plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot_plotsBoth_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-beta-vol2-2", dir="LSbinomialtesting")
  })

  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-plot-2-vol2-3", dir="LSbinomialtesting")
  })

  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_priorPredictivePerformanceDistributionPlotPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-prediction-plot-3-vol2-4", dir="LSbinomialtesting")
  })

  test_that("Prior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-prediction-plot-4-vol2-5", dir="LSbinomialtesting")
  })

  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-plot-5-vol2-6", dir="LSbinomialtesting")
  })

  test_that("Predictive Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "predicitve-accuracy-plot-6-vol2-6", dir="LSbinomialtesting")
  })

  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_sequentialAnalysisPredictivePerformancePlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "sequential-plot-7-vol2-7", dir="LSbinomialtesting")
  })

  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })

  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("Hypothesis Spike", -5.616913585436, 0.181526448024068, 0.5, 1.50603972126269,
                                        "Hypothesis Beta", -4.11087386417331, 0.818473551975932, 0.5
                                   ))
  })
}
### more options vol. 3 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$priorPredictivePerformanceBfType <- "LogBF10"
  options$sequentialAnalysisPredictivePerformancePlotBfType <- "BF10"
  options$priorPredictivePerformanceBfComparison <- "vs"
  options$sequentialAnalysisPredictivePerformancePlotBfComparison <- "inclusion"
  options$priorPredictivePerformanceBfVsHypothesis <- "Hypothesis Spike"
  options$sequentialAnalysisPredictivePerformancePlotBfVsHypothesis <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataInputType <- "variable"
  options$dataSequenceSequenceOfObservations <- ""
  options$introductoryText <- TRUE
  options$dataSequenceFailures <- list()
  options$dataVariableFailures <- c("2", "3")
  options$dataSequenceSuccesses <- list()
  options$dataVariableSuccesses <- "1"
  options$dataCountsFailures <- 0
  options$dataCountsSuccesses <- 0
  options$priorAndPosteriorDistributionPlot <- TRUE
  options$priorAndPosteriorDistributionPlotObservedProportion <- FALSE
  options$priorAndPosteriorDistributionPlotType <- "joint"
  options$sequentialAnalysisPredictivePerformancePlot <- TRUE
  options$sequentialAnalysisPredictivePerformancePlotType <- "joint"
  options$sequentialAnalysisPredictivePerformancePlotUpdatingTable <- FALSE
  options$posteriorDistributionPlot <- TRUE
  options$posteriorDistributionPlotConditionalCiBf <- 1
  options$posteriorDistributionPlotConditionalCi <- FALSE
  options$posteriorDistributionPlotConditionalCiMass <- 0.95
  options$posteriorDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorDistributionPlotJointType <- "overlying"
  options$posteriorDistributionPlotConditionalCiLower <- 0
  options$posteriorDistributionPlotMarginalCiBf <- 1
  options$posteriorDistributionPlotMarginalCi <- FALSE
  options$posteriorDistributionPlotMarginalCiMass <- 0.95
  options$posteriorDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorDistributionPlotMarginalCiLower <- 0.25
  options$posteriorDistributionPlotMarginalCiType <- "central"
  options$posteriorDistributionPlotMarginalCiUpper <- 0.75
  options$posteriorDistributionPlotObservedProportion <- FALSE
  options$posteriorDistributionPlotType <- "marginal"
  options$posteriorDistributionPlotConditionalCiType <- "central"
  options$posteriorDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotConditionalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotJoinType <- "overlying"
  options$priorPredictivePerformanceDistributionPlotConditionalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCi <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimate <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotMarginalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCiType <- "HPD"
  options$priorPredictivePerformanceDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotConditionalCi <- FALSE
  options$posteriorPredictionDistributionPlotConditionalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotJoinType <- "overlying"
  options$posteriorPredictionDistributionPlotConditionalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCi <- FALSE
  options$posteriorPredictionDistributionPlotMarginalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorPredictionDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotMarginalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCiType <- "central"
  options$posteriorPredictionDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotType <- "marginal"
  options$posteriorPredictionDistributionPlotConditionalCiType <- "central"
  options$posteriorPredictionDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotType <- "marginal"
  options$priorPredictivePerformanceDistributionPlotConditionalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlot <- TRUE
  options$priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess <- FALSE
  options$posteriorPredictionDistributionPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlotType <- "joint"
  options$priorDistributionPlot <- TRUE
  options$priorDistributionPlotConditionalCi <- FALSE
  options$priorDistributionPlotConditionalCiMass <- 0.95
  options$priorDistributionPlotConditionalPointEstimate <- FALSE
  options$priorDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorDistributionPlotJointType <- "overlying"
  options$priorDistributionPlotConditionalCiLower <- 0.25
  options$priorDistributionPlotMarginalCi <- TRUE
  options$priorDistributionPlotMarginalCiMass <- 0.95
  options$priorDistributionPlotMarginalPointEstimate <- TRUE
  options$priorDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorDistributionPlotMarginalCiLower <- 0.25
  options$priorDistributionPlotMarginalCiType <- "custom"
  options$priorDistributionPlotMarginalCiUpper <- 0.75
  options$priorDistributionPlotType <- "marginal"
  options$priorDistributionPlotConditionalCiType <- "central"
  options$priorDistributionPlotConditionalCiUpper <- 0.75
  options$posteriorPredictionNumberOfFutureTrials <- 1
  options$posteriorPredictionDistributionPlotAsSampleProportion <- FALSE
  options$posteriorPredictionDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTable <- FALSE
  options$priorPredictivePerformanceDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTablePointEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""),
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$dataVariableSelected <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("LSbinomialtesting", "debug", options)


  test_that("Hypothesis Spike plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot_plotsBoth_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-spike-vol3-1", dir="LSbinomialtesting")
  })

  test_that("Hypothesis Beta plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot_plotsBoth_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-beta-vol3-2", dir="LSbinomialtesting")
  })

  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-plot-2-vol3-3", dir="LSbinomialtesting")
  })

  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_priorPredictivePerformanceDistributionPlotPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-prediction-plot-3-vol3-4", dir="LSbinomialtesting")
  })

  test_that("Prior-Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-prediction-plot-4-vol3-5", dir="LSbinomialtesting")
  })

  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-plot-5-vol3-6", dir="LSbinomialtesting")
  })

  test_that("Predictive Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "predictive-accuracy-plot-6-vol3-7", dir="LSbinomialtesting")
  })

  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_sequentialAnalysisPredictivePerformancePlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "sequential-plot-7-vol3-8", dir="LSbinomialtesting")
  })

  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })

  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("Hypothesis Spike", -5.616913585436, 0.181526448024068, 0.5, 1.50603972126269,
                                        "Hypothesis Beta", -4.11087386417331, 0.818473551975932, 0.5
                                   ))
  })
}
### more options vol. 4 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$priorPredictivePerformanceBfType <- "LogBF10"
  options$sequentialAnalysisPredictivePerformancePlotBfType <- "BF10"
  options$priorPredictivePerformanceBfComparison <- "vs"
  options$sequentialAnalysisPredictivePerformancePlotBfComparison <- "inclusion"
  options$priorPredictivePerformanceBfVsHypothesis <- "Hypothesis Spike"
  options$sequentialAnalysisPredictivePerformancePlotBfVsHypothesis <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataInputType <- "variable"
  options$dataSequenceSequenceOfObservations <- ""
  options$introductoryText <- TRUE
  options$dataSequenceFailures <- list()
  options$dataVariableFailures <- c("2", "3")
  options$dataSequenceSuccesses <- list()
  options$dataVariableSuccesses <- "1"
  options$dataCountsFailures <- 0
  options$dataCountsSuccesses <- 0
  options$priorAndPosteriorDistributionPlot <- TRUE
  options$priorAndPosteriorDistributionPlotObservedProportion <- FALSE
  options$priorAndPosteriorDistributionPlotType <- "marginal"
  options$sequentialAnalysisPredictivePerformancePlot <- TRUE
  options$sequentialAnalysisPredictivePerformancePlotType <- "marginal"
  options$sequentialAnalysisPredictivePerformancePlotUpdatingTable <- FALSE
  options$posteriorDistributionPlot <- TRUE
  options$posteriorDistributionPlotConditionalCiBf <- 1
  options$posteriorDistributionPlotConditionalCi <- FALSE
  options$posteriorDistributionPlotConditionalCiMass <- 0.95
  options$posteriorDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorDistributionPlotJointType <- "overlying"
  options$posteriorDistributionPlotConditionalCiLower <- 0
  options$posteriorDistributionPlotMarginalCiBf <- 1
  options$posteriorDistributionPlotMarginalCi <- TRUE
  options$posteriorDistributionPlotMarginalCiMass <- 0.95
  options$posteriorDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorDistributionPlotMarginalCiLower <- 0.25
  options$posteriorDistributionPlotMarginalCiType <- "support"
  options$posteriorDistributionPlotMarginalCiUpper <- 0.75
  options$posteriorDistributionPlotObservedProportion <- FALSE
  options$posteriorDistributionPlotType <- "marginal"
  options$posteriorDistributionPlotConditionalCiType <- "central"
  options$posteriorDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotConditionalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotJoinType <- "overlying"
  options$priorPredictivePerformanceDistributionPlotConditionalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCi <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimate <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimateType <- "median"
  options$priorPredictivePerformanceDistributionPlotMarginalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCiType <- "custom"
  options$priorPredictivePerformanceDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotConditionalCi <- FALSE
  options$posteriorPredictionDistributionPlotConditionalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotJoinType <- "overlying"
  options$posteriorPredictionDistributionPlotConditionalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCi <- FALSE
  options$posteriorPredictionDistributionPlotMarginalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorPredictionDistributionPlotMarginalPointEstimateType <- "mode"
  options$posteriorPredictionDistributionPlotMarginalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCiType <- "central"
  options$posteriorPredictionDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotType <- "marginal"
  options$posteriorPredictionDistributionPlotConditionalCiType <- "central"
  options$posteriorPredictionDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotType <- "marginal"
  options$priorPredictivePerformanceDistributionPlotConditionalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlot <- TRUE
  options$priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess <- FALSE
  options$posteriorPredictionDistributionPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlotType <- "marginal"
  options$priorDistributionPlot <- TRUE
  options$priorDistributionPlotConditionalCi <- FALSE
  options$priorDistributionPlotConditionalCiMass <- 0.95
  options$priorDistributionPlotConditionalPointEstimate <- FALSE
  options$priorDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorDistributionPlotJointType <- "overlying"
  options$priorDistributionPlotConditionalCiLower <- 0.25
  options$priorDistributionPlotMarginalCi <- TRUE
  options$priorDistributionPlotMarginalCiMass <- 0.01
  options$priorDistributionPlotMarginalPointEstimate <- TRUE
  options$priorDistributionPlotMarginalPointEstimateType <- "median"
  options$priorDistributionPlotMarginalCiLower <- 0.25
  options$priorDistributionPlotMarginalCiType <- "central"
  options$priorDistributionPlotMarginalCiUpper <- 0.75
  options$priorDistributionPlotType <- "marginal"
  options$priorDistributionPlotConditionalCiType <- "central"
  options$priorDistributionPlotConditionalCiUpper <- 0.75
  options$posteriorPredictionNumberOfFutureTrials <- 1
  options$posteriorPredictionDistributionPlotAsSampleProportion <- FALSE
  options$posteriorPredictionDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTable <- FALSE
  options$priorPredictivePerformanceDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTablePointEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""),
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$dataVariableSelected <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("LSbinomialtesting", "debug", options)


  test_that("Prior and Posterior plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot_plotsBothPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-plot-0-vol4-1", dir="LSbinomialtesting")
  })

  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-plot-1-vol4-2", dir="LSbinomialtesting")
  })

  test_that("Posterior prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_priorPredictivePerformanceDistributionPlotPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-prediction-plot-2-vol4-3", dir="LSbinomialtesting")
  })

  test_that("Prior prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-prediction-plot-3-vol4-4", dir="LSbinomialtesting")
  })

  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-plot-4-vol4-5", dir="LSbinomialtesting")
  })

  test_that("Predictive Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "predictive-accuracy-plot-5-vol4-6", dir="LSbinomialtesting")
  })

  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_sequentialAnalysisPredictivePerformancePlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "sequential-plot-6-vol4-7", dir="LSbinomialtesting")
  })

  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })

  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("Hypothesis Spike", -5.616913585436, 0.181526448024068, 0.5, 1.50603972126269,
                                        "Hypothesis Beta", -4.11087386417331, 0.818473551975932, 0.5
                                   ))
  })
}
### more options vol. 5 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$priorPredictivePerformanceBfType <- "LogBF10"
  options$sequentialAnalysisPredictivePerformancePlotBfType <- "BF01"
  options$priorPredictivePerformanceBfComparison <- "vs"
  options$sequentialAnalysisPredictivePerformancePlotBfComparison <- "vs"
  options$priorPredictivePerformanceBfVsHypothesis <- "Hypothesis Beta"
  options$sequentialAnalysisPredictivePerformancePlotBfVsHypothesis <- "Hypothesis Spike"
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataInputType <- "variable"
  options$dataSequenceSequenceOfObservations <- ""
  options$introductoryText <- TRUE
  options$dataSequenceFailures <- list()
  options$dataVariableFailures <- c("2", "3")
  options$dataSequenceSuccesses <- list()
  options$dataVariableSuccesses <- "1"
  options$dataCountsFailures <- 0
  options$dataCountsSuccesses <- 0
  options$priorAndPosteriorDistributionPlot <- TRUE
  options$priorAndPosteriorDistributionPlotObservedProportion <- TRUE
  options$priorAndPosteriorDistributionPlotType <- "marginal"
  options$sequentialAnalysisPredictivePerformancePlot <- TRUE
  options$sequentialAnalysisPredictivePerformancePlotType <- "BF"
  options$sequentialAnalysisPredictivePerformancePlotUpdatingTable <- TRUE
  options$posteriorDistributionPlot <- TRUE
  options$posteriorDistributionPlotConditionalCiBf <- 1
  options$posteriorDistributionPlotConditionalCi <- FALSE
  options$posteriorDistributionPlotConditionalCiMass <- 0.95
  options$posteriorDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorDistributionPlotJointType <- "stacked"
  options$posteriorDistributionPlotConditionalCiLower <- 0
  options$posteriorDistributionPlotMarginalCiBf <- 1
  options$posteriorDistributionPlotMarginalCi <- TRUE
  options$posteriorDistributionPlotMarginalCiMass <- 0.95
  options$posteriorDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorDistributionPlotMarginalCiLower <- 0.25
  options$posteriorDistributionPlotMarginalCiType <- "support"
  options$posteriorDistributionPlotMarginalCiUpper <- 0.75
  options$posteriorDistributionPlotObservedProportion <- TRUE
  options$posteriorDistributionPlotType <- "joint"
  options$posteriorDistributionPlotConditionalCiType <- "central"
  options$posteriorDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotConditionalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotJoinType <- "stacked"
  options$priorPredictivePerformanceDistributionPlotConditionalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCi <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimate <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimateType <- "median"
  options$priorPredictivePerformanceDistributionPlotMarginalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCiType <- "custom"
  options$priorPredictivePerformanceDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotConditionalCi <- FALSE
  options$posteriorPredictionDistributionPlotConditionalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotJoinType <- "overlying"
  options$posteriorPredictionDistributionPlotConditionalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCi <- FALSE
  options$posteriorPredictionDistributionPlotMarginalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorPredictionDistributionPlotMarginalPointEstimateType <- "mode"
  options$posteriorPredictionDistributionPlotMarginalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCiType <- "central"
  options$posteriorPredictionDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotType <- "marginal"
  options$posteriorPredictionDistributionPlotConditionalCiType <- "central"
  options$posteriorPredictionDistributionPlotConditionalCiUpper <- 1
  options$posteriorPredictionDistributionPlotIndividualCiType <- "joint"
  options$priorPredictivePerformanceDistributionPlotType <- "joint"
  options$priorPredictivePerformanceDistributionPlotConditionalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlot <- TRUE
  options$priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess <- FALSE
  options$posteriorPredictionDistributionPlot <- FALSE
  options$priorPredictivePerformanceAccuracyPlot <- FALSE
  options$priorPredictivePerformanceAccuracyPlotType <- "marginal"
  options$priorDistributionPlot <- TRUE
  options$priorDistributionPlotConditionalCi <- FALSE
  options$priorDistributionPlotConditionalCiMass <- 0.95
  options$priorDistributionPlotConditionalPointEstimate <- FALSE
  options$priorDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorDistributionPlotJointType <- "stacked"
  options$priorDistributionPlotConditionalCiLower <- 0.25
  options$priorDistributionPlotMarginalCi <- TRUE
  options$priorDistributionPlotMarginalCiMass <- 0.01
  options$priorDistributionPlotMarginalPointEstimate <- TRUE
  options$priorDistributionPlotMarginalPointEstimateType <- "median"
  options$priorDistributionPlotMarginalCiLower <- 0.25
  options$priorDistributionPlotMarginalCiType <- "central"
  options$priorDistributionPlotMarginalCiUpper <- 0.75
  options$priorDistributionPlotType <- "joint"
  options$priorDistributionPlotConditionalCiType <- "central"
  options$priorDistributionPlotConditionalCiUpper <- 0.75
  options$posteriorPredictionNumberOfFutureTrials <- 1
  options$posteriorPredictionDistributionPlotAsSampleProportion <- FALSE
  options$posteriorPredictionDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTable <- TRUE
  options$priorPredictivePerformanceDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTablePointEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""),
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$dataVariableSelected <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("LSbinomialtesting", "debug", options)


  test_that("Prior and Posterio matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot_plotsBothPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-plot-0-vol5-1", dir="LSbinomialtesting")
  })

  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-plot-1-vol5-2", dir="LSbinomialtesting")
  })

  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-prediction-plot-2-vol5-3", dir="LSbinomialtesting")
  })

  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-plot-3-vol5-4", dir="LSbinomialtesting")
  })

  test_that("Predictions table results match", {
    table <- results[["results"]][["containerPredictions"]][["collection"]][["containerPredictions_predictionsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("Hypothesis Spike", "spike at 0.5", 0.5, "binomial (1, 0.5)",
                                        0.5, 0.181526448024068, "Hypothesis Beta", "beta (21, 41)",
                                        0.338709677419355, "beta-binomial (1, 21, 41)", 0.338709677419355,
                                        0.818473551975932, "Marginal", 0.367988136778075, 0.367988136778075
                                   ))
  })

  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_sequentialAnalysisPredictivePerformancePlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "sequential-plot-5-vol5-5", dir="LSbinomialtesting")
  })

  test_that("Sequentail table results match", {
    table <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_tableIterative"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 1, 1, 1.5, 1, 2, 1.5, 1, 3, 1.875, 1, 4, 1.875, 1, 5, 1.640625,
                                        1, 6, 2.1875, 1, 7, 1.96875, 1, 8, 1.640625, 1, 9, 2.255859375,
                                        1, 10, 1.93359375, 1, 11, 1.571044921875, 1, 12, 2.199462890625,
                                        1, 13, 1.8328857421875, 1, 14, 1.46630859375, 1, 15, 2.0772705078125,
                                        1, 16, 1.6995849609375, 1, 17, 1.34550476074219, 1, 18, 1.92214965820313,
                                        1, 19, 1.55250549316406, 1, 20, 1.21982574462891, 1, 21, 1.75349950790405,
                                        1, 22, 1.40279960632324, 1, 23, 1.09593719244004, 1, 24, 1.58302038908005,
                                        1, 25, 1.25710442662239, 1, 26, 0.977747887372972, 1, 27, 1.41773443669081,
                                        1, 28, 1.11926402896643, 1, 29, 0.867429622448983, 1, 30, 1.26171581447124,
                                        1, 31, 0.991348139941694, 1, 32, 0.7660417445004, 1, 33, 1.11714421072975,
                                        1, 34, 0.874286773614583, 1, 35, 0.673929387994579, 1, 36, 0.984973720915149,
                                        1, 37, 0.768279502313816, 1, 38, 0.590984232549092, 1, 39, 0.865369769089739,
                                        1, 40, 0.673065375958685, 1, 41, 0.516818056539706, 1, 42, 0.757999816258231,
                                        1, 43, 0.588103305717597, 1, 44, 0.450879201050157, 1, 45, 0.66222882654242,
                                        1, 46, 0.512693285065102, 1, 47, 0.392530796377967, 1, 48, 0.577251171144071,
                                        1, 49, 0.446057723156778, 1, 50, 0.34110296476695, 1, 51, 0.502179364795783,
                                        1, 52, 0.387395509985322, 1, 53, 0.295927125683231, 1, 54, 0.436103132585814,
                                        1, 55, 0.335917277802586, 1, 56, 0.256357922533554, 1, 57, 0.378127935736991,
                                        1, 58, 0.290867642874606, 1, 59, 0.22178657769189, 1, 60))
  })

  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })

  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(-1.50603972126269, "Hypothesis Spike", -5.616913585436, 0.181526448024068,
                                        0.5, "Hypothesis Beta", -4.11087386417331, 0.818473551975932,
                                        0.5))
  })
}
### more options vol. 6 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$priorPredictivePerformanceBfType <- "BF01"
  options$sequentialAnalysisPredictivePerformancePlotBfType <- "LogBF10"
  options$priorPredictivePerformanceBfComparison <- "best"
  options$sequentialAnalysisPredictivePerformancePlotBfComparison <- "inclusion"
  options$priorPredictivePerformanceBfVsHypothesis <- "Hypothesis Beta"
  options$sequentialAnalysisPredictivePerformancePlotBfVsHypothesis <- "Hypothesis Spike"
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataInputType <- "variable"
  options$dataSequenceSequenceOfObservations <- ""
  options$introductoryText <- TRUE
  options$dataSequenceFailures <- list()
  options$dataVariableFailures <- c("2", "3")
  options$dataSequenceSuccesses <- list()
  options$dataVariableSuccesses <- "1"
  options$dataCountsFailures <- 0
  options$dataCountsSuccesses <- 0
  options$priorAndPosteriorDistributionPlot <- FALSE
  options$priorAndPosteriorDistributionPlotObservedProportion <- TRUE
  options$priorAndPosteriorDistributionPlotType <- "marginal"
  options$sequentialAnalysisPredictivePerformancePlot <- TRUE
  options$sequentialAnalysisPredictivePerformancePlotType <- "BF"
  options$sequentialAnalysisPredictivePerformancePlotUpdatingTable <- FALSE
  options$posteriorDistributionPlot <- FALSE
  options$posteriorDistributionPlotConditionalCiBf <- 1
  options$posteriorDistributionPlotConditionalCi <- FALSE
  options$posteriorDistributionPlotConditionalCiMass <- 0.95
  options$posteriorDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorDistributionPlotJointType <- "stacked"
  options$posteriorDistributionPlotConditionalCiLower <- 0
  options$posteriorDistributionPlotMarginalCiBf <- 1
  options$posteriorDistributionPlotMarginalCi <- TRUE
  options$posteriorDistributionPlotMarginalCiMass <- 0.95
  options$posteriorDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorDistributionPlotMarginalCiLower <- 0.25
  options$posteriorDistributionPlotMarginalCiType <- "support"
  options$posteriorDistributionPlotMarginalCiUpper <- 0.75
  options$posteriorDistributionPlotObservedProportion <- TRUE
  options$posteriorDistributionPlotType <- "joint"
  options$posteriorDistributionPlotConditionalCiType <- "central"
  options$posteriorDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotConditionalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotJoinType <- "stacked"
  options$priorPredictivePerformanceDistributionPlotConditionalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCi <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimate <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimateType <- "median"
  options$priorPredictivePerformanceDistributionPlotMarginalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCiType <- "custom"
  options$priorPredictivePerformanceDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotConditionalCi <- FALSE
  options$posteriorPredictionDistributionPlotConditionalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotJoinType <- "overlying"
  options$posteriorPredictionDistributionPlotConditionalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCi <- TRUE
  options$posteriorPredictionDistributionPlotMarginalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorPredictionDistributionPlotMarginalPointEstimateType <- "mode"
  options$posteriorPredictionDistributionPlotMarginalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCiType <- "central"
  options$posteriorPredictionDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotType <- "marginal"
  options$posteriorPredictionDistributionPlotConditionalCiType <- "central"
  options$posteriorPredictionDistributionPlotConditionalCiUpper <- 1
  options$posteriorPredictionDistributionPlotIndividualCiType <- "joint"
  options$priorPredictivePerformanceDistributionPlotConditionalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlot <- FALSE
  options$priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess <- FALSE
  options$posteriorPredictionDistributionPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlot <- FALSE
  options$priorPredictivePerformanceAccuracyPlotType <- "marginal"
  options$priorDistributionPlot <- FALSE
  options$priorDistributionPlotConditionalCi <- FALSE
  options$priorDistributionPlotConditionalCiMass <- 0.95
  options$priorDistributionPlotConditionalPointEstimate <- FALSE
  options$priorDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorDistributionPlotJointType <- "stacked"
  options$priorDistributionPlotConditionalCiLower <- 0.25
  options$priorDistributionPlotMarginalCi <- TRUE
  options$priorDistributionPlotMarginalCiMass <- 0.01
  options$priorDistributionPlotMarginalPointEstimate <- TRUE
  options$priorDistributionPlotMarginalPointEstimateType <- "median"
  options$priorDistributionPlotMarginalCiLower <- 0.25
  options$priorDistributionPlotMarginalCiType <- "central"
  options$priorDistributionPlotMarginalCiUpper <- 0.75
  options$priorDistributionPlotType <- "joint"
  options$priorDistributionPlotConditionalCiType <- "central"
  options$priorDistributionPlotConditionalCiUpper <- 0.75
  options$posteriorPredictionNumberOfFutureTrials <- 10
  options$posteriorPredictionDistributionPlotAsSampleProportion <- TRUE
  options$posteriorPredictionDistributionPlotPredictionsTable <- TRUE
  options$posteriorPredictionSummaryTable <- TRUE
  options$priorPredictivePerformanceDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTablePointEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""),
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$dataVariableSelected <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("LSbinomialtesting", "debug", options)


  test_that("Prediction Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_priorPredictivePerformanceDistributionPlotPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "titleless-plot-0-vol6-1", dir="LSbinomialtesting")
  })

  test_that("Prediction table results match", {
    table <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_tablePredictions"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.0183858383377217, 0, 0.0782486981654427, 0.1, 0.162489928642561,
                                        0.2, 0.218705515050819, 0.3, 0.213656489678812, 0.4, 0.159735167414158,
                                        0.5, 0.0926276354595948, 0.6, 0.0406987911504769, 0.7, 0.0127208338223604,
                                        0.8, 0.00250057277861945, 0.9, 0.000230529499432729, 1))
  })

  test_that("Prediction summary table match", {
    table <- results[["results"]][["containerPredictions"]][["collection"]][["containerPredictions_predictionsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("Hypothesis Spike", "spike at 0.5", 0.5, "binomial (10, 0.5)",
                                        5, 0.181526448024068, "Hypothesis Beta", "beta (21, 41)", 0.338709677419355,
                                        "beta-binomial (10, 21, 41)", 3.38709677419355, 0.818473551975932,
                                        "Marginal", 0.367988136778075, 3.67988136778075))
  })

  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_sequentialAnalysisPredictivePerformancePlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "sequential-plot-3-vol6-2", dir="LSbinomialtesting")
  })

  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })

  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(4.50883912997304, "Hypothesis Spike", -5.616913585436, 0.181526448024068,
                                        0.5, 1, "Hypothesis Beta", -4.11087386417331, 0.818473551975932,
                                        0.5))
  })
}
### more options vol. 7 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$priorPredictivePerformanceBfType <- "BF01"
  options$sequentialAnalysisPredictivePerformancePlotBfType <- "LogBF10"
  options$priorPredictivePerformanceBfComparison <- "best"
  options$sequentialAnalysisPredictivePerformancePlotBfComparison <- "inclusion"
  options$priorPredictivePerformanceBfVsHypothesis <- "Hypothesis Beta"
  options$sequentialAnalysisPredictivePerformancePlotBfVsHypothesis <- "Hypothesis Spike"
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataInputType <- "variable"
  options$dataSequenceSequenceOfObservations <- ""
  options$introductoryText <- TRUE
  options$dataSequenceFailures <- list()
  options$dataVariableFailures <- c("2", "3")
  options$dataSequenceSuccesses <- list()
  options$dataVariableSuccesses <- "1"
  options$dataCountsFailures <- 0
  options$dataCountsSuccesses <- 0
  options$priorAndPosteriorDistributionPlot <- FALSE
  options$priorAndPosteriorDistributionPlotObservedProportion <- TRUE
  options$priorAndPosteriorDistributionPlotType <- "marginal"
  options$sequentialAnalysisPredictivePerformancePlot <- FALSE
  options$sequentialAnalysisPredictivePerformancePlotType <- "BF"
  options$sequentialAnalysisPredictivePerformancePlotUpdatingTable <- FALSE
  options$posteriorDistributionPlot <- FALSE
  options$posteriorDistributionPlotConditionalCiBf <- 1
  options$posteriorDistributionPlotConditionalCi <- FALSE
  options$posteriorDistributionPlotConditionalCiMass <- 0.95
  options$posteriorDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorDistributionPlotJointType <- "stacked"
  options$posteriorDistributionPlotConditionalCiLower <- 0
  options$posteriorDistributionPlotMarginalCiBf <- 1
  options$posteriorDistributionPlotMarginalCi <- TRUE
  options$posteriorDistributionPlotMarginalCiMass <- 0.95
  options$posteriorDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorDistributionPlotMarginalCiLower <- 0.25
  options$posteriorDistributionPlotMarginalCiType <- "support"
  options$posteriorDistributionPlotMarginalCiUpper <- 0.75
  options$posteriorDistributionPlotObservedProportion <- TRUE
  options$posteriorDistributionPlotType <- "joint"
  options$posteriorDistributionPlotConditionalCiType <- "central"
  options$posteriorDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotConditionalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotJoinType <- "stacked"
  options$priorPredictivePerformanceDistributionPlotConditionalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCi <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimate <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimateType <- "median"
  options$priorPredictivePerformanceDistributionPlotMarginalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCiType <- "custom"
  options$priorPredictivePerformanceDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotConditionalCi <- FALSE
  options$posteriorPredictionDistributionPlotConditionalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotJoinType <- "overlying"
  options$posteriorPredictionDistributionPlotConditionalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCi <- TRUE
  options$posteriorPredictionDistributionPlotMarginalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotMarginalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotMarginalPointEstimateType <- "mode"
  options$posteriorPredictionDistributionPlotMarginalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCiType <- "HPD"
  options$posteriorPredictionDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotType <- "marginal"
  options$posteriorPredictionDistributionPlotConditionalCiType <- "central"
  options$posteriorPredictionDistributionPlotConditionalCiUpper <- 1
  options$posteriorPredictionDistributionPlotIndividualCiType <- "joint"
  options$priorPredictivePerformanceDistributionPlotConditionalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlot <- FALSE
  options$priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess <- FALSE
  options$posteriorPredictionDistributionPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlot <- FALSE
  options$priorPredictivePerformanceAccuracyPlotType <- "marginal"
  options$priorDistributionPlot <- FALSE
  options$priorDistributionPlotConditionalCi <- FALSE
  options$priorDistributionPlotConditionalCiMass <- 0.95
  options$priorDistributionPlotConditionalPointEstimate <- FALSE
  options$priorDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorDistributionPlotJointType <- "stacked"
  options$priorDistributionPlotConditionalCiLower <- 0.25
  options$priorDistributionPlotMarginalCi <- TRUE
  options$priorDistributionPlotMarginalCiMass <- 0.01
  options$priorDistributionPlotMarginalPointEstimate <- TRUE
  options$priorDistributionPlotMarginalPointEstimateType <- "median"
  options$priorDistributionPlotMarginalCiLower <- 0.25
  options$priorDistributionPlotMarginalCiType <- "central"
  options$priorDistributionPlotMarginalCiUpper <- 0.75
  options$priorDistributionPlotType <- "joint"
  options$priorDistributionPlotConditionalCiType <- "central"
  options$priorDistributionPlotConditionalCiUpper <- 0.75
  options$posteriorPredictionNumberOfFutureTrials <- 10
  options$posteriorPredictionDistributionPlotAsSampleProportion <- TRUE
  options$posteriorPredictionDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTable <- FALSE
  options$priorPredictivePerformanceDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTablePointEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""),
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1",
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$dataVariableSelected <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("LSbinomialtesting", "debug", options)


  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_priorPredictivePerformanceDistributionPlotPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-prediction-plot-0-vol7-1", dir="LSbinomialtesting")
  })

  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })

  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(4.50883912997304, "Hypothesis Spike", -5.616913585436, 0.181526448024068,
                                        0.5, 1, "Hypothesis Beta", -4.11087386417331, 0.818473551975932,
                                        0.5))
  })
}
### output for all default settings (spike only)
{
  options <- analysisOptions("LSbinomialtesting")
  options$priorPredictivePerformanceBfType <- "BF10"
  options$sequentialAnalysisPredictivePerformancePlotBfType <- "BF10"
  options$priorPredictivePerformanceBfComparison <- "inclusion"
  options$sequentialAnalysisPredictivePerformancePlotBfComparison <- "inclusion"
  options$priorPredictivePerformanceBfVsHypothesis <- ""
  options$sequentialAnalysisPredictivePerformancePlotBfVsHypothesis <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataInputType <- "variable"
  options$dataSequenceSequenceOfObservations <- ""
  options$introductoryText <- TRUE
  options$dataSequenceFailures <- list()
  options$dataVariableFailures <- "4"
  options$dataSequenceSuccesses <- list()
  options$dataVariableSuccesses <- "1"
  options$dataCountsFailures <- 0
  options$dataCountsSuccesses <- 0
  options$priorAndPosteriorDistributionPlot <- TRUE
  options$priorAndPosteriorDistributionPlotObservedProportion <- FALSE
  options$priorAndPosteriorDistributionPlotType <- "joint"
  options$sequentialAnalysisPredictivePerformancePlot <- TRUE
  options$sequentialAnalysisPredictivePerformancePlotType <- "joint"
  options$sequentialAnalysisPredictivePerformancePlotUpdatingTable <- FALSE
  options$posteriorDistributionPlot <- TRUE
  options$posteriorDistributionPlotConditionalCiBf <- 1
  options$posteriorDistributionPlotConditionalCi <- FALSE
  options$posteriorDistributionPlotConditionalCiMass <- 0.95
  options$posteriorDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorDistributionPlotJointType <- "overlying"
  options$posteriorDistributionPlotConditionalCiLower <- 0
  options$posteriorDistributionPlotMarginalCiBf <- 1
  options$posteriorDistributionPlotMarginalCi <- FALSE
  options$posteriorDistributionPlotMarginalCiMass <- 0.95
  options$posteriorDistributionPlotMarginalPointEstimate <- FALSE
  options$posteriorDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorDistributionPlotMarginalCiLower <- 0.25
  options$posteriorDistributionPlotMarginalCiType <- "central"
  options$posteriorDistributionPlotMarginalCiUpper <- 0.75
  options$posteriorDistributionPlotObservedProportion <- FALSE
  options$posteriorDistributionPlotType <- "marginal"
  options$posteriorDistributionPlotConditionalCiType <- "central"
  options$posteriorDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotConditionalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotJoinType <- "stacked"
  options$priorPredictivePerformanceDistributionPlotConditionalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotMarginalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotMarginalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotConditionalCi <- FALSE
  options$posteriorPredictionDistributionPlotConditionalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotJoinType <- "overlying"
  options$posteriorPredictionDistributionPlotConditionalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCi <- TRUE
  options$posteriorPredictionDistributionPlotMarginalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorPredictionDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotMarginalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCiType <- "central"
  options$posteriorPredictionDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotType <- "marginal"
  options$posteriorPredictionDistributionPlotConditionalCiType <- "central"
  options$posteriorPredictionDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotType <- "joint"
  options$priorPredictivePerformanceDistributionPlotConditionalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlot <- TRUE
  options$priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess <- FALSE
  options$posteriorPredictionDistributionPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlotType <- "conditional"
  options$priorDistributionPlot <- TRUE
  options$priorDistributionPlotConditionalCi <- FALSE
  options$priorDistributionPlotConditionalCiMass <- 0.95
  options$priorDistributionPlotConditionalPointEstimate <- FALSE
  options$priorDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorDistributionPlotJointType <- "overlying"
  options$priorDistributionPlotConditionalCiLower <- 0.25
  options$priorDistributionPlotMarginalCi <- FALSE
  options$priorDistributionPlotMarginalCiMass <- 0.95
  options$priorDistributionPlotMarginalPointEstimate <- FALSE
  options$priorDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorDistributionPlotMarginalCiLower <- 0.25
  options$priorDistributionPlotMarginalCiType <- "central"
  options$priorDistributionPlotMarginalCiUpper <- 0.75
  options$priorDistributionPlotType <- "joint"
  options$priorDistributionPlotConditionalCiType <- "central"
  options$priorDistributionPlotConditionalCiUpper <- 0.75
  options$posteriorPredictionNumberOfFutureTrials <- 1
  options$posteriorPredictionDistributionPlotAsSampleProportion <- FALSE
  options$posteriorPredictionDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTable <- FALSE
  options$priorPredictivePerformanceDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTablePointEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis 1", parAlpha = "1", parBeta = "1",
                              parPoint = "0.5", type = "spike", value = ""))
  options$dataVariableSelected <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("LSbinomialtesting", "debug", options)


  test_that("Hypothesis 1 plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot_plotsBoth_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "hypothesis-1-spike-1", dir="LSbinomialtesting")
  })

  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-plot-1-spike-2", dir="LSbinomialtesting")
  })

  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_priorPredictivePerformanceDistributionPlotPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-prediction-plot-2-spike-3", dir="LSbinomialtesting")
  })

  test_that("Prior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-prediction-plot-3-spike-4", dir="LSbinomialtesting")
  })

  test_that("Prior matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-prediction-plot-4-spike-5", dir="LSbinomialtesting")
  })

  test_that("Predictive Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "predictive-accuracy-plot-5-spike-6", dir="LSbinomialtesting")
  })

  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_sequentialAnalysisPredictivePerformancePlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "sequential-plot-6-spike-7", dir="LSbinomialtesting")
  })

  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, 0.5, "Successes", 20, 0.5, "Failures", 40, "", "Total"))
  })

  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("NaN", "Hypothesis 1", -2.07648042914739, 1, 1))
  })
}
### output for all default settings (beta only)
{
  options <- analysisOptions("LSbinomialtesting")
  options$priorPredictivePerformanceBfType <- "BF10"
  options$sequentialAnalysisPredictivePerformancePlotBfType <- "BF10"
  options$priorPredictivePerformanceBfComparison <- "inclusion"
  options$sequentialAnalysisPredictivePerformancePlotBfComparison <- "inclusion"
  options$priorPredictivePerformanceBfVsHypothesis <- ""
  options$sequentialAnalysisPredictivePerformancePlotBfVsHypothesis <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataInputType <- "variable"
  options$dataSequenceSequenceOfObservations <- ""
  options$introductoryText <- TRUE
  options$dataSequenceFailures <- list()
  options$dataVariableFailures <- "4"
  options$dataSequenceSuccesses <- list()
  options$dataVariableSuccesses <- "1"
  options$dataCountsFailures <- 0
  options$dataCountsSuccesses <- 0
  options$priorAndPosteriorDistributionPlot <- TRUE
  options$priorAndPosteriorDistributionPlotObservedProportion <- FALSE
  options$priorAndPosteriorDistributionPlotType <- "marginal"
  options$sequentialAnalysisPredictivePerformancePlot <- TRUE
  options$sequentialAnalysisPredictivePerformancePlotType <- "marginal"
  options$sequentialAnalysisPredictivePerformancePlotUpdatingTable <- FALSE
  options$posteriorDistributionPlot <- TRUE
  options$posteriorDistributionPlotConditionalCiBf <- 1
  options$posteriorDistributionPlotConditionalCi <- FALSE
  options$posteriorDistributionPlotConditionalCiMass <- 0.95
  options$posteriorDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorDistributionPlotJointType <- "stacked"
  options$posteriorDistributionPlotConditionalCiLower <- 0
  options$posteriorDistributionPlotMarginalCiBf <- 1
  options$posteriorDistributionPlotMarginalCi <- FALSE
  options$posteriorDistributionPlotMarginalCiMass <- 0.95
  options$posteriorDistributionPlotMarginalPointEstimate <- FALSE
  options$posteriorDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorDistributionPlotMarginalCiLower <- 0.25
  options$posteriorDistributionPlotMarginalCiType <- "central"
  options$posteriorDistributionPlotMarginalCiUpper <- 0.75
  options$posteriorDistributionPlotObservedProportion <- FALSE
  options$posteriorDistributionPlotType <- "joint"
  options$posteriorDistributionPlotConditionalCiType <- "central"
  options$posteriorDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotConditionalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotJoinType <- "stacked"
  options$priorPredictivePerformanceDistributionPlotConditionalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCi <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotMarginalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCiType <- "HPD"
  options$priorPredictivePerformanceDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotConditionalCi <- FALSE
  options$posteriorPredictionDistributionPlotConditionalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotJoinType <- "overlying"
  options$posteriorPredictionDistributionPlotConditionalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCi <- TRUE
  options$posteriorPredictionDistributionPlotMarginalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorPredictionDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotMarginalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCiType <- "central"
  options$posteriorPredictionDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotType <- "joint"
  options$posteriorPredictionDistributionPlotConditionalCiType <- "central"
  options$posteriorPredictionDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotType <- "marginal"
  options$priorPredictivePerformanceDistributionPlotConditionalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlot <- TRUE
  options$priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess <- FALSE
  options$posteriorPredictionDistributionPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlot <- TRUE
  options$priorPredictivePerformanceAccuracyPlotType <- "marginal"
  options$priorDistributionPlot <- TRUE
  options$priorDistributionPlotConditionalCi <- FALSE
  options$priorDistributionPlotConditionalCiMass <- 0.95
  options$priorDistributionPlotConditionalPointEstimate <- FALSE
  options$priorDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorDistributionPlotJointType <- "overlying"
  options$priorDistributionPlotConditionalCiLower <- 0.25
  options$priorDistributionPlotMarginalCi <- FALSE
  options$priorDistributionPlotMarginalCiMass <- 0.95
  options$priorDistributionPlotMarginalPointEstimate <- TRUE
  options$priorDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorDistributionPlotMarginalCiLower <- 0.25
  options$priorDistributionPlotMarginalCiType <- "central"
  options$priorDistributionPlotMarginalCiUpper <- 0.75
  options$priorDistributionPlotType <- "marginal"
  options$priorDistributionPlotConditionalCiType <- "central"
  options$priorDistributionPlotConditionalCiUpper <- 0.75
  options$posteriorPredictionNumberOfFutureTrials <- 10
  options$posteriorPredictionDistributionPlotAsSampleProportion <- FALSE
  options$posteriorPredictionDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTable <- FALSE
  options$priorPredictivePerformanceDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTablePointEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis 1", parAlpha = "1", parBeta = "1",
                              parPoint = "0.5", type = "beta", value = ""))
  options$dataVariableSelected <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("LSbinomialtesting", "debug", options)


  test_that("Prior and Posterior matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot"]][["collection"]][["containerBoth_priorAndPosteriorDistributionPlot_plotsBothPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-plot-0-beta-1", dir="LSbinomialtesting")
  })

  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-plot-1-beta-2", dir="LSbinomialtesting")
  })

  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_priorPredictivePerformanceDistributionPlotPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "posterior-prediction-plot-2-beta-3", dir="LSbinomialtesting")
  })

  test_that("Prior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-prediction-plot-3-beta-4", dir="LSbinomialtesting")
  })

  test_that("Prior plor matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-plot-4-beta-5", dir="LSbinomialtesting")
  })

  test_that("Predictive-Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "predictive-accuracy-plot-5-beta-6", dir="LSbinomialtesting")
  })

  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_sequentialAnalysisPredictivePerformancePlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "sequential-plot-6-beta-7", dir="LSbinomialtesting")
  })

  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, 0.5, "Successes", 20, 0.5, "Failures", 40, "", "Total"))
  })

  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("NaN", "Hypothesis 1", -3.71357206670431, 1, 1))
  })
}
### some challenging plots
{
  options <- analysisOptions("LSbinomialtesting")
  options$priorPredictivePerformanceBfType <- "BF10"
  options$sequentialAnalysisPredictivePerformancePlotBfType <- "BF10"
  options$priorPredictivePerformanceBfComparison <- "inclusion"
  options$sequentialAnalysisPredictivePerformancePlotBfComparison <- "inclusion"
  options$priorPredictivePerformanceBfVsHypothesis <- ""
  options$sequentialAnalysisPredictivePerformancePlotBfVsHypothesis <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataInputType <- "variable"
  options$dataSequenceSequenceOfObservations <- ""
  options$introductoryText <- TRUE
  options$dataSequenceFailures <- list()
  options$dataVariableFailures <- "4"
  options$dataSequenceSuccesses <- list()
  options$dataVariableSuccesses <- "1"
  options$dataCountsFailures <- 0
  options$dataCountsSuccesses <- 0
  options$priorAndPosteriorDistributionPlot <- FALSE
  options$priorAndPosteriorDistributionPlotObservedProportion <- FALSE
  options$priorAndPosteriorDistributionPlotType <- "marginal"
  options$sequentialAnalysisPredictivePerformancePlot <- FALSE
  options$sequentialAnalysisPredictivePerformancePlotType <- "marginal"
  options$sequentialAnalysisPredictivePerformancePlotUpdatingTable <- FALSE
  options$posteriorDistributionPlot <- FALSE
  options$posteriorDistributionPlotConditionalCiBf <- 1
  options$posteriorDistributionPlotConditionalCi <- FALSE
  options$posteriorDistributionPlotConditionalCiMass <- 0.95
  options$posteriorDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorDistributionPlotJointType <- "stacked"
  options$posteriorDistributionPlotConditionalCiLower <- 0
  options$posteriorDistributionPlotMarginalCiBf <- 1
  options$posteriorDistributionPlotMarginalCi <- FALSE
  options$posteriorDistributionPlotMarginalCiMass <- 0.95
  options$posteriorDistributionPlotMarginalPointEstimate <- FALSE
  options$posteriorDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorDistributionPlotMarginalCiLower <- 0.25
  options$posteriorDistributionPlotMarginalCiType <- "central"
  options$posteriorDistributionPlotMarginalCiUpper <- 0.75
  options$posteriorDistributionPlotObservedProportion <- FALSE
  options$posteriorDistributionPlotType <- "joint"
  options$posteriorDistributionPlotConditionalCiType <- "central"
  options$posteriorDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotConditionalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotJoinType <- "stacked"
  options$priorPredictivePerformanceDistributionPlotConditionalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCi <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimate <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimateType <- "median"
  options$priorPredictivePerformanceDistributionPlotMarginalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCiType <- "HPD"
  options$priorPredictivePerformanceDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotConditionalCi <- FALSE
  options$posteriorPredictionDistributionPlotConditionalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotJoinType <- "overlying"
  options$posteriorPredictionDistributionPlotConditionalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCi <- TRUE
  options$posteriorPredictionDistributionPlotMarginalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorPredictionDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotMarginalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCiType <- "central"
  options$posteriorPredictionDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotType <- "joint"
  options$posteriorPredictionDistributionPlotConditionalCiType <- "central"
  options$posteriorPredictionDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotType <- "marginal"
  options$priorPredictivePerformanceDistributionPlotConditionalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlot <- TRUE
  options$priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess <- FALSE
  options$posteriorPredictionDistributionPlot <- FALSE
  options$priorPredictivePerformanceAccuracyPlot <- FALSE
  options$priorPredictivePerformanceAccuracyPlotType <- "marginal"
  options$priorDistributionPlot <- TRUE
  options$priorDistributionPlotConditionalCi <- FALSE
  options$priorDistributionPlotConditionalCiMass <- 0.95
  options$priorDistributionPlotConditionalPointEstimate <- FALSE
  options$priorDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorDistributionPlotJointType <- "overlying"
  options$priorDistributionPlotConditionalCiLower <- 0.25
  options$priorDistributionPlotMarginalCi <- TRUE
  options$priorDistributionPlotMarginalCiMass <- 0.95
  options$priorDistributionPlotMarginalPointEstimate <- TRUE
  options$priorDistributionPlotMarginalPointEstimateType <- "mean"
  options$priorDistributionPlotMarginalCiLower <- 0.25
  options$priorDistributionPlotMarginalCiType <- "central"
  options$priorDistributionPlotMarginalCiUpper <- 0.75
  options$priorDistributionPlotType <- "marginal"
  options$priorDistributionPlotConditionalCiType <- "central"
  options$priorDistributionPlotConditionalCiUpper <- 0.75
  options$posteriorPredictionNumberOfFutureTrials <- 10
  options$posteriorPredictionDistributionPlotAsSampleProportion <- FALSE
  options$posteriorPredictionDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTable <- FALSE
  options$priorPredictivePerformanceDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTablePointEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis 1", parAlpha = ".1", parBeta = "1",
                              parPoint = "0.5", type = "beta", value = ""), list(PH = "1",
                                                                                 name = "Hypothesis 2", parAlpha = "1", parBeta = ".1", parPoint = "0.5",
                                                                                 type = "beta", value = "2"))
  options$dataVariableSelected <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("LSbinomialtesting", "debug", options)


  test_that("Prior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-prediction-plot-0-hard-1", dir="LSbinomialtesting")
  })

  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-plot-1-hard-2", dir="LSbinomialtesting")
  })

  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, 0.5, "Successes", 20, 0.5, "Failures", 40, "", "Total"))
  })

  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, "Hypothesis 1", -5.37126513656346, 0.5, 0.5, 1, "Hypothesis 2",
                                        -5.37126513656346, 0.5, 0.5))
  })
}
{
  options <- analysisOptions("LSbinomialtesting")
  options$priorPredictivePerformanceBfType <- "BF10"
  options$sequentialAnalysisPredictivePerformancePlotBfType <- "BF10"
  options$priorPredictivePerformanceBfComparison <- "inclusion"
  options$sequentialAnalysisPredictivePerformancePlotBfComparison <- "inclusion"
  options$priorPredictivePerformanceBfVsHypothesis <- ""
  options$sequentialAnalysisPredictivePerformancePlotBfVsHypothesis <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataInputType <- "variable"
  options$dataSequenceSequenceOfObservations <- ""
  options$introductoryText <- TRUE
  options$dataSequenceFailures <- list()
  options$dataVariableFailures <- "4"
  options$dataSequenceSuccesses <- list()
  options$dataVariableSuccesses <- "1"
  options$dataCountsFailures <- 0
  options$dataCountsSuccesses <- 0
  options$priorAndPosteriorDistributionPlot <- FALSE
  options$priorAndPosteriorDistributionPlotObservedProportion <- FALSE
  options$priorAndPosteriorDistributionPlotType <- "marginal"
  options$sequentialAnalysisPredictivePerformancePlot <- FALSE
  options$sequentialAnalysisPredictivePerformancePlotType <- "marginal"
  options$sequentialAnalysisPredictivePerformancePlotUpdatingTable <- FALSE
  options$posteriorDistributionPlot <- FALSE
  options$posteriorDistributionPlotConditionalCiBf <- 1
  options$posteriorDistributionPlotConditionalCi <- FALSE
  options$posteriorDistributionPlotConditionalCiMass <- 0.95
  options$posteriorDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorDistributionPlotJointType <- "stacked"
  options$posteriorDistributionPlotConditionalCiLower <- 0
  options$posteriorDistributionPlotMarginalCiBf <- 1
  options$posteriorDistributionPlotMarginalCi <- FALSE
  options$posteriorDistributionPlotMarginalCiMass <- 0.95
  options$posteriorDistributionPlotMarginalPointEstimate <- FALSE
  options$posteriorDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorDistributionPlotMarginalCiLower <- 0.25
  options$posteriorDistributionPlotMarginalCiType <- "central"
  options$posteriorDistributionPlotMarginalCiUpper <- 0.75
  options$posteriorDistributionPlotObservedProportion <- FALSE
  options$posteriorDistributionPlotType <- "joint"
  options$posteriorDistributionPlotConditionalCiType <- "central"
  options$posteriorDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotConditionalCi <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimate <- FALSE
  options$priorPredictivePerformanceDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorPredictivePerformanceDistributionPlotJoinType <- "stacked"
  options$priorPredictivePerformanceDistributionPlotConditionalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCi <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalCiMass <- 0.95
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimate <- TRUE
  options$priorPredictivePerformanceDistributionPlotMarginalPointEstimateType <- "median"
  options$priorPredictivePerformanceDistributionPlotMarginalCiLower <- 0
  options$priorPredictivePerformanceDistributionPlotMarginalCiType <- "HPD"
  options$priorPredictivePerformanceDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotConditionalCi <- FALSE
  options$posteriorPredictionDistributionPlotConditionalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotConditionalPointEstimate <- FALSE
  options$posteriorPredictionDistributionPlotConditionalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotJoinType <- "overlying"
  options$posteriorPredictionDistributionPlotConditionalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCi <- TRUE
  options$posteriorPredictionDistributionPlotMarginalCiMass <- 0.95
  options$posteriorPredictionDistributionPlotMarginalPointEstimate <- TRUE
  options$posteriorPredictionDistributionPlotMarginalPointEstimateType <- "mean"
  options$posteriorPredictionDistributionPlotMarginalCiLower <- 0
  options$posteriorPredictionDistributionPlotMarginalCiType <- "central"
  options$posteriorPredictionDistributionPlotMarginalCiUpper <- 1
  options$posteriorPredictionDistributionPlotType <- "joint"
  options$posteriorPredictionDistributionPlotConditionalCiType <- "central"
  options$posteriorPredictionDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlotType <- "marginal"
  options$priorPredictivePerformanceDistributionPlotConditionalCiType <- "central"
  options$priorPredictivePerformanceDistributionPlotConditionalCiUpper <- 1
  options$priorPredictivePerformanceDistributionPlot <- TRUE
  options$priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess <- FALSE
  options$posteriorPredictionDistributionPlot <- FALSE
  options$priorPredictivePerformanceAccuracyPlot <- FALSE
  options$priorPredictivePerformanceAccuracyPlotType <- "marginal"
  options$priorDistributionPlot <- TRUE
  options$priorDistributionPlotConditionalCi <- FALSE
  options$priorDistributionPlotConditionalCiMass <- 0.95
  options$priorDistributionPlotConditionalPointEstimate <- FALSE
  options$priorDistributionPlotConditionalPointEstimateType <- "mean"
  options$priorDistributionPlotJointType <- "overlying"
  options$priorDistributionPlotConditionalCiLower <- 0.25
  options$priorDistributionPlotMarginalCi <- TRUE
  options$priorDistributionPlotMarginalCiMass <- 0.95
  options$priorDistributionPlotMarginalPointEstimate <- TRUE
  options$priorDistributionPlotMarginalPointEstimateType <- "mode"
  options$priorDistributionPlotMarginalCiLower <- 0.25
  options$priorDistributionPlotMarginalCiType <- "HPD"
  options$priorDistributionPlotMarginalCiUpper <- 0.75
  options$priorDistributionPlotType <- "marginal"
  options$priorDistributionPlotConditionalCiType <- "central"
  options$priorDistributionPlotConditionalCiUpper <- 0.75
  options$posteriorPredictionNumberOfFutureTrials <- 10
  options$posteriorPredictionDistributionPlotAsSampleProportion <- FALSE
  options$posteriorPredictionDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTable <- FALSE
  options$priorPredictivePerformanceDistributionPlotPredictionsTable <- FALSE
  options$posteriorPredictionSummaryTablePointEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis 1", parAlpha = ".1", parBeta = "1",
                              parPoint = "0.5", type = "beta", value = ""), list(PH = "1",
                                                                                 name = "Hypothesis 2", parAlpha = "1", parBeta = ".1", parPoint = "0.5",
                                                                                 type = "beta", value = "2"), list(PH = "1", name = "Hypothesis 3",
                                                                                                                   parAlpha = "1", parBeta = "1", parPoint = "0.3", type = "spike",
                                                                                                                   value = "3"), list(PH = "1", name = "Hypothesis 4", parAlpha = "1",
                                                                                                                                      parBeta = "1", parPoint = "0.5", type = "spike", value = "4"),
                         list(PH = "1", name = "Hypothesis 5", parAlpha = "1", parBeta = "1",
                              parPoint = "0.6", type = "spike", value = "5"))
  options$dataVariableSelected <- "facFive"
  set.seed(1)
  results <- jaspTools::runAnalysis("LSbinomialtesting", "debug", options)


  test_that("Prior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_priorPredictivePerformanceDistributionPlotPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-prediction-plot-0-hard-3", dir="LSbinomialtesting")
  })

  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-plot-1-hard-4", dir="LSbinomialtesting")
  })

  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, 0.5, "Successes", 20, 0.5, "Failures", 40, "", "Total"))
  })

  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.0982361798125317, "Hypothesis 1", -5.37126513656346, 0.0239703559049214,
                                        0.2, 0.0982361798125317, "Hypothesis 2", -5.37126513656346,
                                        0.0239703559049214, 0.2, 0.0807053373343048, "Hypothesis 3",
                                        -5.56354817204294, 0.0197773008984337, 0.2, 7.31602582186271,
                                        "Hypothesis 4", -2.07648042914739, 0.646519010917071, 0.2, 1.60038176080074,
                                        "Hypothesis 5", -2.89292031955249, 0.285762976374653, 0.2))
  })
}
