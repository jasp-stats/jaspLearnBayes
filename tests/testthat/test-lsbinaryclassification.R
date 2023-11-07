context("Learn Bayes - Binary Classification")

# Point estimates ----
options <- jaspTools::analysisOptions("LSbinaryclassification")
options$introductoryText   <- FALSE
options$inputType   <- "pointEstimates"
options$prevalence  <- 0.1
options$sensitivity <- 0.8
options$specificity <- 0.8

options$probabilityPositivePlot                                 <- TRUE
options$iconPlot                                                <- TRUE
options$rocPlot                                                 <- TRUE
options$testCharacteristicsPlot                                 <- TRUE
options$predictiveValuesByPrevalence                            <- TRUE
options$alluvialPlot                                            <- TRUE
options$signalDetectionPlot                                     <- TRUE
options$estimatesPlot                                           <- TRUE
options$estimatesPlotPrevalence                                 <- TRUE
options$estimatesPlotSensitivity                                <- TRUE
options$estimatesPlotSpecificity                                <- TRUE
options$estimatesPlotTruePositive                               <- TRUE
options$estimatesPlotTrueNegative                               <- TRUE
options$estimatesPlotFalsePositive                              <- TRUE
options$estimatesPlotFalseNegative                              <- TRUE
options$estimatesPlotPositivePredictiveValue                    <- TRUE
options$estimatesPlotNegativePredictiveValue                    <- TRUE
options$estimatesPlotFalseDiscoveryRate                         <- TRUE
options$estimatesPlotFalseOmissionRate                          <- TRUE
options$estimatesPlotFalsePositiveRate                          <- TRUE
options$estimatesPlotFalseNegativeRate                          <- TRUE
options$estimatesPlotAccuracy                                   <- TRUE

options$statistics                    <- FALSE
options$confusionMatrix               <- TRUE
options$confusionMatrixAdditionalInfo <- TRUE
options$confusionMatrixType           <- "both"

options$colorTruePositive <- "darkgreen"
options$colorFalseNegative <- "red"
options$colorFalsePositive <- "darkorange"
options$colorTrueNegative <- "steelblue"

set.seed(1)
results <- jaspTools::runAnalysis(name    = "LSbinaryclassification",
                                  dataset = "binaryClassification.csv",
                                  options = options)

test_that("Alluvial plot matches", {
  testthat::skip_on_os(c("windows", "linux")) # see https://github.com/jasp-stats/jaspLearnBayes/pull/120
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_alluvialPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "alluvial-plot", dir="LSbinaryclassification")
})

test_that("Estimates plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_estimatesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "estimates", dir="LSbinaryclassification")
})

test_that("Icon plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_iconPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "icon-plot", dir="LSbinaryclassification")
})

test_that("Probability positive plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_probabilityPositivePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-positive", dir="LSbinaryclassification")
})

test_that("Receiving Operating Characteristic Curve plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_rocPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "receiving-operating-characteristic-curve", dir="LSbinaryclassification")
})

test_that("Signal detection plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_signalDetectionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "signal-detection", dir="LSbinaryclassification")
})

test_that("Test characteristics plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_testCharacteristicsPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-characteristics", dir="LSbinaryclassification")
})

test_that("PPV and NPV by prevalence plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_predictiveValuesByPrevalence"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "ppv-and-npv-by-prevalence", dir="LSbinaryclassification")
})

test_that("Confusion matrix table results match", {
  table <- results[["results"]][["tables"]][["collection"]][["tables_confusionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Sensitivity", 0.8, "False negative rate", 0.2, "Positive condition",
                                      "False negative", 0.02, "True positive", 0.08, "Prevalence",
                                      0.1, "False positive rate", 0.2, "Specificity", 0.8, "Negative condition",
                                      "True negative", 0.72, "False positive", 0.18, "Rareness", 0.9,
                                      "Accuracy", 0.8, "", "", "True/Total", "Negative predictive value",
                                      0.972972972972973, "Positive predictive value", 0.307692307692308,
                                      "", ""))
})


# Using data ----
options <- jaspTools::analysisOptions("LSbinaryclassification")
options$introductoryText <- FALSE
options$inputType <- "data"
options$marker    <- "marker"
options$labels    <- "condition"
options$threshold <- "0"
options$priorPrevalenceAlpha  <- 1
options$priorPrevalenceBeta   <- 9
options$priorSensitivityAlpha <- 8
options$priorSensitivityBeta  <- 2
options$priorSpecificityAlpha <- 8
options$priorSpecificityBeta  <- 2

options$probabilityPositivePlot                 <- TRUE
options$iconPlot                                <- FALSE
options$rocPlot                                 <- TRUE
options$testCharacteristicsPlot                 <- TRUE
options$predictiveValuesByPrevalence            <- TRUE
options$alluvialPlot                            <- TRUE
options$signalDetectionPlot                     <- TRUE
options$estimatesPlot                           <- TRUE
options$estimatesPlotPrevalence                 <- TRUE
options$estimatesPlotSensitivity                <- TRUE
options$estimatesPlotSpecificity                <- TRUE
options$estimatesPlotTruePositive               <- TRUE
options$estimatesPlotTrueNegative               <- TRUE
options$estimatesPlotFalsePositive              <- TRUE
options$estimatesPlotFalseNegative              <- TRUE
options$estimatesPlotPositivePredictiveValue    <- TRUE
options$estimatesPlotNegativePredictiveValue    <- TRUE
options$estimatesPlotFalseDiscoveryRate         <- TRUE
options$estimatesPlotFalseOmissionRate          <- TRUE
options$estimatesPlotFalsePositiveRate          <- TRUE
options$estimatesPlotFalseNegativeRate          <- TRUE
options$estimatesPlotAccuracy                   <- TRUE

options$statistics                    <- TRUE
options$confusionMatrix               <- TRUE
options$confusionMatrixAdditionalInfo <- TRUE
options$confusionMatrixType           <- "both"

options$colorTruePositive <- "darkgreen"
options$colorFalseNegative <- "red"
options$colorFalsePositive <- "darkorange"
options$colorTrueNegative <- "steelblue"

options$samples <- 1e4
options$burnin <- 500
options$thinning <- 1
options$chains <- 4

options$varyingThresholdSamples <- 1e3
options$varyingThresholdBurnin <- 500
options$varyingThresholdThinning <- 1
options$varyingThresholdChains <- 2

options$setSeed <- TRUE
options$seed <- 1
options(BINARY_CLASSIFICATION_SAMPLES = "bc-data-samples.Rds")
options(BINARY_CLASSIFICATION_SUMMARY_BY_THRESHOLD = "bc-data-summary-by-threshold.Rds")
set.seed(1)
# temporarily replace functions that rely on JAGS to avoid seed issues
testthat::with_mocked_bindings(
  results <- jaspTools::runAnalysis(name    = "LSbinaryclassification",
                                    dataset = "binaryClassification.csv",
                                    options = options),
  .bcSamplesByThreshold = .bcLoadSamplesByThreshold,
  .bcRunJags = .bcLoadRunJags,
  # in principle .package should be NULL, but since jaspTools::runAnalysis installs the module we gotta do it this way
  .package = "jaspLearnBayes"
)

test_that("Alluvial plot matches", {
  testthat::skip_on_os(c("windows", "linux")) # see https://github.com/jasp-stats/jaspLearnBayes/pull/120
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_alluvialPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "alluvial-plot-data", dir="LSbinaryclassification")
})

test_that("Estimates plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_estimatesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "estimates-data", dir="LSbinaryclassification")
})

test_that("Probability positive plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_probabilityPositivePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-positive-data", dir="LSbinaryclassification")
})


test_that("Receiving Operating Characteristic Curve plot matches", {
  testthat::skip_on_os(c("linux")) # fails on linux, no idea why
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_rocPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "receiving-operating-characteristic-curve-data", dir="LSbinaryclassification")
})



test_that("Signal detection plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_signalDetectionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "signal-detection-data", dir="LSbinaryclassification")
})

test_that("Test characteristics plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_testCharacteristicsPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-characteristics-data", dir="LSbinaryclassification")
})

test_that("PPV and NPV by prevalence plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_predictiveValuesByPrevalence"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "ppv-and-npv-by-prevalence-data", dir="LSbinaryclassification")
})

test_that("Confusion Matrix table results match", {
  table <- results[["results"]][["tables"]][["collection"]][["tables_confusionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Sensitivity", 0.854257834228264, 0.951431844073659, 0.908459191944529,
                                      "False negative rate", 0.0485681559263413, 0.145742165771736,
                                      0.0915408080554712, "Positive condition", "False negative",
                                      0.00570937390369739, 0.0180131333638601, 0.0110633305241458,
                                      "True positive", 0.0912384152445435, 0.12969582042926, 0.10980378553446,
                                      "Prevalence", 0.101351870668318, 0.14172979593409, 0.120867116058606,
                                      "False positive rate", 0.444192576487119, 0.510052017259065,
                                      0.476987019601344, "Specificity", 0.489947982740935, 0.555807423512881,
                                      0.523012980398656, "Negative condition", "True negative", 0.429157936214345,
                                      0.490642993581375, 0.459798001387487, "False positive", 0.389099945998821,
                                      0.450411060204435, 0.419334882553907, "Rareness", 0.898648129331682,
                                      0.85827020406591, 0.879132883941394, "Accuracy", 0.538540981007685,
                                      0.599832112751126, 0.569601786921947, "", "", "", "", "True/Total",
                                      "Negative predictive value", 0.961840387553215, 0.987803780124914,
                                      0.976501292947063, "Positive predictive value", 0.174071092651079,
                                      0.242715760455815, 0.20751578223418, "", "", "", ""))
})

test_that("Statistics table results match", {
  table <- results[["results"]][["tables"]][["collection"]][["tables_statistics"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.120867116058606, "Proportion of a population affected by the condition.",
                                      0.101351870668318, 0.12064354503662, "P(Condition = positive)",
                                      1.00006814554785, 0.0102788067584067, 24654.5326782062, "Prevalence",
                                      0.14172979593409, 0.908459191944529, "(True positive rate) Proportion of those who are affected by the condition and are correctly tested positive.",
                                      0.854257834228264, 0.910519844671039, "P(Test = positive | Condition = positive)",
                                      1.00025404390256, 0.0250219064832006, 24127.3198641937, "Sensitivity",
                                      0.951431844073659, 0.523012980398656, "(True negative rate) Proportion of those who are not affected by the condition and are correctly tested negative.",
                                      0.489947982740935, 0.523110575423086, "P(Test = negative | Condition = negative)",
                                      1.00000335321342, 0.0168158401738967, 24356.322155301, "Specificity",
                                      0.555807423512881, 0.10980378553446, "Proportion of a population affected by a condition and correctly tested positive.",
                                      0.0912384152445435, 0.109568160574591, "P(Condition = positive <unicode> Test = positive)",
                                      1.00012276458556, 0.00983072556106431, 24411.9642126466, "True positive",
                                      0.12969582042926, 0.419334882553907, "Proportion of a population not affected by a condition and incorrectly tested positive.",
                                      0.389099945998821, 0.419258944643181, "P(Condition = negative <unicode> Test = positive)",
                                      0.999979541884593, 0.0155749732821612, 24330.7885398618, "False positive",
                                      0.450411060204435, 0.459798001387487, "Proportion of a population not affected by a condition and correctly tested negative.",
                                      0.429157936214345, 0.459817864792174, "P(Condition = negative <unicode> Test = negative)",
                                      1.00000906278576, 0.0157348533082046, 24449.073561607, "True negative",
                                      0.490642993581375, 0.0110633305241458, "Proportion of a population affected by a condition and incorrectly tested negative.",
                                      0.00570937390369739, 0.0107752268888461, "P(Condition = positive <unicode> Test = negative)",
                                      1.00025933809547, 0.00317112080340567, 24244.0753539589, "False negative",
                                      0.0180131333638601, 0.20751578223418, "Proportion of those who tested positive and are affected by the condition.",
                                      0.174071092651079, 0.20723787687338, "P(Condition = positive | Test = positive)",
                                      1.0000946073055, 0.0175288086834811, 24078.8923454883, "Positive predictive value",
                                      0.242715760455815, 0.976501292947063, "Proportion of those who tested negative and are not affected by the condition.",
                                      0.961840387553215, 0.977095872169707, "P(Condition = negative | Test = negative)",
                                      1.00023968881048, 0.00669739053595994, 24294.7346937001, "Negative predictive value",
                                      0.987803780124914, 0.79248421776582, "Proportion of false positives in the pool of those that test positive.",
                                      0.757284239544185, 0.79276212312662, "P(Condition = negative | Test = positive)",
                                      1.0000946073055, 0.0175288086834811, 24078.8923454883, "False discovery rate",
                                      0.825928907348921, 0.0234987070529374, "Proportion of false negatives in the pool of those that test negative.",
                                      0.0121962198750858, 0.0229041278302931, "P(Condition = positive | Test = negative)",
                                      1.00023947053951, 0.00669739053595994, 24294.7346937001, "False omission rate",
                                      0.0381596124467849, 0.476987019601344, "Complement proportion to specificity.",
                                      0.444192576487119, 0.476889424576914, "P(Test = positive | Condition = negative)",
                                      1.00000335321342, 0.0168158401738967, 24356.322155301, "False positive rate",
                                      0.510052017259065, 0.0915408080554712, "Complement proportion to sensitivity.",
                                      0.0485681559263413, 0.0894801553289608, "P(Test = negative | Condition = positive)",
                                      1.00025402560669, 0.0250219064832006, 24127.3198641937, "False negative rate",
                                      0.145742165771736, 0.569601786921947, "Proportion of the population that is true positive or true negative.",
                                      0.538540981007685, 0.569727612235762, "P(Condition = positive <unicode> Test = positive <unicode> Condition = negative <unicode> Test = negative)",
                                      1.00002519732869, 0.0155968864621797, 24170.1867967932, "Accuracy",
                                      0.599832112751126))
})

# Errors ----
test_that("Analysis handles errors", {
  set.seed(1)
  data <- data.frame(bin = factor(sample(1:2, 100, TRUE)), three = factor(sample(1:3, 100, TRUE)))
  data$marker <- rnorm(100, as.numeric(data$bin))
  data$badMarker <- 1-data$marker
  data$miss99 <- NA
  data$miss99[1] <- data$marker[1]

  # More than two levels
  options <- jaspTools::analysisOptions("LSbinaryclassification")
  options$inputType <- "data"
  options$labels <- "three"
  options$marker <- "marker"

  options$colorTruePositive <- "darkgreen"
  options$colorFalseNegative <- "red"
  options$colorFalsePositive <- "darkorange"
  options$colorTrueNegative <- "steelblue"

  options$samples <- 1e4
  options$burnin <- 500
  options$thinning <- 1
  options$chains <- 4

  options$varyingThresholdSamples <- 1e3
  options$varyingThresholdBurnin <- 500
  options$varyingThresholdThinning <- 1
  options$varyingThresholdChains <- 2

  results <- jaspTools::runAnalysis("LSbinaryclassification", data, options)

  testthat::expect_true(results[["results"]][["error"]])
  testthat::expect_identical(results[["results"]][["errorMessage"]],
                             "The 'Positive condition (binary)' variable must have two levels!")

  # Less than one observation per level
  options <- jaspTools::analysisOptions("LSbinaryclassification")
  options$inputType <- "data"
  options$labels <- "bin"
  options$marker <- "miss99"

  options$colorTruePositive <- "darkgreen"
  options$colorFalseNegative <- "red"
  options$colorFalsePositive <- "darkorange"
  options$colorTrueNegative <- "steelblue"


  results <- jaspTools::runAnalysis("LSbinaryclassification", data, options)

  testthat::expect_true(results[["results"]][["error"]])
  testthat::expect_identical(results[["results"]][["errorMessage"]],
                             "Each condition needs at least one observation.")

  # Bad group means
  options <- jaspTools::analysisOptions("LSbinaryclassification")
  options$inputType <- "data"
  options$labels <- "bin"
  options$marker <- "badMarker"

  options$colorTruePositive <- "darkgreen"
  options$colorFalseNegative <- "red"
  options$colorFalsePositive <- "darkorange"
  options$colorTrueNegative <- "steelblue"


  results <- jaspTools::runAnalysis("LSbinaryclassification", data, options)

  testthat::expect_true(results[["results"]][["error"]])
  testthat::expect_identical(results[["results"]][["errorMessage"]],
                             "Mean of marker in positive condition (2) needs to be larger than the mean of marker in negative condition (1).")

  # Bad colors
  options <- jaspTools::analysisOptions("LSbinaryclassification")
  options$inputType <- "data"
  options$labels <- "bin"
  options$marker <- "badMarker"

  options$colorTruePositive <- "darkgree"
  options$colorFalseNegative <- "ref"
  options$colorFalsePositive <- "darkorange"
  options$colorTrueNegative <- "steelblue"

  results <- jaspTools::runAnalysis("LSbinaryclassification", data, options)

  testthat::expect_true(results[["results"]][["error"]])
  testthat::expect_identical(results[["results"]][["errorMessage"]],
                             "Some of the specified colors are not valid colors.")

})
