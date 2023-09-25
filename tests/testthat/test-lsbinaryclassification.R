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
set.seed(1)
results <- jaspTools::runAnalysis(name    = "LSbinaryclassification",
                                  dataset = "binaryClassification.csv",
                                  options = options)

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
                                 list("Sensitivity", 0.853779111361799, 0.951050732434266, 0.908557670487564,
                                      "False negative rate", 0.0489492675657335, 0.146220888638201,
                                      0.0914423295124356, "Positive condition", "False negative",
                                      0.00576179487199414, 0.018100987390675, 0.0110485325784403,
                                      "True positive", 0.0911973251723204, 0.129742901284888, 0.109797860082283,
                                      "Prevalence", 0.10131587246111, 0.141902361984049, 0.120846392660723,
                                      "False positive rate", 0.444217366611229, 0.509693733184746,
                                      0.476908856673286, "Specificity", 0.490306266815254, 0.555782633388771,
                                      0.523091143326714, "Negative condition", "True negative", 0.429169344925411,
                                      0.490738060331644, 0.459879054569247, "False positive", 0.389380261720891,
                                      0.449840579798052, 0.41927455277003, "Rareness", 0.89868412753889,
                                      0.858097638015951, 0.879153607339276, "Accuracy", 0.538931600110022,
                                      0.599860346671375, 0.56967691465153, "", "", "", "", "True/Total",
                                      "Negative predictive value", 0.961646380572624, 0.98777558547579,
                                      0.97653568516653, "Positive predictive value", 0.174111463998809,
                                      0.242726250870084, 0.207523777149946, "", "", "", ""))
})

test_that("Statistics table results match", {
  table <- results[["results"]][["tables"]][["collection"]][["tables_statistics"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.120846392660723, "Proportion of a population affected by the condition.",
                                      0.10131587246111, 0.120596053638879, "P(Condition = positive)",
                                      1.00007734737461, 0.0103155054905895, 25148.4183901585, "Prevalence",
                                      0.141902361984049, 0.908557670487564, "(True positive rate) Proportion of those who are affected by the condition and are correctly tested positive.",
                                      0.853779111361799, 0.910600106900222, "P(Test = positive | Condition = positive)",
                                      1.00005476066721, 0.0250579585490953, 22388.5875672874, "Sensitivity",
                                      0.951050732434266, 0.523091143326714, "(True negative rate) Proportion of those who are not affected by the condition and are correctly tested negative.",
                                      0.490306266815254, 0.523232506673211, "P(Test = negative | Condition = negative)",
                                      1.00006693334085, 0.0167206063501019, 23892.3076149721, "Specificity",
                                      0.555782633388771, 0.109797860082283, "Proportion of a population affected by a condition and correctly tested positive.",
                                      0.0911973251723204, 0.10957217331269, "P(Condition = positive <unicode> Test = positive)",
                                      1.00015410066547, 0.00987577202481968, 24875.7674194863, "True positive",
                                      0.129742901284888, 0.41927455277003, "Proportion of a population not affected by a condition and incorrectly tested positive.",
                                      0.389380261720891, 0.419197990071671, "P(Condition = negative <unicode> Test = positive)",
                                      1.0000491213492, 0.0154613850624348, 24169.0627375078, "False positive",
                                      0.449840579798052, 0.459879054569247, "Proportion of a population not affected by a condition and correctly tested negative.",
                                      0.429169344925411, 0.459974449980892, "P(Condition = negative <unicode> Test = negative)",
                                      1.00007500601746, 0.0157077388383745, 23921.982523023, "True negative",
                                      0.490738060331644, 0.0110485325784403, "Proportion of a population affected by a condition and incorrectly tested negative.",
                                      0.00576179487199414, 0.0107503370273541, "P(Condition = positive <unicode> Test = negative)",
                                      1.00004781126759, 0.00317293895543754, 22668.1489005594, "False negative",
                                      0.018100987390675, 0.207523777149946, "Proportion of those who tested positive and are affected by the condition.",
                                      0.174111463998809, 0.207236770555983, "P(Condition = positive | Test = positive)",
                                      1.00011539745055, 0.0175469992350098, 25006.3377660076, "Positive predictive value",
                                      0.242726250870084, 0.97653568516653, "Proportion of those who tested negative and are not affected by the condition.",
                                      0.961646380572624, 0.977194848912109, "P(Condition = negative | Test = negative)",
                                      1.00003962877827, 0.00670047789392488, 22752.8075104483, "Negative predictive value",
                                      0.98777558547579, 0.792476222850054, "Proportion of false positives in the pool of those that test positive.",
                                      0.757273749129916, 0.792763229444017, "P(Condition = negative | Test = positive)",
                                      1.00011528903354, 0.0175469992350098, 25006.3377660076, "False discovery rate",
                                      0.825888536001191, 0.0234643148334699, "Proportion of false negatives in the pool of those that test negative.",
                                      0.0122244145242099, 0.022805151087891, "P(Condition = positive | Test = negative)",
                                      1.00003951160333, 0.00670047789392488, 22752.8075104483, "False omission rate",
                                      0.038353619427376, 0.476908856673286, "Complement proportion to specificity.",
                                      0.444217366611229, 0.476767493326789, "P(Test = positive | Condition = negative)",
                                      1.00006693334085, 0.0167206063501019, 23892.3076149721, "False positive rate",
                                      0.509693733184746, 0.0914423295124356, "Complement proportion to sensitivity.",
                                      0.0489492675657335, 0.0893998930997783, "P(Test = negative | Condition = positive)",
                                      1.00005476066721, 0.0250579585490953, 22388.5875672874, "False negative rate",
                                      0.146220888638201, 0.56967691465153, "Proportion of the population that is true positive or true negative.",
                                      0.538931600110022, 0.569804219729413, "P(Condition = positive <unicode> Test = positive <unicode> Condition = negative <unicode> Test = negative)",
                                      1.00001653844188, 0.0154959738174959, 23943.6741214115, "Accuracy",
                                      0.599860346671375))
})

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
