context("Learn Bayes - Binary Classification")

# Point estimates ----
options <- jaspTools::analysisOptions("LSbinaryclassification")
options$introText   <- FALSE
options$inputType   <- "pointEstimates"
options$prevalence  <- "0.1"
options$sensitivity <- "0.8"
options$specificity <- "0.8"

options$plotPriorPosteriorPositive <- TRUE
options$plotIconPlot               <- TRUE
options$plotROC                    <- TRUE
options$plotTestCharacteristics    <- TRUE
options$plotVaryingPrevalence      <- TRUE
options$plotAlluvial               <- TRUE
options$plotSignal                 <- TRUE
options$plotEstimates              <- TRUE
options$plotPrevalence             <- TRUE
options$plotSensitivity            <- TRUE
options$plotSpecificity            <- TRUE
options$plotTruePositive           <- TRUE
options$plotTrueNegative           <- TRUE
options$plotFalsePositive          <- TRUE
options$plotFalseNegative          <- TRUE
options$plotPPV                    <- TRUE
options$plotNPV                    <- TRUE
options$plotFDR                    <- TRUE
options$plotFOR                    <- TRUE
options$plotFPF                    <- TRUE
options$plotFNF                    <- TRUE
options$plotAccuracy               <- TRUE

options$statistics <- TRUE
options$confusionMatrix <- TRUE
options$confusionMatrixAddInfo <- TRUE
options$confusionMatrixType <- "both"

set.seed(1)
results <- jaspTools::runAnalysis(name    = "LSbinaryclassification",
                                  dataset = "binaryClassification.csv",
                                  options = options)

test_that("Alluvial plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotAlluvial"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "alluvial-plot", dir="LSbinaryclassification")
})

test_that("Estimates plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotEstimates"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "estimates", dir="LSbinaryclassification")
})

test_that("Icon plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotIconPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "icon-plot", dir="LSbinaryclassification")
})

test_that("Probability positive plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotPriorPosteriorPositive"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-positive", dir="LSbinaryclassification")
})

test_that("Receiving Operating Characteristic Curve plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotROC"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "receiving-operating-characteristic-curve", dir="LSbinaryclassification")
})

test_that("Signal detection plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotSignal"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "signal-detection", dir="LSbinaryclassification")
})

test_that("Test characteristics plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotTestCharacteristics"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-characteristics", dir="LSbinaryclassification")
})

test_that("PPV and NPV by prevalence plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotVaryingPrevalence"]][["data"]]
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

test_that("Statistics table results match", {
  table <- results[["results"]][["tables"]][["collection"]][["tables_statistics"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.1, "Prevalence", 0.8, "Sensitivity", 0.8, "Specificity", 0.08,
                                      "True positive", 0.18, "False positive", 0.72, "True negative",
                                      0.02, "False negative", 0.307692307692308, "Positive predictive value",
                                      0.972972972972973, "Negative predictive value", 0.692307692307692,
                                      "False discovery rate", 0.0270270270270271, "False omission rate",
                                      0.2, "False positive rate", 0.2, "False negative rate", 0.8,
                                      "Accuracy"))
})


# Using data ----
options <- jaspTools::analysisOptions("LSbinaryclassification")
options$introText <- FALSE
options$inputType <- "data"
options$marker <- "marker"
options$labels <- "condition"
options$threshold <- "0"
options$prevalenceAlpha  <- "1"
options$prevalenceBeta   <- "9"
options$sensitivityAlpha <- "8"
options$sensitivityBeta  <- "2"
options$specificityAlpha <- "8"
options$specificityBeta  <- "2"

options$plotPriorPosteriorPositive <- TRUE
options$plotIconPlot               <- TRUE
options$plotROC                    <- TRUE
options$plotTestCharacteristics    <- TRUE
options$plotVaryingPrevalence      <- TRUE
options$plotAlluvial               <- TRUE
options$plotSignal                 <- TRUE
options$plotEstimates              <- TRUE
options$plotPrevalence             <- TRUE
options$plotSensitivity            <- TRUE
options$plotSpecificity            <- TRUE
options$plotTruePositive           <- TRUE
options$plotTrueNegative           <- TRUE
options$plotFalsePositive          <- TRUE
options$plotFalseNegative          <- TRUE
options$plotPPV                    <- TRUE
options$plotNPV                    <- TRUE
options$plotFDR                    <- TRUE
options$plotFOR                    <- TRUE
options$plotFPF                    <- TRUE
options$plotFNF                    <- TRUE
options$plotAccuracy               <- TRUE

options$statistics <- TRUE
options$confusionMatrix <- TRUE
options$confusionMatrixAddInfo <- TRUE
options$confusionMatrixType <- "both"

set.seed(1)
results <- jaspTools::runAnalysis(name    = "LSbinaryclassification",
                                  dataset = "binaryClassification.csv",
                                  options = options)

test_that("Alluvial plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotAlluvial"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "alluvial-plot-data", dir="LSbinaryclassification")
})

test_that("Estimates plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotEstimates"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "estimates-data", dir="LSbinaryclassification")
})

test_that("Icon plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotIconPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "icon-plot-data", dir="LSbinaryclassification")
})

test_that("Probability positive plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotPriorPosteriorPositive"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-positive-data", dir="LSbinaryclassification")
})

test_that("Receiving Operating Characteristic Curve plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotROC"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "receiving-operating-characteristic-curve-data", dir="LSbinaryclassification")
})

test_that("Signal detection plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotSignal"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "signal-detection-data", dir="LSbinaryclassification")
})

test_that("Test characteristics plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotTestCharacteristics"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "test-characteristics-data", dir="LSbinaryclassification")
})

test_that("PPV and NPV by prevalence plot matches", {
  plotName <- results[["results"]][["plots"]][["collection"]][["plots_plotVaryingPrevalence"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "ppv-and-npv-by-prevalence-data", dir="LSbinaryclassification")
})

test_that("Confusion matrix table results match", {
  table <- results[["results"]][["tables"]][["collection"]][["tables_confusionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Sensitivity", 0.853617585613721, 0.951377771203091, 0.908527820792427,
                                      "False negative rate", 0.0486222287969091, 0.146382414386279,
                                      0.0914721792075729, "Positive condition", "False negative",
                                      0.00573111107389931, 0.0181994038449614, 0.0110449348609118,
                                      "True positive", 0.0910487594642042, 0.129881672882025, 0.109684204862779,
                                      "Prevalence", 0.100993363501433, 0.141735819388412, 0.120729139723691,
                                      "False positive rate", 0.444793819629683, 0.510017048554804,
                                      0.47723170327149, "Specificity", 0.489982951445196, 0.555206180370318,
                                      0.52276829672851, "Negative condition", "True negative", 0.428748967815013,
                                      0.49036390814685, 0.459654696673328, "False positive", 0.389502323249142,
                                      0.450231840410815, 0.419616163602981, "Rareness", 0.899006636498567,
                                      0.858264180611588, 0.879270860276309, "Accuracy", 0.538892152581733,
                                      0.599568884863196, 0.569338901536107, "", "", "", "", "True/Total",
                                      "Negative predictive value", 0.961407041351944, 0.987761345858896,
                                      0.976531650504969, "Positive predictive value", 0.173785155214146,
                                      0.243210163821242, 0.207225799308979, "", "", "", ""))
})

test_that("Statistics table results match", {
  table <- results[["results"]][["tables"]][["collection"]][["tables_statistics"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.120729139723691, 0.100993363501433, "Prevalence", 0.141735819388412,
                                      0.908527820792427, 0.853617585613721, "Sensitivity", 0.951377771203091,
                                      0.52276829672851, 0.489982951445196, "Specificity", 0.555206180370318,
                                      0.109684204862779, 0.0910487594642042, "True positive", 0.129881672882025,
                                      0.419616163602981, 0.389502323249142, "False positive", 0.450231840410815,
                                      0.459654696673328, 0.428748967815013, "True negative", 0.49036390814685,
                                      0.0110449348609118, 0.00573111107389931, "False negative", 0.0181994038449614,
                                      0.207225799308979, 0.173785155214146, "Positive predictive value",
                                      0.243210163821242, 0.976531650504969, 0.961407041351944, "Negative predictive value",
                                      0.987761345858896, 0.792774200691021, 0.756789836178758, "False discovery rate",
                                      0.826214844785854, 0.0234683494950311, 0.0122386541411042, "False omission rate",
                                      0.038592958648056, 0.47723170327149, 0.444793819629683, "False positive rate",
                                      0.510017048554804, 0.0914721792075729, 0.0486222287969091, "False negative rate",
                                      0.146382414386279, 0.569338901536107, 0.538892152581733, "Accuracy",
                                      0.599568884863196))
})
