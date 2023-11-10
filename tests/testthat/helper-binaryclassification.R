.bcLoadRunJags <- function(...) {
  targetFile <- getOption("BINARY_CLASSIFICATION_SAMPLES")
  if (is.null(targetFile))
    stop("BINARY_CLASSIFICATION_SAMPLES is not set!", domain = NA)

  return(readRDS(testthat::test_path(targetFile)))
}


.bcLoadSamplesByThreshold <- function(...) {
  targetFile <- getOption("BINARY_CLASSIFICATION_SUMMARY_BY_THRESHOLD")
  if (is.null(targetFile))
    stop("BINARY_CLASSIFICATION_SUMMARY_BY_THRESHOLD is not set!", domain = NA)

  return(readRDS(testthat::test_path(targetFile)))
}
