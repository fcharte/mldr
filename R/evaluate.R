#' @title Creates an object representing a multilabel dataset
#' @description Reads a multilabel dataset from a file and returns an \code{mldr} object
#' containing the data and additional measures. The file has to be in ARFF format.
#' The label information could be in a separate XML file (MULAN style) or in the
#' the arff header (MEKA style)
#' @param mldr Object of \code{mldr} type containing the instances to evaluate
#' @param predictions Matrix with the labels predicted for each instance in the \code{mldr} parameter. Each element
#' should be a value into [0,1] range
#' @param threshold Threshold to use to generate bipartition of labels
#' @return An list with multilabel predictive performance measures
#' @seealso \code{\link{mldr}}
#' @examples
#'
#' library(mldr)
#'\dontrun{
#' # Evaluate predictive performance
#' evaluation.measures <- mldr_evaluate(emotions.test, predictions.emotions)
#'}
#' @export
mldr_evaluate <- function(mldr, predictions, threshold = 0.5) {
  trueLabels <- mldr$dataset[, mldr$labels$index]
  if(any((dim(trueLabels) == dim(predictions)) == FALSE))
    stop("Wrong predicitions matrix!")

  bipartition <- predictions
  active <- bipartition >= threshold
  bipartition[active] <- 1
  bipartition[!active] <- 0

  counters <- list(
    RealPositives      = sum(trueLabels),
    RealNegatives      = sum(!trueLabels),
    PredictedPositives = sum(bipartition),
    PredictedNegatives = sum(!bipartition),
    TruePositives      = sum(trueLabels & bipartition),
    TrueNegatives      = sum(!trueLabels & !bipartition)
  )

  list(
    HammingLoss  = mldr_HL(trueLabels, bipartition),
    Accuracy     = mldr_Accuracy(counters),
    Precision    = mldr_Precision(counters),
    Recall       = mldr_Recall(counters),
    FMeasure     = mldr_FMeasure(counters)
  )
}

# Calculate global Hamming Loss
mldr_HL <- function(trueLabels, predictions) {
  sum(trueLabels != predictions) / (nrow(trueLabels) * ncol(trueLabels))
}

# Calculate global accuracy
mldr_Accuracy <- function(counters) {
  (counters[["TruePositives"]] + counters[["TrueNegatives"]]) / (counters[["PredictedPositives"]] + counters[["PredictedNegatives"]])
}

# Calculate global precision
mldr_Precision <- function(counters) {
  counters[["TruePositives"]] / counters[["PredictedPositives"]]
}

# Calculate global recall
mldr_Recall <- function(counters) {
  counters[["TruePositives"]] / counters[["RealPositives"]]
}

# Calculate F-Measure
mldr_FMeasure <- function(counters) {
  mldr_Precision(counters) * mldr_Recall(counters) * 2 / (mldr_Precision(counters) + mldr_Recall(counters))
}
