#' @title Evaluates the predictions made by a multilabel classifier
#' @description Taking as input an \code{mldr} object and a matrix with the predictions
#' given by a classifier, this function evaluates the classifier performance through
#' several multilabel measures
#' @param mldr Object of \code{mldr} type containing the instances to evaluate
#' @param predictions Matrix with the labels predicted for each instance in the \code{mldr} parameter. Each element
#' should be a value into [0,1] range
#' @param threshold Threshold to use to generate bipartition of labels. By default the value 0.5 is used
#' @return A list with multilabel predictive performance measures: HammingLoss, Accuracy, Precision, Recall, FMeasure
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

  counters <- data.frame(
    RealPositives      = rowSums(trueLabels),
    RealNegatives      = rowSums(!trueLabels),
    PredictedPositives = rowSums(bipartition),
    PredictedNegatives = rowSums(!bipartition),
    TruePositives      = rowSums(trueLabels & bipartition),
    TrueNegatives      = rowSums(!trueLabels & !bipartition)
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
  mean((counters$TruePositives + counters$TrueNegatives) / (counters$PredictedPositives + counters$PredictedNegatives))
}

# Calculate global precision
mldr_Precision <- function(counters) {
  mean(counters$TruePositives / counters$PredictedPositives, na.rm = TRUE)
}

# Calculate global recall
mldr_Recall <- function(counters) {
  mean(counters$TruePositives / counters$RealPositives, na.rm  = TRUE)
}

# Calculate F-Measure
mldr_FMeasure <- function(counters) {
  precision <- counters$TruePositives / counters$PredictedPositives
  recall <- counters$TruePositives / counters$RealPositives

  mean(precision * recall * 2 / (precision + recall), na.rm = TRUE)
}




testMeasures <- function() {
  m <- mldr_from_dataframe(data.frame(F = c(1,1,1,1,1,1), L1 = c(1,0,1,1,1,0), L2 = c(1,1,0,1,1,1)), labelIndices = c(2, 3))
  p <- matrix(c(1,0,1,1,1,0,1,0,1,0,1,0), ncol = 2, byrow = T)
  mldr_evaluate(m[1:3], p[1:3,])
}
