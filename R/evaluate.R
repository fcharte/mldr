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

  list(
    HammingLoss  = mldr_HL(trueLabels, bipartition),
    MicroF       = mldr_MicroF(trueLabels, bipartition)
  )
}

# Calculate global Hamming Loss
mldr_HL <- function(trueLabels, predictions) {
  sum(trueLabels != predictions) / (nrow(trueLabels) * ncol(trueLabels))
}

mldr_MicroF <- function(trueLabels, predictions) {
  0
}
