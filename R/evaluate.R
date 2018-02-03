#' @title Evaluates the predictions made by a multilabel classifier
#' @description Taking as input an \code{mldr} object and a matrix with the predictions
#' given by a classifier, this function evaluates the classifier performance through
#' several multilabel metrics.
#' @param mldr Object of \code{mldr} type containing the instances to evaluate
#' @param predictions Matrix with the labels predicted for each instance in the \code{mldr} parameter. Each element
#' should be a value into [0,1] range
#' @param threshold Threshold to use to generate bipartition of labels. By default the value 0.5 is used
#' @return A list with multilabel predictive performance measures. The items in the list will be \itemize{
#'  \item \code{Accuracy}
#'  \item \code{AUC}
#'  \item \code{AveragePrecision}
#'  \item \code{Coverage}
#'  \item \code{FMeasure}
#'  \item \code{HammingLoss}
#'  \item \code{MacroAUC}
#'  \item \code{MacroFMeasure}
#'  \item \code{MacroPrecision}
#'  \item \code{MacroRecall}
#'  \item \code{MicroAUC}
#'  \item \code{MicroFMeasure}
#'  \item \code{MicroPrecision}
#'  \item \code{MicroRecall}
#'  \item \code{OneError}
#'  \item \code{Precision}
#'  \item \code{RankingLoss}
#'  \item \code{Recall}
#'  \item \code{SubsetAccuracy}
#'  \item \code{ROC}
#'  }
#'  The \code{ROC} element corresponds to a \code{roc} object associated to the \code{MicroAUC} value. This object can be given as input to \code{plot} for plotting the ROC curve
#'  The \code{AUC}, \code{MacroAUC}, \code{MicroAUC} and \code{ROC} members will be \code{NULL} if the \code{pROC} package is not installed.
#'
#' @seealso \code{\link{mldr}}, \link{Basic metrics}, \link{Averaged metrics}, \link{Ranking-based metrics}, \code{\link{mldr_roc}}
#' @examples
#'\dontrun{
#' library(mldr)
#'
#' # Get the true labels in emotions
#' predictions <- as.matrix(emotions$dataset[,emotions$labels$index])
#' # and introduce some noise (alternatively get the predictions from some classifier)
#' predictions[sample(1:593, 100), 1:6] <- sample(0:1, 600, replace = TRUE)
#' # then evaluate predictive performance
#' res <- mldr_evaluate(emotions, predictions)
#' str(res)
#' plot(res$ROC, main = "ROC curve for emotions")
#'}
#' @export
mldr_evaluate <- function(mldr, predictions, threshold = 0.5) {
  if(class(mldr) != 'mldr')
    stop('First argument must be an mldr object')

  trueLabels <- as.matrix(mldr$dataset[, mldr$labels$index])
  if(any((dim(trueLabels) == dim(predictions)) == FALSE))
    stop("Wrong predictions matrix!")

  bipartition <- as.integer(predictions >= threshold)

  list(
    Accuracy         = accuracy(trueLabels, predictions),
    AUC              = example_auc(trueLabels, predictions),
    AveragePrecision = average_precision(trueLabels, predictions),
    Coverage         = coverage(trueLabels, predictions),
    FMeasure         = fmeasure(trueLabels, predictions),
    HammingLoss      = hamming_loss(trueLabels, bipartition),
    MacroAUC         = macro_auc(trueLabels, predictions),
    MacroFMeasure    = macro_fmeasure(trueLabels, bipartition),
    MacroPrecision   = macro_precision(trueLabels, bipartition),
    MacroRecall      = macro_recall(trueLabels, bipartition),
    MicroAUC         = micro_auc(trueLabels, predictions),
    MicroFMeasure    = micro_fmeasure(trueLabels, bipartition),
    MicroPrecision   = micro_precision(trueLabels, bipartition),
    MicroRecall      = micro_recall(trueLabels, bipartition),
    OneError         = one_error(trueLabels, predictions),
    Precision        = precision(trueLabels, predictions),
    RankingLoss      = ranking_loss(trueLabels, predictions),
    Recall           = recall(trueLabels, predictions),
    SubsetAccuracy   = subset_accuracy(trueLabels, bipartition),
    ROC              = mldr_roc(trueLabels, predictions)
  )
}

#' @title ROC curve
#' @description Calculates the ROC (Receiver Operating Characteristic) curve
#'  for given true labels and predicted ones. The pROC package is needed for
#'  this functionality.
#' @param true_labels Matrix of true labels, columns corresponding to labels and
#'  rows to instances.
#' @param predicted_labels Matrix of predicted labels, columns corresponding to
#'  labels and rows to instances.
#' @param ... Additional parameters to be passed to the \code{pROC::roc} function.
#' @return ROC object from pROC package
#' @seealso \code{\link{mldr_evaluate}}
#' @export
mldr_roc <- function(true_labels, predicted_labels, ...) {
  if (!requireNamespace("pROC", quietly = TRUE))
    return(NULL)

  pROC::roc(unlist(true_labels),
            as.numeric(predicted_labels),
            algorithm = 3, ...)
}
