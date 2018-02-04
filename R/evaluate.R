#' @title Evaluate predictions made by a multilabel classifier
#' @description Taking as input an \code{mldr} object and a matrix with the predictions
#' given by a classifier, this function evaluates the classifier performance through
#' several multilabel metrics.
#' @param mldr Object of \code{"mldr"} class containing the instances to evaluate
#' @param predictions Matrix with the labels predicted for each instance in the \code{mldr} parameter. Each element
#' should be a value into [0,1] range
#' @param threshold Threshold to use to generate bipartition of labels. By default the value 0.5 is used
#' @return A list with multilabel predictive performance measures. The items in the list will be \itemize{
#'  \item \code{accuracy}
#'  \item \code{example_auc}
#'  \item \code{average_precision}
#'  \item \code{coverage}
#'  \item \code{fmeasure}
#'  \item \code{hamming_loss}
#'  \item \code{macro_auc}
#'  \item \code{macro_fmeasure}
#'  \item \code{macro_precision}
#'  \item \code{macro_recall}
#'  \item \code{micro_auc}
#'  \item \code{micro_fmeasure}
#'  \item \code{micro_precision}
#'  \item \code{micro_recall}
#'  \item \code{one_error}
#'  \item \code{precision}
#'  \item \code{ranking_loss}
#'  \item \code{recall}
#'  \item \code{subset_accuracy}
#'  \item \code{roc}
#'  }
#'  The \code{roc} element corresponds to a \code{roc} object associated to the \code{MicroAUC} value. This object can be given as input to \code{plot} for plotting the ROC curve
#'  The \code{example_auc}, \code{macro_auc}, \code{micro_auc} and \code{roc} members will be \code{NULL} if the \code{pROC} package is not installed.
#'
#' @seealso \code{\link{mldr}}, \link{Basic metrics}, \link{Averaged metrics}, \link{Ranking-based metrics}, \code{\link{roc.mldr}}
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

  trueLabels <- mldr_to_labels(mldr)

  if(any((dim(trueLabels) == dim(predictions)) == FALSE))
    stop("Wrong predictions matrix!")

  bipartition <- as.integer(predictions >= threshold)

  list(
    accuracy          = accuracy(trueLabels, predictions),
    example_auc       = example_auc(trueLabels, predictions),
    average_precision = average_precision(trueLabels, predictions),
    coverage          = coverage(trueLabels, predictions),
    fmeasure          = fmeasure(trueLabels, predictions),
    hamming_loss      = hamming_loss(trueLabels, bipartition),
    macro_auc         = macro_auc(trueLabels, predictions),
    macro_fmeasure    = macro_fmeasure(trueLabels, bipartition),
    macro_precision   = macro_precision(trueLabels, bipartition),
    macro_recall      = macro_recall(trueLabels, bipartition),
    micro_auc         = micro_auc(trueLabels, predictions),
    micro_fmeasure    = micro_fmeasure(trueLabels, bipartition),
    micro_precision   = micro_precision(trueLabels, bipartition),
    micro_recall      = micro_recall(trueLabels, bipartition),
    one_error         = one_error(trueLabels, predictions),
    precision         = precision(trueLabels, predictions),
    ranking_loss      = ranking_loss(trueLabels, predictions),
    recall            = recall(trueLabels, predictions),
    subset_accuracy   = subset_accuracy(trueLabels, bipartition),
    roc               = roc(mldr, predictions)
  )
}

#' @title Label matrix of an MLD
#' @description Extracts a matrix with the true 0-1 assignment of labels of an
#'  \code{"mldr"} object.
#' @param mldr \code{"mldr"} object.
#' @return Numeric matrix of labels.
#' @seealso \link{Basic metrics}, \link{Averaged metrics}, \link{Ranking-based metrics}.
#' @export
mldr_to_labels <- function(mldr) {
  as.matrix(mldr$dataset[, mldr$labels$index])
}

#' @rdname roc.mldr
#' @export
roc <- function(...) UseMethod("roc")

#' @title ROC curve
#' @description Calculates the ROC (Receiver Operating Characteristic) curve
#'  for given true labels and predicted ones. The pROC package is needed for
#'  this functionality.
#' @param mldr An \code{"mldr"} object. Its labels will be extracted via
#'  \code{\link{mldr_to_labels}}.
#' @param predictions Matrix of predicted labels or probabilities, columns
#'  corresponding to labels and rows to instances.
#' @param ... Additional parameters to be passed to the \code{pROC::roc} function.
#'  See \code{\link[pROC]{roc}} for more information.
#' @return ROC object from pROC package.
#' @seealso \code{\link{mldr_evaluate}} \code{\link[pROC]{roc}}
#' @export
roc.mldr <- function(mldr, predictions, ...) {
  if (!requireNamespace("pROC", quietly = TRUE))
    return(NULL)

  pROC::roc(as.integer(mldr_to_labels(mldr)),
            as.numeric(predictions),
            algorithm = 3, ...)
}

