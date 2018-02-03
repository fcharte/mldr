#' @title Evaluates the predictions made by a multilabel classifier
#' @description Taking as input an \code{mldr} object and a matrix with the predictions
#' given by a classifier, this function evaluates the classifier performance through
#' several multilabel metrics.
#' @param mldr Object of \code{mldr} type containing the instances to evaluate
#' @param predictions Matrix with the labels predicted for each instance in the \code{mldr} parameter. Each element
#' should be a value into [0,1] range
#' @param threshold Threshold to use to generate bipartition of labels. By default the value 0.5 is used
#' @return A list with multilabel predictive performance measures. The items in the list will be \itemize{
#'  \item \code{Accuracy}: Example and bipartition based accuracy (averaged by instance)
#'  \item \code{AUC}: Example and binary partition Area Under the Curve ROC (averaged by instance)
#'  \item \code{AveragePrecision}: Example and ranking based average precision (how many steps have to be made in the ranking to reach a certain relevant label, averaged by instance)
#'  \item \code{Coverage}: Example and ranking based coverage (how many steps have to be made in the ranking to cover all the relevant labels, averaged by instance)
#'  \item \code{FMeasure}:  Example and binary partition F_1 measure (harmonic mean between precision and recall, averaged by instance)
#'  \item \code{HammingLoss}:  Example and binary partition Hamming Loss (simmetric difference between sets of labels, averaged by instance)
#'  \item \code{MacroAUC}: Label and ranking based Area Under the Curve ROC (macro-averaged by label)
#'  \item \code{MacroFMeasure}: Label and bipartition based F_1 measure (harmonic mean between precision and recall, macro-averaged by label)
#'  \item \code{MacroPrecision}: Label and bipartition based precision (macro-averaged by label)
#'  \item \code{MacroRecall}: Label and bipartition based recall (macro-averaged by label)
#'  \item \code{MicroAUC}: Label and ranking based Area Under the Curve ROC (micro-averaged)
#'  \item \code{MicroFMeasure}: Label and bipartition based F_1 measure (micro-averaged)
#'  \item \code{MicroPrecision}: Label and bipartition based precision (micro-averaged)
#'  \item \code{MicroRecall}: Label and bipartition based recall (micro-averaged)
#'  \item \code{OneError}: Example and ranking based one-error (how many times the top-ranked label is not a relevant label, averaged by instance)
#'  \item \code{Precision}: Example and bipartition based precision (averaged by instance)
#'  \item \code{RankingLoss}: Example and ranking based ranking-loss (how many times a non-relevant label is ranked above a relevant one, evaluated for all label pairs and averaged by instance)
#'  \item \code{Recall}: Example and bipartition based recall (averaged by instance)
#'  \item \code{SubsetAccuracy}: Example and bipartition based subset accuracy (strict equality between predicted and real labelset, averaged by instance)
#'  \item \code{ROC}: A \code{roc} object corresponding to the \code{MicroAUC} value. This object can be given as input to \code{plot} for plotting the ROC curve
#'  }
#'  The \code{AUC}, \code{MacroAUC}, \code{MicroAUC} and \code{ROC} members will be \code{NULL} if the \code{pROC} package is not installed.
#'
#' @seealso \code{\link{mldr}}
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

  bipartition <- predictions
  active <- bipartition >= threshold
  bipartition[active] <- 1
  bipartition[!active] <- 0

  # counters <- data.frame(
  #   RealPositives      = rowSums(trueLabels),
  #   RealNegatives      = rowSums(!trueLabels),
  #   PredictedPositives = rowSums(bipartition),
  #   PredictedNegatives = rowSums(!bipartition),
  #   TruePositives      = rowSums(trueLabels & bipartition),
  #   TrueNegatives      = rowSums(!trueLabels & !bipartition)
  # )

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
#' @return ROC object from pROC package
#' @export
mldr_roc <- function(true_labels, predicted_labels) {
  if (!requireNamespace("pROC", quietly = TRUE))
    return(NULL)

  pROC::roc(unlist(true_labels),
            as.numeric(predicted_labels),
            algorithm = 3)
}
