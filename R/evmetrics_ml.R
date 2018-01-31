#  OTHER MULTI-LABEL EVALUATION METRICS ========================================
#' @name Evaluation metrics
#' @rdname evmetrics
#' @title Multi-label evaluation metrics
#' @description Several evaluation metrics designed for multi-label problems.
#' @param trueLabels Matrix of true labels, columns corresponding to labels and
#'  rows to instances.
#' @param predictedLabels Matrix of predicted labels, columns corresponding to
#'  labels and rows to instances.
#' @return Resulting value in the range [0, 1]
#' @details As defined in the multi-label literature, Hamming Loss describes
#'  the average absolute distance between a predicted label and its true value.
#'  Subset Accuracy is the ratio of correctly predicted labelsets.
NULL

#' @rdname evmetrics
#' @export
hammingLoss <- function(trueLabels, predictedLabels)
  mean(abs(trueLabels - predictedLabels))

#' @rdname evmetrics
#' @export
subsetAccuracy <- function(trueLabels, predictedLabels)
  mean(apply(trueLabels == predictedLabels, 1, all))
