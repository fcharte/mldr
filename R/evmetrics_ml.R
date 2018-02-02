#  OTHER MULTI-LABEL EVALUATION METRICS ========================================
#' @name Evaluation metrics
#' @rdname evmetrics-ml
#' @title Multi-label evaluation metrics
#' @description Several evaluation metrics designed for multi-label problems.
#' @param true_labels Matrix of true labels, columns corresponding to labels and
#'  rows to instances.
#' @param predicted_labels Matrix of predicted labels, columns corresponding to
#'  labels and rows to instances.
#' @return Resulting value in the range [0, 1]
#' @details As defined in the multi-label literature, Hamming Loss describes
#'  the average absolute distance between a predicted label and its true value.
#'  Subset Accuracy is the ratio of correctly predicted labelsets.
NULL

#' @rdname evmetrics-ml
#' @export
hamming_loss <- function(true_labels, predicted_labels)
  mean(true_labels != predicted_labels)

#' @rdname evmetrics-ml
#' @export
subset_accuracy <- function(true_labels, predicted_labels)
  mean(apply(true_labels == predicted_labels, 1, all))
