#  OTHER MULTI-LABEL EVALUATION METRICS ========================================
#' @name Basic metrics
#' @rdname evmetrics-ml
#' @family evaluation metrics
#' @title Multi-label evaluation metrics
#' @description Several evaluation metrics designed for multi-label problems.
#' @param true_labels Matrix of true labels, columns corresponding to labels and
#'  rows to instances.
#' @param predicted_labels Matrix of predicted labels, columns corresponding to
#'  labels and rows to instances.
#' @return Resulting value in the range [0, 1]
#' @details
#' \strong{Available metrics in this category}
#' \itemize{
#'  \item \code{hamming_loss}: describes
#'  the average absolute distance between a predicted label and its true value.
#'  \item \code{subset_accuracy}: the ratio of correctly predicted labelsets.
#' }
#' @examples
#' true_labels <- matrix(c(
#' 1,1,1,
#' 0,0,0,
#' 1,0,0,
#' 1,1,1,
#' 0,0,0,
#' 1,0,0
#' ), ncol = 3, byrow = TRUE)
#' predicted_labels <- matrix(c(
#' 1,1,1,
#' 0,0,0,
#' 1,0,0,
#' 1,1,0,
#' 1,0,0,
#' 0,1,0
#' ), ncol = 3, byrow = TRUE)
#'
#' hamming_loss(true_labels, predicted_labels)
#' subset_accuracy(true_labels, predicted_labels)
#' @seealso \code{\link{mldr_evaluate}}
NULL

#' @rdname evmetrics-ml
#' @export
hamming_loss <- function(true_labels, predicted_labels)
  mean(true_labels != predicted_labels)

#' @rdname evmetrics-ml
#' @export
subset_accuracy <- function(true_labels, predicted_labels)
  mean(apply(true_labels == predicted_labels, 1, all))
