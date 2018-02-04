#  FILE: RANKING EVALUATION METRICS ==========================================
#' @name Ranking-based metrics
#' @rdname evmetrics-rk
#' @family evaluation metrics
#' @title Multi-label ranking-based evaluation metrics
#' @description Functions that compute ranking-based metrics, given a matrix
#'  of true labels and a matrix of predicted probabilities.
#' @param true_labels Matrix of true labels, columns corresponding to labels and
#'  rows to instances.
#' @param predictions Matrix of probabilities predicted by a classifier.
#' @param ... Additional parameters to be passed to the ranking function.
#' @param undefined_value A default value for the cases when macro-averaged
#'  and example-averaged AUC encounter undefined (not computable) values, e.g.
#'  \code{0}, \code{0.5}, or \code{NA}.
#' @param na.rm Logical specifying whether to ignore undefined values when
#'  \code{undefined_value} is set to \code{NA}.
#' @return Atomical numeric vector specifying the resulting performance metric
#'  value.
#' @details
#' \strong{Available metrics in this category}
#'
#' \itemize{
#'  \item \code{average_precision}: Example and ranking based average precision (how many steps have to be made in the ranking to reach a certain relevant label, averaged by instance)
#'  \item \code{coverage}: Example and ranking based coverage (how many steps have to be made in the ranking to cover all the relevant labels, averaged by instance)
#'  \item \code{example_auc}: Example based Area Under the Curve ROC (averaged by instance)
#'  \item \code{macro_auc}: Label and ranking based Area Under the Curve ROC (macro-averaged by label)
#'  \item \code{micro_auc}: Label and ranking based Area Under the Curve ROC (micro-averaged)
#'  \item \code{one_error}: Example and ranking based one-error (how many times the top-ranked label is not a relevant label, averaged by instance)
#'  \item \code{ranking_loss}: Example and ranking based ranking-loss (how many times a non-relevant label is ranked above a relevant one, evaluated for all label pairs and averaged by instance)
#' }
#'
#' \strong{Breaking ties in rankings}
#'
#' The additional \code{ties_method} parameter for the ranking
#'  function is passed to R's own \code{rank}. It accepts the following values:
#'  \itemize{
#'  \item \code{"average"}
#'  \item \code{"first"}
#'  \item \code{"last"}
#'  \item \code{"random"}
#'  \item \code{"max"}
#'  \item \code{"min"}
#'  }
#'  See \code{\link[base]{rank}} for information on the effect of each
#'  parameter.
#'  The default behavior in mldr corresponds to value \code{"last"}, since this
#'  is the behavior of the ranking method in MULAN, in order to facilitate fair
#'  comparisons among classifiers over both platforms.
#' @examples
#' true_labels <- matrix(c(
#' 1,1,1,
#' 0,0,0,
#' 1,0,0,
#' 1,1,1,
#' 0,0,0,
#' 1,0,0
#' ), ncol = 3, byrow = TRUE)
#' predicted_probs <- matrix(c(
#' .6,.5,.9,
#' .0,.1,.2,
#' .8,.3,.2,
#' .7,.9,.1,
#' .7,.3,.2,
#' .1,.8,.3
#' ), ncol = 3, byrow = TRUE)
#'
#' # by default, labels with same ranking are assigned ascending rankings
#' # in the order they are encountered
#' coverage(true_labels, predicted_probs)
#' # in the following, labels with same ranking will receive the same,
#' # averaged ranking
#' average_precision(true_labels, predicted_probs, ties_method = "average")
#'
#' # the following will treat all undefined values as 0 (counting them
#' # for the average)
#' example_auc(true_labels, predicted_probs, undefined_value = 0)
#' # the following will ignore undefined values (not counting them for
#' # the average)
#' example_auc(true_labels, predicted_probs, undefined_value = NA, na.rm = TRUE)
#' @seealso \code{\link{mldr_evaluate}}
NULL

rank_labels <- function(predicted_labels, ties_method = "last") {
  t(apply(
    X = predicted_labels,
    MARGIN = 1,
    FUN = function(row)
      rank(-row, ties.method = ties_method)
  ))
}

relevant_labels <- function(true_labels) {
  lapply(split(true_labels == 1, 1:nrow(true_labels)), which)
}

#  Calculate example based Average Precision
#' @rdname evmetrics-rk
#' @export
average_precision <- function(true_labels, predictions, ...) {
  rankings <- rank_labels(predictions, ...)
  relevant <- relevant_labels(true_labels)

  mean(sapply(seq_along(relevant), function(row) {
    ratios <- sapply(relevant[[row]], function(label) {
      sum(rankings[row, relevant[[row]]] <= rankings[row, label]) /
        rankings[row, label]
    })

    if (length(relevant[[row]]) == 0) {
      1
    } else {
      sum(ratios) / length(relevant[[row]])
    }
  }))
}

#' @rdname evmetrics-rk
#' @export
one_error <- function(true_labels, predictions) {
  i_max <- apply(predictions, 1, which.max)

  mean(1 - true_labels[cbind(1:length(i_max), i_max)])
}

#' @rdname evmetrics-rk
#' @export
coverage <- function(true_labels, predictions, ...) {
  rankings <- rank_labels(predictions, ...)
  relevant <- relevant_labels(true_labels)

  mean(sapply(1:length(relevant), function(row) {
    if (length(relevant[[row]]) > 0) {
      max(rankings[row, relevant[[row]]]) - 1
    } else {
      0
    }
  }))
}

# Calculate example based Ranking Loss
#' @rdname evmetrics-rk
#' @export
ranking_loss <- function(true_labels, predictions) {
  relevant <- relevant_labels(true_labels)
  nonrelevant <- relevant_labels(1 - true_labels)

  # compute mean across instances
  mean(sapply(seq_along(relevant), function(row) {
    if (length(relevant[[row]]) > 0 && length(nonrelevant[[row]]) > 0) {
      # for each pair of one relevant and one non-relevant label
      mean(outer(relevant[[row]], nonrelevant[[row]], function(i_rel, i_non) {
        # detect whether the non-relevant was better ranked
        predictions[row, i_rel] <= predictions[row, i_non]
      }))
    } else {
      0
    }
  }))
}

# Calculate label based MacroAUC
#' @rdname evmetrics-rk
#' @export
macro_auc <- function(true_labels, predictions, undefined_value = 0.5, na.rm = FALSE) {
  if (!requireNamespace("pROC", quietly = TRUE))
    return(NULL)

  computable <- colSums(true_labels) != 0 & colMeans(true_labels) != 1
  undefined_vec <- rep(undefined_value, times = sum(!computable))

  results <- sapply(which(computable), function(l)
      pROC::auc(true_labels[, l], predictions[, l])
  )

  mean(c(undefined_vec, results), na.rm = na.rm)
}

# Calculate label based MicroAUC
#' @rdname evmetrics-rk
#' @export
micro_auc <- function(true_labels, predictions) {
  if (!requireNamespace("pROC", quietly = TRUE))
    return(NULL)

  as.numeric(pROC::auc(as.integer(true_labels), as.numeric(predictions)))
}

# Calculate example based AUC
#' @rdname evmetrics-rk
#' @export
example_auc <- function(true_labels, predictions, undefined_value = 0.5, na.rm = FALSE) {
  if (!requireNamespace("pROC", quietly = TRUE))
    return(NULL)

  computable <- rowSums(true_labels) != 0 & rowMeans(true_labels) != 1
  undefined_vec <- rep(undefined_value, times = sum(!computable))

  results <- sapply(which(computable), function(r)
    pROC::auc(true_labels[r,], predictions[r,])
  )

  mean(c(undefined_vec, results), na.rm = na.rm)
}
