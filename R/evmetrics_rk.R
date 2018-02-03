#  FILE: RANKING EVALUATION METRICS ==========================================
#' @name Ranking-based metrics
#' @rdname evmetrics-rk
#' @title Multi-label ranking-based evaluation metrics
#' @description ...
#' @param true_labels Matrix of true labels, columns corresponding to labels and
#'  rows to instances.
#' @param predictions Matrix of probabilities predicted by a classifier.
#' @param ... Additional parameters to be passed to the ranking function.
#' @return Performance metric value
#' @details The \code{ties_method} parameter for the ranking function is passed
#'  to R's own \code{rank}. It accepts the following values:
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
#'
#'  The default behavior in mldr corresponds to value \code{"last"}, since this
#'  is the behavior of the ranking method in MULAN, in order to facilitate fair
#'  comparisons among classifiers over both platforms.
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
macro_auc <- function(true_labels, predictions) {
  if (!requireNamespace("pROC", quietly = TRUE))
    return(NULL)

  mean(sapply(1:ncol(trueLabels), function(l) {
    if (sum(trueLabels[, l]) == 0)
      0.5
    else
      pROC::auc(true_labels[, l], predictions[, l])
  }))
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
example_auc <- function(true_labels, predictions) {
  if (!requireNamespace("pROC", quietly = TRUE))
    return(NULL)

  computable <- rowSums(true_labels) != 0 & rowMeans(true_labels) != 1

  mean(sum(!computable) * 0.5 +
         sum(sapply(which(computable), function(r)
           pROC::auc(true_labels[r, ], predictions[r, ]))))
}
