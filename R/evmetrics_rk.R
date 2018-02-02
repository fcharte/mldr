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
  apply(X = (true_labels == 1), MARGIN = 1, FUN = which)
}

#  Calculate example based Average Precision
#' @rdname evmetrics-rk
#' @export
average_precision <- function(true_labels, predictions, ...) {
  rankings <- rank_labels(predictions, ...)
  relevant <- relevant_labels(true_labels)

  mean(sapply(1:length(relevant), function(row) {
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
ranking_loss <- function(trueLabels, predictions) {
  sum(unlist(lapply(1:nrow(trueLabels), function(idr) {
    idxT <- which(trueLabels[idr,] == 1)
    idxF <- which(trueLabels[idr,] == 0)

    if (length(idxT) > 0 && length(idxF) > 0)
      sum(mapply(function(k, l)
        predictions[idr, k] > predictions[idr, l], idxT, idxF)) / (length(idxT) * length(idxF))
  }))) / nrow(trueLabels)
}


# Calculate label based MacroAUC
mldr_MacroAUC <- function(trueLabels, predictions) {
  if (!requireNamespace("pROC", quietly = TRUE))
    return(NULL)

  mean(unlist(lapply(1:ncol(trueLabels), function(l)
    if (sum(trueLabels[, l]) == 0)
      0.5
    else
      pROC::auc(trueLabels[, l], predictions[, l]))))
}

# Calculate label based MicroAUC
mldr_MicroAUC <- function(trueLabels, predictions) {
  if (!requireNamespace("pROC", quietly = TRUE))
    return(NULL)

  as.numeric(pROC::auc(unlist(trueLabels), as.numeric(predictions)))
}

# Calculate example based AUC
mldr_AUC <- function(trueLabels, predictions) {
  if (!requireNamespace("pROC", quietly = TRUE))
    return(NULL)

  TL <- as.matrix(trueLabels)
  idxs <-
    which(rowSums(trueLabels) != 0 & rowMeans(trueLabels) != 1)
  (sum(!idxs) * 0.5 + sum(unlist(lapply(idxs, function(r)
    pROC::auc(TL[r, ], predictions[r,]))))) / nrow(trueLabels)
}
