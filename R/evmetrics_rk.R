#  FILE: RANKING EVALUATION METRICS ==========================================
#' @name Ranking-based metrics
#' @rdname evmetrics-rk
#' @title Multi-label ranking-based evaluation metrics
#' @description ...
#' @param true_labels Matrix of true labels, columns corresponding to labels and
#'  rows to instances.
#' @param predictions Matrix of probabilities predicted by a classifier.
#' @param ... Additional parameters to be passed to the \code{rank} function.
#' @return Performance metric value
#' @details The \code{ties.method} parameter for the \code{rank} function may
#'  vary results in some edge cases.
NULL

rank_labels <- function(predicted_labels, ...) {
  t(apply(
    X = predicted_labels,
    MARGIN = 1,
    FUN = function(row)
      rank(-row, ...)
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


# Calculate example based Coverage
mldr_Coverage <- function(trueLabels, predictions) {
  sum(unlist(lapply(1:nrow(predictions), function(idr) {
    idxs <- which(trueLabels[idr,] == TRUE)
    rk <- order(predictions[idr,], decreasing = TRUE)
    max(rk[idxs]) - 1
  }))) / nrow(trueLabels)
}

# Calculate example based One Error
mldr_OneError <- function(trueLabels, predictions) {
  maxIndex <- apply(predictions, 1, function(r)
    order(r)[length(r)])
  sum(trueLabels[cbind(1:nrow(trueLabels), maxIndex)] != 1) / nrow(trueLabels)
}
# Calculate example based Ranking Loss
mldr_RankingLoss <- function(trueLabels, predictions) {
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
