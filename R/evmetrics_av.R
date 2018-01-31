#  FILE: AVERAGED EVALUATION METRICS ==========================================
#' @name Averaged metrics
#' @rdname metrics
#' @title Multi-label averaged evaluation metrics
#' @description Evaluation metrics based on simple metrics for the confusion
#'  matrix, averaged through several criteria.
#'
#' @param trueLabels Matrix of true labels, columns corresponding to labels and
#'  rows to instances.
#' @param predictedLabels Matrix of predicted labels, columns corresponding to
#'  labels and rows to instances.
#' @param ... Additional parameters for precision, recall and Fmeasure
#' @return Resulting value in the range [0, 1]
#' @details
#'
#' \strong{Deciding a value when denominators are zero}
#'
#' Additional parameter \code{undefinedValue}: The value to be returned when a
#' computation results in an undefined value due to a division by zero. Can be
#' a single value (e.g. NA, 0), a function with the following signature:
#'
#' \code{function(tp, fp, tn, fn)}
#'
#' or a string corresponding to one of the predefined strategies. These are:
#' \itemize{
#'  \item \code{"diagnose"}: This strategy performs the following decision:
#'  \itemize{
#'   \item Returns 1 if there are no true labels and none were predicted
#'   \item Returns 0 otherwise
#'  }
#'  This is the default strategy, and the one followed by MULAN.
#'  \item \code{"ignore"}: Occurrences of undefined values will be ignored when
#'  averaging (averages will be computed with potentially less values than
#'  instances/labels). Undefined values in micro-averaged metrics cannot be
#'  ignored (will return \code{NA}).
#'  \item \code{"na"}: Will return \code{NA} (with class \code{numeric}) and it
#'  will be propagated when averaging (averaged metrics will potentially return
#'  \code{NA}).
#' }
#' @examples
#' trueLabels <- matrix(c(
#' 1,1,1,
#' 0,0,0,
#' 1,0,0,
#' 1,1,1,
#' 0,0,0,
#' 1,0,0
#' ), ncol = 3, byrow = T)
#' predictedLabels <- matrix(c(
#' 1,1,1,
#' 0,0,0,
#' 1,0,0,
#' 1,1,0,
#' 1,0,0,
#' 0,1,0
#' ), ncol = 3, byrow = T)
#'
#' precision(trueLabels, predictedLabels, undefinedValue = "diagnose")
#' macroRecall(trueLabels, predictedLabels, undefinedValue = 0)
#' macroFmeasure(
#'   trueLabels, predictedLabels,
#'   undefinedValue = function(tp, fp, tn, fn) as.numeric(fp == 0 && fn == 0)
#' )
NULL

# CONFUSION MATRIX ============================================================
# counts the ones in a binary matrix, by columns or rows
count_by <- function(matrix, cols = TRUE)
  (if (cols) colSums else rowSums)(matrix)

# counts true positives for a classifier output and ground truth
true_positive <- function(trueLabels, predictedLabels, ...)
  count_by(trueLabels * predictedLabels, ...)

# counts false positives for a classifier output and ground truth
false_positive <- function(trueLabels, predictedLabels, ...)
  count_by(predictedLabels * (1 - trueLabels), ...)

# counts true negatives for a classifier output and ground truth
true_negative <- function(trueLabels, predictedLabels, ...)
  true_positive(1 - trueLabels, 1 - predictedLabels, ...)

# counts false negatives for a classifier output and ground truth
false_negative <- function(trueLabels, predictedLabels, ...)
  false_positive(1 - trueLabels, 1 - predictedLabels, ...)

# TREATMENT OF UNDEFINED VALUES ===============================================
undefinedStrategies <- list()
# Diagnose undefined results: default behavior for MULAN
undefinedStrategies$diagnose = function(tp, fp, tn, fn) {
  # no positive values predicted are considered a good classification
  # if there were no true positive values
  if (tp + fp + fn == 0)
    1
  else
    0
}
# Ignore undefined results or return NA (ignored results are removed
# from means in macro-averaged metrics)
undefinedStrategies$ignore = function(tp, fp, tn, fn) as.numeric(NA)
undefinedStrategies$na = undefinedStrategies$ignore

ifUndefined <- function(value, tp, fp, tn, fn) {
  if (is.function(value)) {
    value(tp, fp, tn, fn)
  } else if (is.function(undefinedStrategies[[value]])) {
    undefinedStrategies[[value]](tp, fp, tn, fn)
  } else {
    value
  }
}

# BASE EVALUATION METRICS =====================================================
base_accuracy <- function(tp, fp, tn, fn)
  (tp + tn) / (tp + fp + tn + fn)

base_precision <- function(tp, fp, tn, fn, undefinedValue = "diagnose") {
  if (tp + fp == 0)
    ifUndefined(undefinedValue, tp, fp, tn, fn)
  else
    tp / (tp + fp)
}

base_recall <- function(tp, fp, tn, fn, undefinedValue = "diagnose") {
  if (tp + fn == 0)
    ifUndefined(undefinedValue, tp, fp, tn, fn)
  else
    tp / (tp + fn)
}

base_fmeasure <- function(precision_f, recall_f) function(trueLabels, predictedLabels, ...) {
  p <- precision_f(trueLabels, predictedLabels, ...)
  r <- recall_f(trueLabels, predictedLabels, ...)
  2 * p * r / (p + r)
}

# AVERAGING FUNCTIONALS =======================================================
# micro takes a base evaluation metric and returns a label-based micro-averaged
# evaluation metric
micro <- function(metric) function(trueLabels, predictedLabels, ...) {
  tp <- sum(true_positive(trueLabels, predictedLabels))
  fp <- sum(false_positive(trueLabels, predictedLabels))
  tn <- sum(true_negative(trueLabels, predictedLabels))
  fn <- sum(false_negative(trueLabels, predictedLabels))

  metric(tp, fp, tn, fn, ...)
}

# averagedMetric generalizes instance-based and macro-averaging
averagedMetric <- function(label) function(metric)
  function(trueLabels, predictedLabels, ...) {
    matrix <- rbind(
      true_positive(trueLabels, predictedLabels, cols = label),
      false_positive(trueLabels, predictedLabels, cols = label),
      true_negative(trueLabels, predictedLabels, cols = label),
      false_negative(trueLabels, predictedLabels, cols = label)
    )

    unpacked <-
      function(col, ...)
        do.call(metric, c(as.list(col), list(...)))
    applied <-
      apply(matrix, 2, unpacked, ...)

    na.rm <- FALSE
    if (!is.na(undefinedValue) && undefinedValue == "ignore") {
      warning("Undefined values will be ignored, mean will be computed with the rest.")
      na.rm <- TRUE
    }

    mean(applied, na.rm = na.rm)
  }

# macro takes a base evaluation metric and returns a label-based macro-averaged
# evaluation metric
macro = averagedMetric(label = TRUE)

# instanceAvg takes a base evaluation metric and returns an instance-averaged
# evaluation metric
instanceAvg = averagedMetric(label = FALSE)

# DEFINITION OF MULTI-LABEL METRICS ===========================================
#' @rdname metrics
#' @export
accuracy <- micro(base_accuracy)

#' @rdname metrics
#' @export
recall <- instanceAvg(base_recall)

#' @rdname metrics
#' @export
precision <- instanceAvg(base_precision)

#' @rdname metrics
#' @export
Fmeasure <- base_fmeasure(precision, recall)

#' @rdname metrics
#' @export
microRecall <- micro(base_recall)

#' @rdname metrics
#' @export
microPrecision <- micro(base_precision)

#' @rdname metrics
#' @export
macroRecall <- macro(base_recall)

#' @rdname metrics
#' @export
macroPrecision <- macro(base_precision)

#' @rdname metrics
#' @export
microFmeasure <- base_fmeasure(microPrecision, microRecall)

#' @rdname metrics
#' @export
macroFmeasure <- base_fmeasure(macroPrecision, macroRecall)
