#  FILE: AVERAGED EVALUATION METRICS ==========================================
#' @name Averaged metrics
#' @rdname evmetrics-av
#' @family evaluation metrics
#' @title Multi-label averaged evaluation metrics
#' @description Evaluation metrics based on simple metrics for the confusion
#'  matrix, averaged through several criteria.
#'
#' @param true_labels Matrix of true labels, columns corresponding to labels and
#'  rows to instances.
#' @param predicted_labels Matrix of predicted labels, columns corresponding to
#'  labels and rows to instances.
#' @param undefined_value The value to be returned when a computation results in
#'  an undefined value due to a division by zero. See details.
#' @param ... Additional parameters for precision, recall and Fmeasure.
#' @return Atomical numeric vector containing the resulting value in the range
#'  [0, 1].
#' @details
#' \strong{Available metrics in this category}
#' \itemize{
#'  \item \code{accuracy}: Bipartition based accuracy
#'  \item \code{fmeasure}:  Example and binary partition F_1 measure (harmonic mean between precision and recall, averaged by instance)
#'  \item \code{macro_fmeasure}: Label and bipartition based F_1 measure (harmonic mean between precision and recall, macro-averaged by label)
#'  \item \code{macro_precision}: Label and bipartition based precision (macro-averaged by label)
#'  \item \code{macro_recall}: Label and bipartition based recall (macro-averaged by label)
#'  \item \code{micro_fmeasure}: Label and bipartition based F_1 measure (micro-averaged)
#'  \item \code{micro_precision}: Label and bipartition based precision (micro-averaged)
#'  \item \code{micro_recall}: Label and bipartition based recall (micro-averaged)
#'  \item \code{precision}: Example and bipartition based precision (averaged by instance)
#'  \item \code{recall}: Example and bipartition based recall (averaged by instance)
#'  }
#'
#' \strong{Deciding a value when denominators are zero}
#'
#' Parameter \code{undefined_value}: The value to be returned when a computation
#' results in an undefined value due to a division by zero. Can be a single
#' value (e.g. NA, 0), a function with the following signature:
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
#' precision(true_labels, predicted_labels, undefined_value = "diagnose")
#' macro_recall(true_labels, predicted_labels, undefined_value = 0)
#' macro_fmeasure(
#'   true_labels, predicted_labels,
#'   undefined_value = function(tp, fp, tn, fn) as.numeric(fp == 0 && fn == 0)
#' )
#' @seealso \code{\link{mldr_evaluate}}, \code{\link{mldr_to_labels}}
NULL

# CONFUSION MATRIX ============================================================
# counts the ones in a binary matrix, by columns or rows
count_by <- function(matrix, cols = TRUE)
  (if (cols) colSums else rowSums)(matrix)

# counts true positives for a classifier output and ground truth
true_positive <- function(true_labels, predicted_labels, ...)
  count_by(true_labels * predicted_labels, ...)

# counts false positives for a classifier output and ground truth
false_positive <- function(true_labels, predicted_labels, ...)
  count_by(predicted_labels * (1 - true_labels), ...)

# counts true negatives for a classifier output and ground truth
true_negative <- function(true_labels, predicted_labels, ...)
  true_positive(1 - true_labels, 1 - predicted_labels, ...)

# counts false negatives for a classifier output and ground truth
false_negative <- function(true_labels, predicted_labels, ...)
  false_positive(1 - true_labels, 1 - predicted_labels, ...)

# TREATMENT OF UNDEFINED VALUES ===============================================
undefined_strats <- list()
# Diagnose undefined results: default behavior for MULAN
undefined_strats$diagnose = function(tp, fp, tn, fn) {
  # no positive values predicted are considered a good classification
  # if there were no true positive values
  if (tp + fp + fn == 0)
    1
  else
    0
}
# Ignore undefined results or return NA (ignored results are removed
# from means in macro-averaged metrics)
undefined_strats$ignore = function(tp, fp, tn, fn) as.numeric(NA)
undefined_strats$na = undefined_strats$ignore

treat_undefined <- function(value, tp, fp, tn, fn) {
  if (is.function(value)) {
    value(tp, fp, tn, fn)
  } else if (value %in% names(undefined_strats) && is.function(undefined_strats[[value]])) {
    undefined_strats[[value]](tp, fp, tn, fn)
  } else {
    value
  }
}

# BASE EVALUATION METRICS =====================================================
base_accuracy <- function(tp, fp, tn, fn, ...) {
  if (tp + fp + fn == 0)
    1
  else
    tp / (tp + fp + fn)
}


base_precision <- function(tp, fp, tn, fn, undefined_value = "diagnose") {
  if (tp + fp == 0)
    treat_undefined(undefined_value, tp, fp, tn, fn)
  else
    tp / (tp + fp)
}

base_recall <- function(tp, fp, tn, fn, undefined_value = "diagnose") {
  if (tp + fn == 0)
    treat_undefined(undefined_value, tp, fp, tn, fn)
  else
    tp / (tp + fn)
}

base_fmeasure = function(tp, fp, tn, fn, ...) {
  if (tp + fp + fn == 0)
    1
  else
    2 * tp / (2 * tp + fn + fp)
}

# AVERAGING FUNCTIONALS =======================================================
# micro takes a base evaluation metric and returns a label-based micro-averaged
# evaluation metric
micro <- function(metric) function(true_labels, predicted_labels, ...) {
  tp <- sum(true_positive(true_labels, predicted_labels))
  fp <- sum(false_positive(true_labels, predicted_labels))
  tn <- sum(true_negative(true_labels, predicted_labels))
  fn <- sum(false_negative(true_labels, predicted_labels))

  metric(tp, fp, tn, fn, ...)
}

# averaged_metric generalizes instance-based and macro-averaging
averaged_metric <- function(label) function(metric)
  function(true_labels, predicted_labels, undefined_value = "diagnose") {
    matrix <- rbind(
      true_positive(true_labels, predicted_labels, cols = label),
      false_positive(true_labels, predicted_labels, cols = label),
      true_negative(true_labels, predicted_labels, cols = label),
      false_negative(true_labels, predicted_labels, cols = label)
    )

    unpacked <-
      function(col, ...)
        do.call(metric, c(as.list(col), list(...)))
    applied <-
      apply(matrix, 2, unpacked, undefined_value = undefined_value)

    na.rm <- FALSE
    if (is.atomic(undefined_value) && undefined_value == "ignore") {
      warning("Undefined values will be ignored, mean will be computed with the rest.")
      na.rm <- TRUE
    }

    mean(applied, na.rm = na.rm)
  }

# macro takes a base evaluation metric and returns a label-based macro-averaged
# evaluation metric
macro = averaged_metric(label = TRUE)

# instance_avg takes a base evaluation metric and returns an instance-averaged
# evaluation metric
instance_avg = averaged_metric(label = FALSE)

# DEFINITION OF MULTI-LABEL METRICS ===========================================
#' @rdname evmetrics-av
#' @export
accuracy <- instance_avg(base_accuracy)

#' @rdname evmetrics-av
#' @export
precision <- instance_avg(base_precision)

#' @rdname evmetrics-av
#' @export
micro_precision <- micro(base_precision)

#' @rdname evmetrics-av
#' @export
macro_precision <- macro(base_precision)

#' @rdname evmetrics-av
#' @export
recall <- instance_avg(base_recall)

#' @rdname evmetrics-av
#' @export
micro_recall <- micro(base_recall)

#' @rdname evmetrics-av
#' @export
macro_recall <- macro(base_recall)

#' @rdname evmetrics-av
#' @export
fmeasure <- instance_avg(base_fmeasure)

#' @rdname evmetrics-av
#' @export
micro_fmeasure <- micro(base_fmeasure)

#' @rdname evmetrics-av
#' @export
macro_fmeasure <- macro(base_fmeasure)
