count_by <- function(matrix, cols = TRUE)
  (if (cols) colSums else rowSums)(matrix)

true_positive <- function(y_true, y_pred, ...)
  count_by(y_true * y_pred, ...)

false_positive <- function(y_true, y_pred, ...)
  count_by(y_pred * (1 - y_true), ...)

true_negative <- function(y_true, y_pred, ...)
  true_positive(1 - y_true, 1 - y_pred, ...)

false_negative <- function(y_true, y_pred, ...)
  false_positive(1 - y_true, 1 - y_pred, ...)

base_accuracy <- function(tp, fp, tn, fn)
  (tp + tn) / (tp + fp + tn + fn)

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

micro <- function(metric) function(y_true, y_pred, ...) {
  tp <- sum(true_positive(y_true, y_pred))
  fp <- sum(false_positive(y_true, y_pred))
  tn <- sum(true_negative(y_true, y_pred))
  fn <- sum(false_negative(y_true, y_pred))

  metric(tp, fp, tn, fn, ...)
}

averagedMetric <- function(label) function(metric) function(y_true, y_pred, undefinedValue = "diagnose") {
  matrix <- rbind(
    true_positive(y_true, y_pred, cols = label),
    false_positive(y_true, y_pred, cols = label),
    true_negative(y_true, y_pred, cols = label),
    false_negative(y_true, y_pred, cols = label)
  )

  unpacked <- function(col, ...) do.call(metric, c(as.list(col), list(...)))
  applied <- apply(matrix, 2, unpacked, undefinedValue = undefinedValue)

  na.rm <- FALSE
  if (!is.na(undefinedValue) && undefinedValue == "ignore") {
    warning("Undefined values will be ignored, mean will be computed with the rest.")
    na.rm <- TRUE
  }

  mean(applied, na.rm = na.rm)
}

macro = averagedMetric(label = TRUE)
instanceAvg = averagedMetric(label = FALSE)

# fmeasure <- function(precision, recall)
#   2 * precision * recall / (precision + recall)

base_fmeasure <- function(precision_f, recall_f) function(y_pred, y_true, ...) {
  p <- precision_f(y_pred, y_true, ...)
  r <- recall_f(y_pred, y_true, ...)
  2 * p * r / (p + r)
}

hammingLoss <- function(y_true, y_pred)
  mean(abs(y_true - y_pred))

subsetAccuracy <- function(y_true, y_pred)
  mean(apply(y_true == y_pred, 1, all))

#' @export
accuracy = micro(base_accuracy)

#' @export
recall <- instanceAvg(base_recall)

#' @export
precision <- instanceAvg(base_precision)

#' @export
Fmeasure <- base_fmeasure(precision, recall)

#' @export
microRecall = micro(base_recall)

#' @export
microPrecision = micro(base_precision)

#' @export
macroRecall = macro(base_recall)

#' @export
macroPrecision = macro(base_precision)

#' @export
microFmeasure = base_fmeasure(microPrecision, microRecall)

#' @export
macroFmeasure = base_fmeasure(macroPrecision, macroRecall)

### TEST
#
# y_true <- matrix(c(
#   1,1,1,
#   0,0,0,
#   1,0,0,
#   1,1,1,
#   0,0,0,
#   1,0,0
# ), ncol = 3, byrow = T)
# y_pred <- matrix(c(
#   1,1,1,
#   0,0,0,
#   1,0,0,
#   1,1,0,
#   1,0,0,
#   0,1,0
# ), ncol = 3, byrow = T)

# mldr::microRecall(y_true, y_pred)
# mldr::macroRecall(y_true, y_pred)
# mldr::microPrecision(y_true, y_pred)
# mldr::macroPrecision(y_true, y_pred)
# mldr::microFmeasure(y_true, y_pred)
# mldr::macroFmeasure(y_true, y_pred)
# mldr::hamming_loss(y_true, y_pred)
# mldr::subset_accuracy(y_true, y_pred)
#
# ### TEST mldr
# true_mldr <- list(
#   dataset = as.data.frame(y_true),
#   labels = list(index = 1:3)
# )
# class(true_mldr) <- "mldr" # teehee
# mldr::mldr_evaluate(true_mldr, as.data.frame(y_pred))
