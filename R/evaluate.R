#' @title Evaluates the predictions made by a multilabel classifier
#' @description Taking as input an \code{mldr} object and a matrix with the predictions
#' given by a classifier, this function evaluates the classifier performance through
#' several multilabel measures
#' @param mldr Object of \code{mldr} type containing the instances to evaluate
#' @param predictions Matrix with the labels predicted for each instance in the \code{mldr} parameter. Each element
#' should be a value into [0,1] range
#' @param threshold Threshold to use to generate bipartition of labels. By default the value 0.5 is used
#' @return A list with multilabel predictive performance measures: HammingLoss, Accuracy, Precision, Recall, FMeasure
#' @seealso \code{\link{mldr}}
#' @examples
#'
#' library(mldr)
#'\dontrun{
#' # Evaluate predictive performance
#' evaluation.measures <- mldr_evaluate(emotions.test, predictions.emotions)
#'}
#' @import pROC
#' @export
mldr_evaluate <- function(mldr, predictions, threshold = 0.5) {
  if(class(mldr) != 'mldr')
    stop('First argument must be an mldr object')

  trueLabels <- mldr$dataset[, mldr$labels$index]
  if(any((dim(trueLabels) == dim(predictions)) == FALSE))
    stop("Wrong predicitions matrix!")

  bipartition <- predictions
  active <- bipartition >= threshold
  bipartition[active] <- 1
  bipartition[!active] <- 0

  counters <- data.frame(
    RealPositives      = rowSums(trueLabels),
    RealNegatives      = rowSums(!trueLabels),
    PredictedPositives = rowSums(bipartition),
    PredictedNegatives = rowSums(!bipartition),
    TruePositives      = rowSums(trueLabels & bipartition),
    TrueNegatives      = rowSums(!trueLabels & !bipartition)
  )

  list(
    Accuracy         = mldr_Accuracy(counters),
    AUC              = mldr_AUC(trueLabels, predictions),
    AveragePrecision = mldr_AveragePrecision(trueLabels, predictions),
    Coverage         = mldr_Coverage(trueLabels, predictions),
    FMeasure         = mldr_FMeasure(counters),
    HammingLoss      = mldr_HL(trueLabels, bipartition),
    MacroAUC         = mldr_MacroAUC(trueLabels, predictions),
    MacroFMeasure    = mldr_MacroFMeasure(trueLabels, bipartition),
    MacroPrecision   = mldr_MacroPrecision(trueLabels, bipartition),
    MacroRecall      = mldr_MacroRecall(trueLabels, bipartition),
    MicroFMeasure    = mldr_MicroFMeasure(trueLabels, bipartition),
    MicroPrecision   = mldr_MicroPrecision(trueLabels, bipartition),
    MicroRecall      = mldr_MicroRecall(trueLabels, bipartition),
    OneError         = mldr_OneError(trueLabels, predictions),
    Precision        = mldr_Precision(counters),
    RankingLoss      = mldr_RankingLoss(trueLabels, predictions),
    Recall           = mldr_Recall(counters),
    SubsetAccuracy   = mldr_SubsetAccuracy(trueLabels, bipartition)
  )
}

# Calculate example based accuracy
mldr_Accuracy <- function(counters) {
  mean((counters$TruePositives + counters$TrueNegatives) / (counters$PredictedPositives + counters$PredictedNegatives))
}

# Calculate example based Average Precision
mldr_AveragePrecision <- function(trueLabels, predictions) {
  mean(unlist(lapply(1:nrow(predictions), function(idr) {
    idxs <- which(trueLabels[idr, ] == 1)
    rk <- order(predictions[idr, ], decreasing = TRUE)

    if(length(idxs) > 0)
      sum(unlist(lapply(idxs, function(k) sum(unlist(lapply(idxs, function(l) rk[k] >= rk[l]))) / rk[k]))) / length(idxs)
  })))
}

# Calculate example based Coverage
mldr_Coverage <- function(trueLabels, predictions) {
  sum(unlist(lapply(1:nrow(predictions), function(idr) {
    idxs <- which(trueLabels[idr, ] == TRUE)
    rk <- order(predictions[idr, ], decreasing = TRUE)
    max(rk[idxs]) -1}))) / nrow(trueLabels)
}

# Calculate example based F-Measure
mldr_FMeasure <- function(counters) {
  precision <- counters$TruePositives / counters$PredictedPositives
  recall <- counters$TruePositives / counters$RealPositives

  mean(precision * recall * 2 / (precision + recall), na.rm = TRUE)
}

# Calculate example based Hamming Loss
mldr_HL <- function(trueLabels, predictions) {
  sum(trueLabels != predictions) / (nrow(trueLabels) * ncol(trueLabels))
}

# Calculate label based Macro FMeasure
mldr_MacroFMeasure <- function(trueLabels, bipartition) {
  macroPrecision <-  colSums(trueLabels & bipartition) / colSums(bipartition)
  macroRecall <- colSums(trueLabels & bipartition) / colSums(trueLabels)

  mean(macroPrecision * macroRecall * 2 / (macroPrecision + macroRecall), na.rm = TRUE)
}

# Calculate label based Macro Precision
mldr_MacroPrecision <- function(trueLabels, bipartition) {
  mean(colSums(trueLabels & bipartition) / colSums(bipartition), na.rm = TRUE)
}

# Calculate label based Macro Recall
mldr_MacroRecall <- function(trueLabels, bipartition) {
  mean(colSums(trueLabels & bipartition) / colSums(trueLabels), na.rm = TRUE)
}

# Calculate label based Micro FMeasure
mldr_MicroFMeasure <- function(trueLabels, bipartition) {
  microPrecision <- mldr_MicroPrecision(trueLabels, bipartition)
  microRecall <- mldr_MicroRecall(trueLabels, bipartition)

  microPrecision * microRecall * 2 / (microPrecision + microRecall)
}

# Calculate label based Micro Precision
mldr_MicroPrecision <- function(trueLabels, bipartition) {
  mean(sum(trueLabels & bipartition) / sum(bipartition), na.rm = TRUE)
}

# Calculate label based Micro Recall
mldr_MicroRecall <- function(trueLabels, bipartition) {
  mean(sum(trueLabels & bipartition) / sum(trueLabels), na.rm = TRUE)
}

# Calculate example based One Error
mldr_OneError <- function(trueLabels, predictions) {
  maxIndex <- apply(predictions, 1, function(r) order(r)[length(r)])
  sum(trueLabels[cbind(1:nrow(trueLabels), maxIndex)] != 1) / nrow(trueLabels)
}

# Calculate example based precision
mldr_Precision <- function(counters) {
  mean(counters$TruePositives / counters$PredictedPositives, na.rm = TRUE)
}

# Calculate example based Ranking Loss
mldr_RankingLoss <- function(trueLabels, predictions) {
  sum(unlist(lapply(1:nrow(trueLabels), function(idr) {
    idxT <- which(trueLabels[idr,] == 1)
    idxF <- which(trueLabels[idr,] == 0)

    if(length(idxT) > 0 && length(idxF) > 0)
      sum(mapply(function(k, l) predictions[idr,k] > predictions[idr,l], idxT, idxF)) / (length(idxT) * length(idxF))
  }))) / nrow(trueLabels)
}

# Calculate example based recall
mldr_Recall <- function(counters) {
  mean(counters$TruePositives / counters$RealPositives, na.rm  = TRUE)
}

# Calculate example based Subset Accuracy
mldr_SubsetAccuracy <- function(trueLabels, predictions) {
  sum(apply(trueLabels == predictions, 1, sum) == ncol(trueLabels)) / nrow(trueLabels)
}

# Calculate label based MacroAUC
mldr_MacroAUC <- function(trueLabels, predictions) {
  mean(unlist(lapply(1:ncol(trueLabels), function(l) if(sum(trueLabels[,l]) == 0) 0.5 else auc(trueLabels[,l], predictions[,l]))))
}

# Calculate example based AUC
mldr_AUC <- function(trueLabels, predictions) {
  TL <- as.matrix(trueLabels)
  idxs <- which(rowSums(trueLabels) != 0 & rowMeans(trueLabels) != 1)
  (sum(!idxs) * 0.5 + sum(unlist(lapply(idxs, function(r) auc(TL[r,], predictions[r, ]))))) / nrow(trueLabels)
}

testMeasures <- function() {
  m <- mldr_from_dataframe(data.frame(F = c(1,1,1,1,1,1), L1 = c(1,0,1,1,1,0), L2 = c(1,1,0,1,1,1)), labelIndices = c(2, 3))
  p <- matrix(c(.75,0.25,0.2,0.75,0.75,0.25,0.9,0.4,0.85,0.3,0.25,0.98), ncol = 2, byrow = T)
  p <- matrix(c(1,0,0,1,1,0,1,0,1,0,0,1), ncol = 2, byrow = T)
  mldr_evaluate(m, p)

  p <- as.matrix(emotions$dataset[,emotions$labels$index])
  p[sample(1:593, 100),sample(1:6, 100, replace = T)] <- sample(0:1, 100, replace = T)
  mldr_evaluate(emotions, p)
}
