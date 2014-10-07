#
# Contains functions for calculating measures
# for a multilabel dataset
#

measures <- function(mld) {
  labelsets <- do.call(paste, c(mld$dataset[, mld$labels$index], sep = ""))
  labelsets <- table(as.factor(labelsets))

  list(
    num.attributes = length(names(mld$dataset)),
    num.instances = nrow(mld$dataset),
    num.labels = nrow(mld$labels),

    # Number of different labelsets
    num.labelsets = length(labelsets),

    # Number of labelsets appearing only once
    num.single.labelsets = sum(labelsets == 1),

    # Maximum frequency on a labelset
    max.frequency = max(labelsets),

    cardinality = mean(mld$dataset$.labelcount),
    density = mean(mld$dataset$.labelcount) / nrow(mld$labels),
    meanIR = mean(mld$labels$IRLbl)
  )
}

label_measures <- function(data, indexes) {
  msr <- data.frame(
    index = indexes,
    count = colSums(data[indexes])
  )

  msr$IRLbl <- max(msr$count) / msr$count
  msr$freq <- msr$count / nrow(data)

  msr
}

dataset_measures <- function(mld) {
  mld$dataset$.labelcount <- rowSums(mld$dataset[, mld$labels$index])

  IRs <- data.frame(t(t(mld$dataset[, mld$labels$index]) * mld$labels$IRLbl))
  means <- rowSums(IRs) / mld$dataset$.labelcount
  IRs[IRs == 0] <- 1 # Identity element for (R, *)
  IRprod <- Reduce("*", IRs)
  mld$dataset$.Atkinson <- 1 - (IRprod)^(1/mld$dataset$.labelcount) / means

  mld$dataset
}
