#
# Contains functions for calculating measures
# for a multilabel dataset
#

measures <- function(mld) {
  labelsets <- do.call(paste, c(mld$dataset[, mld$labels$index], sep = ""))
  labelsets <- table(as.factor(labelsets))

  list(
    num.attributes = length(names(mld$dataset)) - 2, # 2 columns are measures!
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
    meanIR = mean(mld$labels$IRLbl),
    scumble = mean(mld$dataset$.Atkinson)
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

  # Atkinson index for IR in each instance
  IRs <- data.frame(t(t(mld$dataset[, mld$labels$index]) * mld$labels$IRLbl))
  IRmeans <- rowSums(IRs) / mld$dataset$.labelcount

  # Atkinson = 1 - exp(mean(log(IRs))) / mean(IRs)
  # Applying properties of the logarithm:
  # Atkinson = 1 - exp(log(prod(IRs)^(1/n))) / mean(IRs)
  # Atkinson = 1 - prod(IRs)^(1/n) / mean(IRs)
  IRs[IRs == 0] <- 1            # Identity element for (R, *)
  IRprod <- Reduce("*", IRs)    # Row products
  mld$dataset$.Atkinson <- ifelse(mld$dataset$.labelcount > 0,
                                  1 - (IRprod)^(1/mld$dataset$.labelcount) / IRmeans,
                                  0)

  mld$dataset
}
