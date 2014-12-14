#
# Shows how to implement a filter over a MLD to decouple highly imbalanced labels
#
decoupleImbalancedLabels <- function(mld) {
  mldbase <- mld[.Atkinson <= mld$measures$scumble]
  mldhigh <- mld[.Atkinson > mld$measures$scumble]  # Samples with coocurrence of highly imbalanced labels

  # Indexes of minority and majority labels
  minIndexes <- mld$labels[mld$labels$IRLbl > mld$measures$meanIR,"index"]
  majIndexes <- mld$labels[mld$labels$IRLbl <= mld$measures$meanIR,"index"]

  # Duplicate rows affected by coocurrence of highly imbalanced labels
  ninstances <- mldhigh$measures$num.instances
  mldhigh$dataset[(ninstances+1):(ninstances*2),] <- mldhigh$dataset

  # Decouple majority and minority labels
  mldhigh$dataset[1:ninstances, minIndexes] <- 0
  mldhigh$dataset[(ninstances+1):(ninstances*2), majIndexes] <- 0

  mldbase + mldhigh # Join the instances without changes with the filtered ones
}

deactivateImbalancedLabels <- function(mld) {
  mldbase <- mld[.Atkinson <= mld$measures$scumble]
  mldhigh <- mld[.Atkinson > mld$measures$scumble]  # Samples with coocurrence of highly imbalanced labels

  majIndexes <- mld$labels[mld$labels$IRLbl < mld$measures$meanIR,"index"]

  # Deactivate majority  labels
  mldhigh$dataset[, majIndexes] <- 0

  mldbase + mldhigh # Join the instances without changes with the filtered ones
}

# Test de function with emotions multilabel dataset
decoupled.emotions <- decoupleImbalancedLabels(emotions)

summary(emotions)
summary(decoupled.emotions) # Reduced number of labelsets and lower cardinality, density and scumble

deactivated.emotions <- deactivateImbalancedLabels(emotions)

summary(emotions)
summary(deactivated.emotions)
