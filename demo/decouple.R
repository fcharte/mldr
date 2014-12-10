#
# Shows how to implement a filter over a MLD to decouple highly imbalanced labels
#
decoupleImbalancedLabels <- function(mld) {
  mldbase <- mld[.Atkinson <= mld$measures$scumble]
  mldhigh <- mld[.Atkinson > mld$measures$scumble]  # Samples with coocurrence of highly imbalanced labels

  minIndexes <- mld$labels[mld$labels$IRLbl > mld$measures$meanIR,"index"]
  majIndexes <- mld$labels[mld$labels$IRLbl <= mld$measures$meanIR,"index"]

  ninstances <- mldhigh$measures$num.instances
  mldhigh$dataset[(ninstances+1):(ninstances*2),] <- mldhigh$dataset

  mldhigh$dataset[1:ninstances, minIndexes] <- 0
  mldhigh$dataset[(ninstances+1):(ninstances*2), majIndexes] <- 0

  mldbase + mldhigh
}

decoupled.emotions <- decoupleImbalancedLabels(emotions)

summary(emotions)
summary(decoupled.emotions) # Reduced number of labelsets and lower cardinality, density and scumble
