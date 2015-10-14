
#' @export
remedial <- function(mld) decoupleImbalancedLabels(mld, mld$measures$scumble)

decoupleImbalancedLabels <- function(mld, atkLevel) {
  mldbase <- mld[.SCUMBLE <= atkLevel]
  mldhigh <- mld[.SCUMBLE > atkLevel]  # Samples with coocurrence of highly imbalanced labels

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

#' @export
concurrenceReport <- function(mld, pdfOutput = FALSE, file = "Rconcurrence.pdf") {
  # Display dataset scumble
  cat("Dataset ", mld$name, ": Mean SCUMBLE ", mld$measures$scumble, " with CV ", mld$measures$scumble.cv, "\n\n",
      sep = "")

  # Display scumble per label
  cat("SCUMBLE mean values by label:\n")
  values <- mld$labels[order(mld$labels$SCUMBLE, decreasing = TRUE),]
  cat(t(cbind("# ", rownames(values), ": ", signif(values$SCUMBLE, digits = 4), "\n")), sep = "")
  cat("\n")

  # Show interesting label interactions
  lblint <- labelInteractions(mld)
  printInteractions(mld, intList = lblint)

  plot(mld, type = "LC", labelIndices = as.numeric(c(lblint$indexes, unique(unlist(lapply(lblint$interactions, names))))))
}

#' @export
labelInteractions <- function(mld, labelProportion) {
  # Extract minority labels
  minority <- mld$labels[mld$labels$IRLbl > mld$measures$meanIR,]
  majority <- mld$labels[mld$labels$IRLbl <= mld$measures$meanIR,]

  if (missing(labelProportion))
    labelProportion <- if (0.1 * nrow(minority) > 3)
        0.1
      else if (nrow(minority) > 3)
        3 / nrow(minority)
      else
        1

  maxScumble <- minority[order(minority$SCUMBLE, decreasing = TRUE)[1:ceiling(nrow(minority) * labelProportion)],]

  labeldata <- mld$dataset[,mld$labels$index]

  # For each selected label, extracts the rest of labels that interact with it
  interactionsByInstance <- lapply(maxScumble$index, function(i) {
    curInstances <- mld$dataset[mld$dataset[i] == 1,]
    indexes <- mld$labels$index[1] + unlist(apply(curInstances[mld$labels$index] == 1, 1, which)) - 1
    table(indexes[indexes %in% majority$index])
  })

  names(interactionsByInstance) <- rownames(maxScumble)

  list(indexes = maxScumble$index, interactions = interactionsByInstance)
}

#' @export
printInteractions <- function(mld, labelProportion = 0.1, intList, interactionLimit = 10) {
  if (missing(intList))
    intList <- labelInteractions(mld, labelProportion)

  for (i in 1:length(intList$indexes)) {
    cat("Minority label ", nameOfLabel(mld, intList$indexes[i]), " (", intList$indexes[i], ", SCUMBLE ", mld$labels[mld$labels$index == intList$indexes[i], ]$SCUMBLE, ") interacts with:\n", sep="")

    intTable <- sort(intList$interactions[[i]], decreasing = TRUE)

    if (interactionLimit > 0)
      intTable <- intTable[1:min(interactionLimit, length(intTable))]

    for (l in 1:length(intTable)) {
      cat("# ", nameOfLabel(mld, names(intTable)[l]),
          " (", names(intTable)[l], ", SCUMBLE ", mld$labels[mld$labels$index == names(intTable)[l], ]$SCUMBLE, "): ",
          intTable[l], " interaction", ifelse(intTable[l] == 1, "", "s"), "\n", sep="")
    }

    cat("\n")
  }
}

nameOfLabel <- function(mld, index) {
  rownames(mld$labels[mld$labels$index == index,])
}
