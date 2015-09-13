
################################
#
# Other package/file

#' @import mldr
mlknn <- function(...) mlcr("mlknn", ...)

initializeMlc.mlknn <- function(baseClassifier, numOfNeighbors = 10, smooth = 1,
                                distance = function(x_i, y_i) {
                                  sqrt(sum((x_i - y_i)^2))
                                }) {
  baseClassifier$parameters$numOfNeighbors <- numOfNeighbors
  baseClassifier$parameters$smoothFactor <- smooth
  baseClassifier$parameters$distance <- distance

  # K nearest neighbors function
  baseClassifier$kNearest <- function(instance, dataset, k) {
    # Calculate distance from each dataset sample to the current instance
    dataset <- data.frame(dataset, dist = baseClassifier$parameters$distance(dataset, instance))

    # Reorder dataset according to the measured distance
    dataset <- dataset[order(dataset$dist)]

    # Capture first k instances
    dataset[1:k]
  }

  # Initializer function MUST return the modified classifier object
  baseClassifier
}

trainMlc.mlknn <- function(classifier, trainingSet) {
  kNearest <- classifier$kNearest

  # Apriori probabilities go here

  classifier$priorProbabilities <- sapply(trainingSet$labels$index, function(label) {
    tempCi <- sum(trainingSet$dataset[[label]])
    (classifier$parameters$smoothFactor + tempCi) / (2 * classifier$parameters$smoothFactor + length(trainingSet$dataset[[label]]))
  })

  classifier$priorNProbabilities <- 1 - classifier$priorProbabilities

  # Accumulated will be a 2-element list: a matrix with positive accumulated
  # values and another one with the negatives
  accumulated <- Reduce(function(acc, li) {
    acc[[1]] <- acc[[1]] + li[[1]]
    acc[[2]] <- acc[[2]] + li[[2]]
    acc
  }, apply(trainingSet$dataset, 1, function(instance) {
    knn <- kNearest(instance, trainingSet$dataset, classifier$parameters$numNeighbors)

    # This returns a matrix with labels as columns, k nearest instances as rows
    acesByLabels <- sapply(trainingSet$labels$index, function(label) {
      aces <- sum(knn$dataset[[label]])
      expanded <- matrix(data = 0, ncol = knn$measures$num.instances)

      # Different result according to whether the instance has the label or not
      if (instance[[label]] == 1)
        expanded[1, aces] <- 1
      else
        expanded[1, aces] <- -1

      expanded
    })

    # Separate 1 and -1 results in different matrices
    positives <- acesByLabels
    positives[positives < 0] <- 0

    negatives <- acesByLabels
    negatives[negatives > 0] <- 0
    negatives <- -negatives
    list(
      positives,
      negatives
    )
  }))

  accpos <- accumulated[[1]]
  accneg <- accumulated[[2]]

  classifier$condProbabilities <- sapply(1:trainingSet$measures$num.labels, function(label) {
    temp <- sum(accpos[label])
    sapply(0:classifier$parameters$numOfNeighbors, function(j) {
      (classifier$parameters$smoothFactor + accpos[label][j]) /
        (classifier$parameters$smoothFactor * (classifier$parameters$numOfNeighbors + 1) + temp)
    })
  })

  classifier$condNProbabilities <- sapply(1:trainingSet$measures$num.labels, function(label) {
    temp <- sum(accneg[label])
    sapply(0:classifier$parameters$numOfNeighbors, function(j) {
      (classifier$parameters$smoothFactor + accneg[label][j]) /
        (classifier$parameters$smoothFactor * (classifier$parameters$numOfNeighbors + 1) + temp)
    })
  })

  classifier$trainingSet <- trainingSet
  classifier$trainInfo <- paste0("The classifier is trained for ", trainingSet$name, ".")

  # Training function MUST return the (possibly modified) classifier object
  classifier
}

predictInstance.mlknn <- function(classifier, instance) {
  knn <- classifier$kNearest(instance, classifier$trainingSet, classifier$parameters$numNeighbors)

  wrapper <- sapply(1:classifier$trainingSet$measures$num.labels, function(label) {
    lIndex <- classifier$trainingSet$labels$index[label]
    aces <- sum(knn$dataset[[lIndex]])

    probIn <- classifier$priorProbabilities[label] * classifier$condProbabilities[aces, label]
    probOut <- classifier$priorNProbabilities[label] * classifier$condNProbabilities[aces, label]

    if (probIn != probOut)
      c(prediction = probIn > probOut, confidence = probIn / (probIn + probOut))
    else
      c(prediction = sample(0:1, 1),   confidence = probIn / (probIn + probOut))
  })
}
