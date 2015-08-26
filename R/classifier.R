#' @title Multilabel classifier constructor
#' @description Creates an object of class 'mlcr'. No algorithms are provided within
#'   the `mldr` package, instead new packages that provide them can be installed or
#'   single code files can be used.
#' @param type String identifying the algorithm to be used
#' @param ... Parameters to be passed to the initializer
#' @seealso \code{\link{print.mlcr}}
#' @examples
#'
#' # Dummy implementation for example classifier
#' trainMlc.example <- function(classifier, trainingSet) classifier
#' predictInstance.example <- function(classifier, instance) 0
#'
#' # Create an instance of this type of classifier
#' classifier <- mlcr("example", emotions)
#'
#' @export
mlcr <- function(type, ...) {
  if (missing(type))
    stop("No type provided for multilabel classifier!")

  checkForMethod <- function(generic, type) {
    exists(paste0(generic, ".", type)) && class(get0(paste0(generic, ".", type))) != "function"
  }

  if (!checkForMethod("trainMlc", type))
    warning("There is no trainMlc function defined for type ",
            type, ". You may not be able to train this classifier.")

  if (!checkForMethod("predictInstance", type))
    warning("There is no predictInstance function defined for type ",
            type, ". You may not be able to test this classifier.")

  classifier <- list()
  classifier$type <- type
  classifier$trained <- FALSE
  class(classifier) <- c(type, "mlcr")

  if (checkForMethod("initializeMlc", type)) {
    classifier <- initializeMlc(classifier, ...)
  }

  classifier
}

#' @title Print method for multilabel classifiers
#' @description Outputs info about the specified classifier when it's trained.
#' @param x `mlcr` object
#' @param ... Extra parameters. Currwntly ignored
#' @seealso \code{\link{mlcr}}
#'
#' @export
print.mlcr <- function(x, ...) {
  cat("Multilabel classifier:", x$type, "\n")

  cat("Parameters: ")
  str(x$parameters)
  cat("\n")

  if (x$trained)
    cat(x$trainInfo)
  else
    cat("Classifier is not trained. Use 'train' function.\n")
}

#' @title Training method for multilabel classifiers
#' @description Delegates on methods specific to the algorithm to train the classifier
#' @param classifier `mlcr` object
#' @param trainingSet An `mldr` with the training instances
#' @seealso \code{\link{mlcr}}
#'
#' @export
train <- function(classifier, trainingSet) {
  classifier <- trainMlc(classifier, trainingSet)
  classifier$trained <- TRUE
  classifier
}

#' @title Test method for multilabel classifiers
#' @description Uses methods specific to the algorithm in order to retrieve predictions.
#' @param classifier `mlcr` object
#' @param testSet An `mldr` with the test instances
#' @seealso \code{\link{mlcr}}
#'
#' @export
test <- function(classifier, testSet) {
  sapply(as.data.frame(t(testSet$dataset)), function(instance) predictInstance(classifier, instance))
}

# Generics: one method of each type will be defined for each classifier
initializeMlc <- function(classifier, ...) UseMethod("initializerMlc")
trainMlc <- function(classifier, ...) UseMethod("trainMlc")
predictInstance <- function(classifier, ...) UseMethod("predictInstance")
