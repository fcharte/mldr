#' @title Multilabel classifier constructor
#' @description Creates an object of class 'mlcr'. No algorithms are provided within
#'   the `mldr` package, instead new packages that provide them can be installed or
#'   single code files can be used.
#' @param type String identifying the algorithm to be used
#' @seealso \code{\link{print.mlcr}}
#' @examples
#'
#' classifier <- mlcr("mlknn", emotions)
#'
#' @export
mlcr <- function(type) {
  if (missing(type))
    stop("No type provided for multilabel classifier!")

  checkForMethod <- function(generic, type) {
    if (!exists(paste0(generic, ".", type)) || class(get0(paste0(generic, ".", type))) != "function") {
      warning("There is no '", generic, "' function defined for type ",
              type, ". You may not be able to train or test this classifier.")
    }
  }

  checkForMethod("trainMlc", type)
  checkForMethod("predictInstance", type)

  classifier <- list()
  classifier$type <- type
  classifier$trained <- FALSE
  class(classifier) <- c("mlcr", type)

  classifier
}

#' @title Print method for multilabel classifiers
#' @description Outputs info about the specified classifier when it's trained.
#' @param classifier `mlcr` object
#' @seealso \code{\link{mlcr}}
#' @examples
#'
#' classifier <- mlcr("mlknn", emotions)
#' print(classifier)
#'
#' @export
print.mlcr <- function(classifier) {
  cat("Multilabel classifier:", classifier$type, "\n")

  cat("Parameters:\n")
  pars <- names(classifier)
  pars <- pars[!(pars %in% c("type", "trainInfo"))]

  for (p in pars) {
    if (typeof(classifier[[p]]) %in% c("double", "character"))
      cat("\t", p, ": ", classifier[[p]], "\n", sep="")
  }

  cat("\n")

  if (classifier$trained)
    cat(classifier$trainInfo)
  else
    cat("Classifier is not trained. Use 'train' function.")
}

#' @export
train <- function(classifier, trainingSet) {
  classifier <- trainMlc(classifier, trainingSet)
  classifier$trained <- TRUE
  classifier
}

#' @export
test <- function(classifier, testSet) {
  sapply(as.data.frame(t(testSet$dataset)), function(instance) predictInstance(classifier, instance))
}

# Generics: one method of each type will be defined for each classifier
trainMlc <- function(classifier, ...) {
  UseMethod("trainMlc")
}

predictInstance <- function(classifier, ...) {
  UseMethod("predictInstance")
}
