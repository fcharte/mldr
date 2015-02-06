#' @title Provides a summary of measures about the mldr
#' @description Prints a summary of the measures obtained from the \code{mldr} object
#' @param mld Object whose measures are to be printed
#' @seealso \code{\link{mldr}}, \code{\link{print.mldr}}
#' @examples
#'
#' library(mldr)
#'
#' summary(emotions)
#'
#' @export
summary.mldr <- function(mld) {
  print(data.frame(mld$measures))
}

#' @title Prints the mldr content
#' @description Prints the \code{mldr} object data, including input attributs and output labels
#' @param mld Object whose data are to be printed
#' @seealso \code{\link{mldr}}, \code{\link{summary.mldr}}
#' @examples
#'
#' library(mldr)
#'
#' emotions
#' print(emotions) # Same as above
#'
#' @export
print.mldr <- function(mld) {
  print(mld$dataset[ , c(mld$attributesIndexes, mld$labels$index)])
}


