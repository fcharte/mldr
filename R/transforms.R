#' Transformns an MLDR into binary or multiclass datasets
#' @description Transforms an \code{mldr} object into one or serveral binary or multiclass datasets, returning them as \code{data.frame} objects
#' @param mldr The mldr object to transform
#' @param type Indicates the type of transformation to apply. Possible types are:
#'  "BR" Produces one or more binary datasets, each one with one label
#'  "LP" Produces a multiclass dataset using each labelset as class label
#' @param labels Vector with the label indexes to include in the transformation. All labels will be used if not specified
#' @return A list containing the resulting datasets
#' @examples
#'
#' library(mldr)
#' emotionsbr <- mldr_transform(emotions, type = "BR")
#'
#' @export

mldr_transform <- function(mldr,  type = 'BR', labels) {
  if(class(mldr) != 'mldr')
    stop('This method applies only to mldr objects')

  if(missing(labels))
    labels <- mldr$labels$index
  else
    labels <- labels[labels %in% mldr$labels$index]

  if(length(labels) == 0)
    stop('One or more labels have to be selected')

  switch(type,
         BR = mldr_to_BR(mldr, labels),
         LP = mldr_to_LP(mldr, labels)
         )
}

mldr_to_BR <- function(mldr, labels) {
  lapply(labels, function(aLabel) mldr$dataset[,c(mldr$attributesIndexes, aLabel)])
}

mldr_to_LP <- function(mldr, labels) {
  if(class(mldr) != 'mldr') stop('This method applies only to mldr objects')

}
