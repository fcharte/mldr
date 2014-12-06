#'
#' Contains functions to filter rows
#'

#' Generates a new mldr object containing the
#' selected rows
#'
#' @param original mldr object
#' @param expression to filter the rows
#' @return a new mldr object with the selected rows

"[.mldr" <- function(mldrObject, rowFilter, ...) {
  rowFilter <- substitute(rowFilter)
  rows <- eval(rowFilter, mldrObject$dataset, parent.frame())
  newDataset <- mldrObject$dataset[rows,]
  print("Hooola")
  return(updateMldr(mldrObject, newDataset))
}
