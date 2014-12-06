#'
#' Contains functions to filter rows
#'

#' Generates a new mldr object containing the
#' selected rows
#'
#' @param original mldr object
#' @param expression to filter the rows
#' @return a new mldr object with the selected rows

"[.mldr" <- function(mldrObject, rowFilter = T, ...) {
  rowFilter <- substitute(rowFilter)
  rows <- eval(rowFilter, mldrObject$dataset, parent.frame())
  newDataset <- mldrObject$dataset[rows,]

  return(updateMldr(mldrObject, newDataset))
}
