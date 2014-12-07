#' Generates a new mldr object containing the
#' selected rows
#'
#' @param mldrObject original mldr object
#' @param rowFilter expression to filter the rows
#' @return a new mldr object with the selected rows
#' @export

"[.mldr" <- function(mldrObject, rowFilter = T, ...) {
  rowFilter <- substitute(rowFilter)
  rows <- eval(rowFilter, mldrObject$dataset, parent.frame())
  newDataset <- mldrObject$dataset[rows,]

  updateMldr(mldrObject, newDataset)
}
