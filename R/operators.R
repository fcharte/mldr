#' Checks if two mldr objects have the same structure
#'
#' @param mldr1, mldr2 mldr objects to compare
#' @return true if the two mldr objects have the same structure
#' @export
"==.mldr" <- function(mldr1, mldr2) {
  length(mldr1$attributes) == length(mldr2$attributes) &&
    names(mldr1$attributes) == names(mldr2$attributes) &&
    mldr1$attributes == mldr2$attributes
}

#' Generates a new mldr object joining the rows
#' in the two mldrs given as input
#'
#' @param mldr1, \code{mldr2} original \code{mldr} objects
#' @return a new \code{mldr} object with all rows in the two parameters
#' @export
"+.mldr" <- function(mldr1, mldr2) {
  # Check the two mldr's structure
  if(mldr1 == mldr2)
    mldr_from_dataframe(
      rbind(subset(mldr1$dataset, select = -c(.labelcount, .SCUMBLE)),
            subset(mldr2$dataset, select = -c(.labelcount, .SCUMBLE))),
      mldr1$labels$index,
      mldr1$name
    )
  else
    stop(paste(substitute(mldr1), " and ", substitute(mldr2), " don't have the same structure"))
}
