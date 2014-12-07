#' Checks if two mldr objects have the same structure
#'
#' @param mldr1, mldr2 mldr objects to comare
#' @return true if the two mldr have the same structure
"==.mldr" <- function(mldr1, mldr2) {
  length(mldr1$attributes) == length(mldr2$attributes) &&
    names(mldr1$attributes) == names(mldr2$attributes) &&
    mldr1$attributes == mldr2$attributes
}

#' Generates a new mldr object joining the rows
#' in the two mldrs given as input
#'
#' @param mldr1, mldr2 original mldr objects
#' @return a new mldr object with all rows in the two parameters
"+.mldr" <- function(mldr1, mldr2) {
  # Check the two mldr's structure
  if(mldr1 == mldr2)
      updateMldr(mldr1, rbind(mldr1$dataset, mldr2$dataset))
  else
    stop(paste(substitute(mldr1), " and ", substitute(mldr2), " don't have the same structure"))
}
