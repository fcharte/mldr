#'
#' Contains operators to manipulate mldr objects
#'

#' Generates a new mldr object joining the rows
#' in the two mldrs given as input
#'
#' @param mldrObject1, mldrObject2 original mldr objects
#' @return a new mldr object with all rows in the two parameters
"+.mldr" <- function(mldrObject1, mldrObject2) {
  # Check the two mldr's structure


  updateMldr(mldrObject1,
             rbind(mldrObject1$dataset, mldrObject2$dataset)
  )
}
