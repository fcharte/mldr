#' @export
summary.mldr <- function(mld) {
  print(data.frame(mld$measures))
}

#' @export
print.mldr <- function(mld) {
  print(mld$dataset[,1:(ncol(mld$dataset)-2)])
}


