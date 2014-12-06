#'
#'

write.mldr <- function(obj, filename) {
  connection <- file(filename)

  header <- paste("@relation ", obj$name, sep = "")

  attributes <- paste("@attribute ", names(obj$attributes), attributes, sep = "")

  writeLines(c(header, "", attributes), connection)

  data
}
