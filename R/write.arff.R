#' Writes an ARFF and an XML file for
#' a mldr object
#'
#' @param obj 'mldr' object
#' @param filename base name for the files (without extension)
#' @export
write_arff <- function(obj, filename, write.xml = FALSE) {
  # Open file
  connection <- file(paste(filename, ".arff", sep = ""))

  # Create header an attribute lines
  header <- paste("@relation ", obj$name, sep = "")
  attributes <- paste("@attribute ", names(obj$attributes), " ", obj$attributes, sep = "")
  data <- obj$dataset[, 1:obj$measures$num.attributes]
  data[is.na(data)] <- '0' # NAs aren't missing values ('?') but 0
  data <- apply(data, 1, function(c) paste(c, collapse = ','))

  # Write header, attributes and data
  writeLines(c(header, "", attributes, "", "@data", data), connection)
  close(connection)

  if(write.xml) {
    # Open XML file
    connection <- file(paste(filename, ".xml", sep = ""))

    xmlheader <- '<?xml version="1.0" encoding="utf-8"?>'
    labelstag <- '<labels xmlns="http://mulan.sourceforge.net/labels">'
    labeltags <- paste(c('<label name="'), rownames(obj$labels), c('"></label>'), sep = "")
    labelsend <- '</labels>'

    writeLines(c(xmlheader, labelstag, labeltags, labelsend), connection)
    close(connection)
  }
}
