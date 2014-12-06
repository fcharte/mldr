#'
#'

write.mldr <- function(obj, filename) {
  # Open file
  connection <- file(paste(filename, ".arff"))

  # Create header an attribute lines
  header <- paste("@relation ", obj$name, sep = "")
  attributes <- paste(c("@attribute "), names(obj$attributes), c(" "), obj$attributes, sep = "")

  # Write header, attributes and data
  writeLines(c(header, "", attributes, "", "@data"), connection)
  write(t(
    obj$dataset[, 1:obj$measures$num.attributes]
  ), filename, ncol = obj$measures$num.attributes, append = T, sep = ",")

  close(connection)

  # Open XML file
  connection <- file(paste(filename, ".xml", sep = ""))

  xmlheader <- '<?xml version="1.0" encoding="utf-8"?>'
  labelstag <- '<labels xmlns="http://mulan.sourceforge.net/labels">'
  labeltags <- paste(c('<label name="'), rownames(obj$labels), c('"></label>'), sep = "")
  labelsend <- '</labels>'

  writeLines(c(xmlheader, labelstag, labeltags, labelsend), connection)
  close(connection)
}
