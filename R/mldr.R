#' Creates an object representing a multilabel
#' dataset
#'
#' @param filename Name of the dataset
#' @param use_xml Specifies whether to use an
#'  associated XML file to identify the labels.
#' @param auto_extension Specifies whether to add
#'  the '.arff' and '.xml' extensions to the filename
#'  where appropriate
#' @param xml_file Path to the XML file. If not
#'  provided, the filename ending in ".xml" will be
#'  assumed
#' @export

mldr <- function(filename = NULL,
                 use_xml = TRUE,
                 auto_extension = TRUE,
                 xml_file = NULL) {

   if (!is.null(filename)) {
    # Parameter check
    if (!is.character(filename))
      stop("Argument 'filename' must be a character string.")
    if (!is.null(xml_file) && !is.character(xml_file))
      stop("Argument 'xml_file' must be a character string.")

    # Calculate names of files
    arff_file <- if (auto_extension)
        paste(filename, ".arff", sep="")
      else
        filename

    if (is.null(xml_file)) xml_file <- if (auto_extension)
        paste(filename, ".xml", sep="")
      else {
        noext <- unlist(strsplit(filename, ".", fixed=TRUE))
        paste(noext[1:length(noext)], ".xml", sep="")
      }

    # Get file contents
    relation <- NULL
    attrs <- NULL
    contents <- read_arff(arff_file)
    relation <- contents$relation
    attrs <- contents$attributes
    dataset <- contents$dataset
    rm(contents)

    header <- read_header(relation)

    if (use_xml) {
      # Read labels from XML file
      labelnames <- read_xml(xml_file)
      labeli <- which(names(attrs) %in% labelnames)

      #spl <- split(attrs, attrs$name %in% labelnames)
      #labels <- spl$`TRUE`
      #features <- spl$`FALSE`
    } else {
      # Read label amount from Meka parameters

      labeli <- 1:header$toplabel
      #features <- attrs[toplabel+1:length(attrs),]
    }


    # Convert labels to numeric
    dataset[, labeli] <- lapply(dataset[, labeli],
                                    function(col) as.numeric(!is.na(as.numeric(col) | NA)))

    # Change type from factor of strings to numeric
    dataset[, which(attrs == "numeric")] <-
      lapply(dataset[, which(attrs == "numeric")], as.numeric)

    updateMldr(list(
      name = header$name,
      attributes = attrs,
      labels = t(dataset[1, labeli])
      ), dataset
    )
  } else {
    NULL
  }
}

updateMldr <- function(mldr, dataset) {
  newMldr <- list()
  newMldr$name <- mldr$name
  newMldr$dataset <- dataset
  newMldr$attributes <- mldr$attributes
  newMldr$labels <- label_measures(dataset, which(names(mldr$attributes) %in% rownames(mldr$labels)))
  newMldr$labelsets <-   sort(table(as.factor(do.call(paste, c(dataset[, newMldr$labels$index], sep = "")))))
  newMldr$dataset <- dataset_measures(newMldr)
  newMldr$measures <- measures(newMldr)

  class(newMldr) <- "mldr"

  newMldr
}

#' Launchs the web-based GUI for mldr
#' @title Launchs the web-based GUI for mldr
#' @return Nothing
#' @description mldrGUI starts loads the web browser an interactive user interface built using R shiny.
#' @examples
#' \dontrun{
#' library(mldr)
#' mldrGUI()
#' }
#' @export

mldrGUI <- function() {
  shiny::runApp(appDir = system.file("shiny", package="mldr"))
}
