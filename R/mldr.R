#' Creates an object representing a multilabel
#' dataset
#' @description Reads a multilabel dataset from a file and returns an \code{mldr} object
#' containing the data and additional measures. The file has to be in ARFF format.
#' The label information could be in a separate XML file (MULAN style) or in the
#' the arff header (MEKA style)
#' @param filename Name of the dataset
#' @param use_xml Specifies whether to use an
#'  associated XML file to identify the labels. Defaults to TRUE
#' @param auto_extension Specifies whether to add
#'  the '.arff' and '.xml' extensions to the filename
#'  where appropriate. Defaults to TRUE
#' @param xml_file Path to the XML file. If not
#'  provided, the filename ending in ".xml" will be
#'  assumed
#' @return An mldr object containing the multilabel dataset
#' @seealso \code{\link{summary.mldr}}
#' @examples
#'
#' library(mldr)
#'
#' # Read "yeast.arff" and labels from "yeast.xml"
#' mymld <- mldr("yeast")
#'
#' # Read "yeast-tra.arff" and labels from "yeast.xml"
#' mymld <- mldr("yeast-tra", xml_file = "yeast.xml")
#'
#' # Read MEKA style dataset, without XML file and giving extension
#' mymld <- mldr("IMDB.arff", use_xml = FALSE, auto_extension = FALSE)
#'
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
        paste(filename, ".xml", sep = "")
      else {
        noext <- unlist(strsplit(filename, ".", fixed = TRUE))
        paste(noext[1:length(noext)], ".xml", sep = "")
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
    } else {
      # Read label amount from Meka parameters
      labeli <- 1:header$toplabel
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

#' Updates the mldr state after changing the internal dataset
#' @title Updates the mldr state after changing the internal dataset
#' @return A new mldr

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

#' The \code{mldr} package provides a basic, Shiny-based GUI to work with multilabel datasets.
#' You have to install the \code{shiny} package to be able to use this GUI.
#'
#' The user interface allows working with any of the previous loaded datasets, as well as loading
#' new ones. The GUI is structured into the following pages:
#' \itemize{
#'   \item{\strong{Main:}}{This page is divided into two sections.
#'   The one at the left can be used to choose apreviously loaded dataset,
#'   as well as to load datasets from files. The right part shows some basic
#'   statistics about the selected multilabel dataset.}
#'   \item{\strong{Labels:}}{}
#'   \item{\strong{Labelsets:}}{}
#'   \item{\strong{Attributes:}}{}
#' }
#' @title Launchs the web-based GUI for mldr
#' @return Nothing
#' @description Loads in the web browser an interactive user interface built using R shiny.
#' @examples
#'
#' library(mldr)
#' mldrGUI()
#'
#' @import shiny
#' @export

mldrGUI <- function() {
  if(require(shiny))
    shiny::runApp(appDir = system.file("shiny", package="mldr"))

  invisible()
}

