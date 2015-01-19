#' @title Creates an object representing a multilabel dataset
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
#' @seealso \code{\link{mldr_from_dataframe}}, \code{\link{summary.mldr}}
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

    mldr_from_dataframe(dataset, labeli, header$name)
  } else {
    NULL
  }
}

#' @title Generates an mldr object from a data.frame and a vector with label indices
#' @description This function creates a new \code{mldr} object from the data
#' stored in a \code{data.frame}, taking as labels the columns pointed by the
#' indexes given in a vector.
#' @param dataframe The \code{data.frame} containing the dataset attributes and labels.
#' @param labelIndices Vector containing the indexes of attributes acting as labels. Usually the
#' labels will be at the end (right-most columns) or the beginning (left-most columns) of the \code{data.frame}
#' @param name Name of the dataset. The name of the dataset given as first parameter will be used by default
#' @return An mldr object containing the multilabel dataset
#' @seealso \code{\link{mldr}}, \code{\link{summary.mldr}}
#' @examples
#'
#' library(mldr)
#'
#' df <- data.frame(matrix(rnorm(1000), ncol = 10))
#' df$Label1 <- c(sample(c(0,1), 100, replace = TRUE))
#' df$Label2 <- c(sample(c(0,1), 100, replace = TRUE))
#' mymldr <- mldr_from_dataframe(df, labelIndices = c(11, 12), name = "testMLDR")
#'
#' summary(mymldr)
#'
#' @export
#'
mldr_from_dataframe <- function(dataframe, labelIndices, name = NULL) {
  new_mldr <- list()
  new_mldr$name <- if(missing(name)) substitute(dataframe) else name
  new_mldr$dataset <- dataframe

  new_mldr$attributes <- sapply(new_mldr$dataset, class)
  new_mldr$attributes[labelIndices] <- "{0,1}"
  factorIndexes <- which(new_mldr$attributes == "factor")
  if(length(factorIndexes > 0))
    new_mldr$attributes[factorIndexes] <- sapply(factorIndexes,
      function(idx) paste("{", paste(
        levels(new_mldr$dataset[, names(new_mldr$attributes)[idx]]),
        collapse = ","), "}", sep = ""))

  new_mldr$labels <- label_measures(dataframe, labelIndices)
  new_mldr$labelsets <- sort(table(as.factor(do.call(paste, c(dataframe[, new_mldr$labels$index], sep = "")))))
  new_mldr$dataset <- dataset_measures(new_mldr)
  new_mldr$measures <- measures(new_mldr)
  new_mldr$labels$SCUMBLE <- sapply(new_mldr$labels$index, function(idx)
    mean(new_mldr$dataset$.SCUMBLE[new_mldr$dataset[,idx] == 1]))

  class(new_mldr) <- "mldr"

  new_mldr
}

#' The \pkg{mldr} package provides a basic, Shiny-based GUI to work with multilabel datasets.
#' You have to install the \pkg{shiny} package to be able to use this GUI.
#'
#' The user interface allows working with any of the previous loaded datasets, as well as loading
#' new ones. The GUI is structured into the following pages:
#' \itemize{
#'   \item{\strong{Main:}}{ This page is divided into two sections.
#'   The one at the left can be used to choose apreviously loaded dataset,
#'   as well as to load datasets from files. The right part shows some basic
#'   statistics about the selected multilabel dataset.}
#'   \item{\strong{Labels:}}{ This page shows a table containing for each label its name, index, count, relative frequency
#'   and imbalance ratio (IRLbl). The labels can be sorted for any of these fields. The page also includes a bar plot
#'   of the label frequency. The range of labels in the plot can be customized.}
#'   \item{\strong{Labelsets:}}{ This page shows a table containing for each labelset its representation and a counter.}
#'   \item{\strong{Attributes:}}{ This page shows a table containing for each attribute its name, type and a summary
#'   of its values.}
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
    shiny::runApp(appDir = system.file("shiny", package="mldr"), launch.browser = TRUE)

  invisible()
}

