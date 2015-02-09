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
#'\dontrun{
#' # Read "yeast.arff" and labels from "yeast.xml"
#' mymld <- mldr("yeast")
#'
#' # Read "yeast-tra.arff" and labels from "yeast.xml"
#' mymld <- mldr("yeast-tra", xml_file = "yeast.xml")
#'
#' # Read MEKA style dataset, without XML file and giving extension
#' mymld <- mldr("IMDB.arff", use_xml = FALSE, auto_extension = FALSE)
#'}
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

    if (is.null(xml_file))
      xml_file <- if (auto_extension)
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

    # Adjust type of numeric attributes
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
#' @param labelIndices Vector containing the indices of attributes acting as labels. Usually the
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
  if(!is.data.frame(dataframe))
    stop(paste(substitute(dataframe), "is not a valid data.frame"))

  if(missing(labelIndices))
    stop("labelIndices parameter is compulsory")

  if(!is.numeric(labelIndices))
    stop(paste(substitute(labelIndices), "is not a numeric vector"))

  new_mldr <- list()
  new_mldr$name <- if(missing(name)) substitute(dataframe) else name
  new_mldr$dataset <- dataframe

  new_mldr$attributesIndexes <- 1:length(dataframe)
  new_mldr$attributesIndexes <- new_mldr$attributesIndexes[! new_mldr$attributesIndexes %in% labelIndices]
  new_mldr$attributes <- sapply(new_mldr$dataset, class)
  new_mldr$attributes[labelIndices] <- "{0,1}"
  factorIndexes <- which(new_mldr$attributes == "factor")
  if(length(factorIndexes > 0))
    new_mldr$attributes[factorIndexes] <- sapply(factorIndexes,
                                                 function(idx) paste("{", paste(
                                                   levels(new_mldr$dataset[, names(new_mldr$attributes)[idx]]),
                                                   collapse = ","), "}", sep = ""))

  new_mldr$labels <- label_measures(dataframe, labelIndices)
  new_mldr$labelsets <- if(nrow(dataframe) > 0)
                           sort(table(as.factor(do.call(paste, c(dataframe[, new_mldr$labels$index], sep = "")))))
                        else
                          array()
  new_mldr$dataset <- dataset_measures(new_mldr)
  new_mldr$measures <- measures(new_mldr)
  new_mldr$labels$SCUMBLE <- sapply(new_mldr$labels$index, function(idx)
    mean(new_mldr$dataset$.SCUMBLE[new_mldr$dataset[,idx] == 1]))

  class(new_mldr) <- "mldr"

  new_mldr
}
