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

mldr <- function(filename = NULL,
                 use_xml = FALSE,
                 auto_extension = TRUE,
                 xml_file = NULL) {

  # Creation of a prototypic multilabel dataset
  obj <- list(
    labels = data.frame(
      name = character(),
      index = integer(),
      IRLbl = numeric(),
      count = integer(),
      stringsAsFactors = FALSE
    ),
    dataset = data.frame()#,
    #features = data.frame(
    #  name = character(),
    #  index = integer(),
    #  type = character(),
    #  values = factor(), ########### ?
    #  stringsAsFactors = FALSE
    #)
  )

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
    obj$dataset <- contents$dataset
    rm(contents)

    if (use_xml) {
      # Read labels from XML file
      labelnames <- read_xml(xml_file)
      labeli <- which(names(attrs) %in% labelnames)

      #spl <- split(attrs, attrs$name %in% labelnames)
      #labels <- spl$`TRUE`
      #features <- spl$`FALSE`
    } else {
      # Read label amount from Meka parameters
      toplabel <- read_meka_header(relation)

      labeli <- 1:toplabel
      #features <- attrs[toplabel+1:length(attrs),]
    }

    # Convert labels to numeric
    obj$dataset[, labeli] <- lapply(obj$dataset[, labeli],
                                    function(col) as.numeric(!is.na(as.numeric(col) | NA)))

    obj$labels <- data.frame(name = names(attrs[labeli]),
                             index = labeli,
                             IRLbl = 0,
                             count = colSums(obj$dataset[labeli] == 1))

    # Change type from factor of strings to numeric
    obj$dataset[, which(attrs == "numeric")] <-
      lapply(obj$dataset[, which(attrs == "numeric")], as.numeric)

    # TODO
    # - Calculate measures and add them to labels
    # - Add dataset generic measures
  }

  class(obj) <- "mldr"
  return(obj)
}
