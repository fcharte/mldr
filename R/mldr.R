#' Creates an object representing a multilabel
#' dataset
#'
#' @param filename Name of the dataset
#' @param auto.extension Specifies whether to add
#'  the '.arff' and '.xml' extensions to the filename
#'  where appropriate
#' @param use.xml Specifies whether to use an
#'  associated XML file to identify the labels.
#' @param xml.file Path to the XML file. If not
#'  provided, the filename ending in ".xml" will be
#'  assumed

mldr <- function(
  filename = NULL,
  auto_extension = TRUE,
  use_xml = FALSE,
  xml_file = NULL
  ) {

  # Creation of a prototypic multilabel dataset
  obj <- list(
    labels = data.frame(
      name = character(),
      index = integer(),
      IRLbl = numeric(),
      count = integer(),
      stringsAsFactors = FALSE
    ),
    dataset = data.frame(),
    features = data.frame(
      name = character(),
      index = integer(),
      type = character(),
      values = factor(), ########### ?
      stringsAsFactors = FALSE
    )
  )

  if (!is.null(filename)) {
    # Calculate names of files
    arff_file <- if (auto_extension)
        paste(filename, ".arff", sep="")
      else
        filename

    xml_file <- if (auto_extension && is.null(xml_file))
        paste(filename, ".xml", sep="")
      else {
        noext <- unlist(strsplit(filename, ".", fixed=TRUE))
        paste(noext[1:length(noext)], ".xml", sep="")
      }

    # Get attributes from file
    attrs <- read_attributes(arff_file)

    if (use_xml) {
      # Read labels from XML file
      labelnames <- read_xml(xml_file)

      spl <- split(attrs, attrs$name %in% labelnames)
      labels <- spl$`TRUE`
      features <- spl$`FALSE`
    } else {
      # Read label amount from Meka parameters
      toplabel <- read_meka_header(arff_file) - 1

      labels <- attrs[0:toplabel,]
      features <- attrs[toplabel+1:length(attrs),]
    }

    # Read data section of dataset
    dataset <- if (detect_sparsity(arff_file))
        read_sparse_data(arff_file)
      else
        read_nonsparse_data(arff_file)

    obj <- list(labels=labels, dataset=dataset, features=features)
  }

  class(obj) <- "mldr"
  return(obj)
}
