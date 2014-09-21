#'
#' Contains necessary functions to read ARFF files in
#' different formats (sparse, nonsparse...)
#'

#' Extracts all useful data from an ARFF file in
#' R objects
#'
#' @param arff_file Path to the file
#' @return List containing the @relation string,
#'  a named vector for attributes and a data.frame
#'  for the data section
read_arff <- function(arff_file) {
  file_con <- file(arff_file, "r")

  if (!isOpen(file_con))
    open(file_con, "r")

  file_data <- readLines(file_con) # Reads whole file

  print("Archivo cargado")

  close(file_con)

  # Split into relation, attributes and data
  relation_at <- pmatch("@relation", file_data)
  data_start <- pmatch("@data", file_data)

  if (is.na(relation_at)) stop("Missing @relation or not unique.")
  if (is.na(data_start)) stop("Missing @data mark or not unique.")

  relation <- file_data[relation_at]

  # Get attribute vector
  attributes <- parse_attributes(file_data[relation_at + 1:data_start - 1])
  num_attrs <- length(attributes)

  # Ignore blank lines before data
  data_start <- data_start + 1
  while (grepl("^\\s*$", file_data[data_start]))
    data_start <- data_start + 1

  # Build data.frame with @data section
  rawdata <- file_data[data_start:length(file_data)]
  dataset <- if (detect_sparsity(rawdata))
      parse_sparse_data(rawdata, num_attrs)
    else
      parse_nonsparse_data(rawdata, num_attrs)

  rm(rawdata)
  names(dataset) <- names(attributes)

  return(list(
    relation = relation,
    attributes = attributes,
    dataset = dataset
    )
  )
}

#' Reads the attributes section of an ARFF file
#'
#' @param arff_attrs Lines containing the attributes
#' @return A vector containing, for each
#'  attribute, its name and its type
parse_attributes <- function(arff_attrs) {
  # Extract attribute definitions
  att_list <- strsplit(arff_attrs, "\\b\\s+", perl=T)

  # Structure by rows
  att_mat <- matrix(unlist(att_list[lapply(att_list, length) == 3]),
                    ncol = 3, byrow=T)
  rm(att_list)

  # Filter any data that is not an attribute
  att_mat <- att_mat[grepl("\\s*@attribute", att_mat[, 1]), 2:3]

  # Create the named vector
  att_v <- att_mat[, 2]
  names(att_v) <- att_mat[, 1]

  rm(att_mat)
  return(att_v)
}

#' Reads the associated XML file for a ML dataset
#'
#' @param xml_file Path to the XMl file
#' @return A vector of strings containing the name
#'  of each label
read_xml <- function(xml_file) {
  library(XML)
  parsed_xml <- xmlParse(xml_file)
  label_list <- xmlToList(parsed_xml, simplify = T)
  rm(parsed_xml)

  # Extract label names
  unname(unlist(label_list[names(label_list) == "label.name"]))
}

#' Reads the Meka parameters in the header of an
#' ARFF file
#'
#' @param arff_relation "@relation" line of the ARFF file
#' @return Number of labels in the dataset
read_meka_header <- function(arff_relation) {
  rgx <- regexpr("-C\\s*\\d+", arff_relation, perl = T)
  as.integer(strsplit(regmatches(arff_relation, rgx), "-C\\s*")[[1]][2])
}

#' Detects whether an ARFF file is in sparse format
#'
#' @param arff_data Content of the @data section
#' @return Boolean, TRUE when the file is sparse
detect_sparsity <- function(arff_data) {
  grepl("^\\s*\\{", arff_data[1])
}

#' Builds a data.frame out of non-sparse ARFF data
#'
#' @param arff_data Content of the @data section
#' @return data.frame containing data values
parse_nonsparse_data <- function(arff_data, num_attrs) {
  data.frame(matrix(
    unlist(strsplit(arff_data, ",", fixed = T)),
    ncol = num_attrs,
    byrow = T
  ))
}

#' Builds a data.frame out of sparse ARFF data
#'
#' @param arff_data Content of the @data section
#' @return data.frame containing data values
parse_sparse_data <- function(arff_data, num_attrs) {
  # Extract data items
  arff_data <- strsplit(gsub("[\\{\\}]", "", arff_data), ",")
  arff_data <- lapply(arff_data, function(item) {
    strsplit(item, " ")
  })

  # Convert data into a list of matrices (with pairs)
  #arff_data <- sapply(arff_data, function(row) {
  #  matrix(unlist(row)[c(F, T)], ncol=1, byrow=T,
  #         dimnames = list(unlist(row)[c(T, F)]))
  #})

  #arff_data <- sapply(arff_data, function(row) { sapply(row, unlist) })

  arff_data <- sapply(arff_data, unlist)

  print(length(arff_data))

  #filler <- function(dat, cols) {
  #  sapply(dat, function(row) {
  #    # Memory black hole here !!
  #    indexes <- c(as.integer(row[, 1]))
  #    matrix(indexes = row[, 2], ncol = cols, nrow = 1)
  #  })
  #}
  #print(arff_data[[1]])

  #extractor <- function(i,j) {
  #  arff_data[i][[1]][,1][as.character(j)]
  #}

  #combos <- expand.grid(i = 1:length(arff_data), j = 1:num_attrs)

  #dataset <- matrix((function(i,j) {
  #    arff_data[i][[1]][,1][as.character(j)]
  #  })(combos$i, combos$j), ncol = num_attrs, byrow = T)

  #print(dataset)

  # Build complete matrix with data
  arff_data <- sapply(arff_data, function(row) {
    # Memory black hole here !!
    complete <- NA[1:num_attrs]
    complete[as.integer(row[c(T, F)])] <- row[c(F, T)]
    complete
  })

  #rm(arff_data)

  # Create and return data.frame
  data.frame(t(arff_data))
}
