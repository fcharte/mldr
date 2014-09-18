#'
#' Contains necessary functions to read ARFF files in
#' different formats (sparse, nonsparse...)
#'

read_arff <- function(arff_file) {
  file_con <- file(arff_file, "r")
  on.exit(close(file_con))

  if (!isOpen(file_con))
    open(file_con, "r")

  file_data <- readLines(file_con) # Reads whole file

  # Split into relation, attributes and data
  relation_at <- pmatch("@relation", tt)
  data_start <- pmatch("@data", tt)

  if (is.na(relation_at)) stop("Missing @relation or not unique.")
  if (is.na(data_start)) stop("Missing @data mark or not unique.")

  relation <- file_data[relation_at]
  attributes <- file_data[relation_at + 1:data_start-1]
  dataset <- file_data[data_start:length(file_data)]

  return(list(
    relation = relation
    attributes = parse_attributes(attributes),
    data = if (detect_sparsity(dataset))
        parse_sparse_data(dataset)
      else
        parse_nonsparse_data(dataset)
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

  # Extract label names
  unname(unlist(label_list[names(label_list) == "label.name"]))
}

#' Reads the Meka parameters in the header of an
#' ARFF file
#'
#' @param arff_relation "@relation" line of the ARFF file
#' @return Number of labels in the dataset
read_meka_header <- function(arff_relation) {
  rgx <- regexpr("-C\\s*(\\d+)", arff_relation, perl = T)
  as.integer(strsplit(regmatches(arff_relation, rgx), "-C\\s*")[[1]][2])
}

detect_sparsity <- function(arff_data) {

}
parse_nonsparse_data <- function(arff_data) {

}
parse_sparse_data <- function(arff_data) {

}
