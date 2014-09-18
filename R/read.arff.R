#'
#' Contains necessary functions to read ARFF files in
#' different formats (sparse, nonsparse...)
#'

#' Reads the attributes section of an ARFF file
#'
#' @param arff_file Name of the file to be read
#' @return A data.frame containing, for each
#'  attribute, its name and its type
read_attributes <- function(arff_file) {

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
#' @param arff_file Path to the ARFF file
#' @return Number of labels in the dataset
read_meka_header <- function(arff_file) {

}
read_nonsparse_data <- function(arff_file) {

}
read_sparse_data <- function(arff_file) {

}
