#' Generates graphic repreentations of an mldr object
#' @description Generates graphic representations of an \code{mldr} object
#' @param mldr The mldr object whose features are going to be drawn
#' @param type Indicates the type of plot to produce. Possible types are:
#'  "LC" for Label Co-ocurrence circular plot
#' @examples
#'
#' library(mldr)
#' plot(emotions, type = "LC")
#'
#' @import circlize
#' @export

plot.mldr <- function(mld, type = "LC")  {
  switch(type,
         LC = labelCoocurrencePlot(mld),
         HC = labelHistogram(mld)
         )
}

labelCoocurrencePlot <- function(mld) {
  labels <- mld$dataset[ , mld$labels$index]
  nlabels <- ncol(labels)

  # Prepare table with labels as columns and rows
  tbl <- as.data.frame(matrix(0, nrow = nlabels, ncol = nlabels))
  colnames(tbl) <- colnames(labels)
  row.names(tbl) <- colnames(tbl)


  for(ind1 in 1:(nlabels-1)) {
    for(ind2 in (ind1+1):nlabels) {
      tbl[ind1,ind2] <- sum(labels[,ind1]*labels[,ind2])
      tbl[ind2,ind1] <- 0
    }
  }

  chordDiagram(as.matrix(tbl), annotationTrack = "grid", transparency = 0.5,
               preAllocateTracks = list(track.height = 0.1))

}

labelHistogram <- function(mld) {

}
