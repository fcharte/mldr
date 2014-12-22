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

plot.mldr <- function(mld, type = "LC", title = NULL, ...)  {
  if(missing(title))
    title <- substitute(mld)

  switch(type,
         LC = labelCoocurrencePlot(mld, title, ...),
         HC = labelHistogram(mld, ...)
         )
}

#' Plots a graphic with co-occurrence ratio between pairs of labels
#' @description Draws a circular plot with sectors representing each label
#' and links between them depicting label co-occurrences
#' @param mld An \code{mldr} object with the information to plot
#' @param title A title to show above the plot. Defaults to the name of the dataset passed as first argument
#' @param labelCount Samples the labels in the dataset to show information of only \code{labelCount} of them
#' @param labelIndices Establishes the labels to show in the plot
#' @return The circular plot
#' @seealso \code{\link{plot.mldr}}
#' @examples
#'
#' library(mldr)
#' yeast <- mldr("yeast") # Read "yeast.arff" and labels from "yeast.xml"
#' labelCoocurrencePlot(yeast) # Plots all labels
#' plot(yeast, type = "LC") # Same that above
#' plot(yeast) # Same that above
#' plot(yeast, title = "Yeast dataset") # Changes the title
#' plot(yeast, labelCount = 10) # Randomly selects 10 labels to plot
#' plot(yeast, labelIndices = yeast$label$index[1:10]) # Plots info of first 10 labels
#'
#' @import circlize
#' @export

labelCoocurrencePlot <- function(mld, title, labelCount, labelIndices) {

  if(missing(labelIndices)) {
    labelIndices <- if(!missing(labelCount))
      sample(mld$labels$index, labelCount)
    else
      mld$labels$index
  }

  labels <- mld$dataset[ , labelIndices]
  nlabels <- ncol(labels)

  # Prepare table with labels as columns and rows
  tbl <- sapply(1:nlabels, function(ind1)
    sapply(1:nlabels, function(ind2)
      if(ind2 < ind1) sum(labels[,ind1]*labels[,ind2]) else 0
    ))
  colnames(tbl) <- colnames(labels)
  row.names(tbl) <- colnames(tbl)

  tbl <- tbl[apply(tbl, 1, function(r) !all(r == 0)), ]
  tbl <- tbl[,apply(tbl, 2, function(r) !all(r == 0))]

  color.sector <- rainbow(length(union(colnames(tbl), row.names(tbl))))
  color.links <- rainbow(nrow(tbl) * ncol(tbl))
  circos.par(gap.degree = 1)
  chordDiagram(tbl, annotationTrack = "grid", transparency = 0.5,
               preAllocateTracks = list(track.height = 0.2),
               grid.col = color.sector, col = color.links)
  for(si in get.all.sector.index()) {
    circos.axis(h = "top", labels.cex = 0.4, sector.index = si,
                track.index = 2, direction = "inside", labels = FALSE,
                major.tick.percentage = 0.25)
  }
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    xr <- get.cell.meta.data("xrange")
    sector.name <- get.cell.meta.data("sector.index")
    name.length <- nchar(sector.name)

    sector.name <- if(name.length <= 7)
      sector.name
    else
     paste(substr(sector.name, 1, 3), substr(sector.name, name.length - 3, name.length), sep = "-")

    circos.lines(xlim, c(mean(ylim) - 0.4, mean(ylim) - 0.4), lty = 3)
    if(xr > 15)
      for(p in seq(0, 1, by = 0.25)) {
        circos.text(p*(xlim[2] - xlim[1]) + xlim[1],
                    mean(ylim) - 0.4, p, cex = 0.5, adj = c(0.5, -0.2), niceFacing = TRUE)
      }

    circos.text(mean(xlim), 0, xr, cex = 0.7, niceFacing = TRUE)
    circos.text(mean(xlim), 0.8, sector.name, cex = 0.7, niceFacing = TRUE, facing = "clockwise")
  }, bg.border = NA)
  text(0, 1, title, cex = 1.3, pos = 3)
  text(0, -1, paste("Scumble =", mld$measures$scumble), pos = 1, cex = 0.8)
  circos.clear()

}

labelHistogram <- function(mld) {

}
