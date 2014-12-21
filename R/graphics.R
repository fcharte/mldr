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
  tbl <- sapply(1:nlabels, function(ind1)
    sapply(1:nlabels, function(ind2)
      if(ind2 < ind1) sum(labels[,ind1]*labels[,ind2]) else 0
    ))
  colnames(tbl) <- colnames(labels)
  row.names(tbl) <- colnames(tbl)

  circos.par(gap.degree = 3)
  chordDiagram(tbl, annotationTrack = "grid", transparency = 0.5,
               preAllocateTracks = list(track.height = 0.1))
  for(si in get.all.sector.index()) {
    circos.axis(h = "top", labels.cex = 0.4, sector.index = si, track.index = 2)
  }
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name <- get.cell.meta.data("sector.index")
    name.length <- nchar(sector.name)
    first.part <- if(name.length <= 3) name.length else 3
    second.part <- name.length - 3
    title <- paste(substr(sector.name, 1, first.part), substr(sector.name, second.part, name.length), sep = "-")

    circos.lines(xlim, c(mean(ylim), mean(ylim)), lty = 3)
    for(p in seq(0, 1, by = 0.25)) {
      circos.text(p*(xlim[2] - xlim[1]) + xlim[1], mean(ylim), p, cex = 0.5, adj = c(0.5, -0.2), niceFacing = TRUE)
    }

    circos.text(mean(xlim), 1.1, title, cex = 0.6, niceFacing = TRUE)
  }, bg.border = NA)
  circos.clear()

}

labelHistogram <- function(mld) {

}
