#' Generates graphic representations of an mldr object
#' @description Generates graphic representations of an \code{mldr} object
#' @param mldr The mldr object whose features are going to be drawn
#' @param type Indicates the type of plot to produce. Possible types are:\itemize{
#'  \item \code{"LC"} Draws a circular plot with sectors representing each label
#' and links between them depicting label co-occurrences
#'  \item \code{"LH"} for label histogram
#'  \item \code{"CH"} for cardinality histogram
#'  \item \code{"AT"} for attributes by type pie chart
#'  }
#' @param title A title to show above the plot. Defaults to the name of the dataset passed as first argument
#' @param labelCount Samples the labels in the dataset to show information of only \code{labelCount} of them
#' @param labelIndices Establishes the labels to show in the plot
#' @examples
#'
#' library(mldr)
#'
#' # Label concurrence plot
#' plot(genbase, type = "LC") # Plots all labels
#' plot(genbase) # Same that above
#' plot(genbase, title = "genbase dataset") # Changes the title
#' plot(genbase, labelCount = 10) # Randomly selects 10 labels to plot
#' plot(genbase, labelIndices = genbase$label$index[1:10]) # Plots info of first 10 labels
#'
#' # Label histogram plot
#' plot(emotions, type = "LH")
#'
#' # Cardinality histogram plot
#' plot(emotions, type = "CH")
#' @import circlize
#' @export

plot.mldr <- function(mld, type = "LC", labelCount, labelIndices, title = NULL, ...)  {
  if(missing(title))
    title <- substitute(mld)

  if(missing(labelIndices)) {
    labelIndices <- if(!missing(labelCount))
      sample(mld$labels$index, labelCount)
    else
      mld$labels$index
  }

  switch(type,
         LC = labelCoocurrencePlot(mld, title, labelIndices, ...),
         LH = labelHistogram(mld, title, labelIndices, ...),
         CH = cardinalityHistogram(mld, title, ...),
         AT = attributeByType(mld, title, ...)
  )
}

#' @import circlize
labelCoocurrencePlot <- function(mld, title, labelIndices, ...) {

  labelIndices <- labelIndices[labelIndices %in% mld$labels$index]
  if(length(labelIndices) == 0) return()

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

    circos.text(mean(xlim), 0, xr, cex = 1, niceFacing = TRUE)
    circos.text(mean(xlim), 0.8, sector.name, cex = 1, niceFacing = TRUE, facing = "clockwise")
  }, bg.border = NA)
  text(0, 1, title, cex = 1.3, pos = 3)
  text(0, -1, paste("Scumble =", mld$measures$scumble), pos = 1, cex = 0.8)
  circos.clear()

}

labelHistogram <- function(mld, title, labelIndices, ...) {
  labels <- mld$labels[mld$labels$index %in% labelIndices, ]

  end_point = 0.5 + nrow(labels) + nrow(labels)-1
  interval = round(max(labels$count) / 25)
  barplot(labels$count, axes=FALSE,
          ylab = "Number of samples",
          col = rainbow(length(labels$count)),
          space = 1, ...)
  axis(2, at = seq(0, max(labels$count), interval), las = 2, cex = 1.25)
  title(main = title, sub = "Instances per label")
  text(seq(1.5, end_point, by=2), par("usr")[3]-0.25,
       srt = 60, adj= 1, xpd = TRUE,
       labels = paste(rownames(labels)), cex=1)
}


cardinalityHistogram <- function(mld, title, ...) {
  hist(mld$dataset$.labelcount,
       breaks = max(mld$dataset$.labelcount),
       col = 'blue', border = 'white',
       main = paste(title, "- Labels per instance histogram"),
       xlab = "Number of labels per instance",
       ylab = "Number of instances", ...)
}


attributeByType <- function(mld, title, ...) {
  data <- rbind(as.data.frame(table(sapply(mld$dataset[ , mld$attributesIndexes], class))),
                data.frame(Var1 = "label", Freq = mld$measures$num.labels))

  pie(data$Freq, labels = paste(data$Var1, data$Freq, sep = "\n"),
      main = title, sub = "Type and number of attributes", col = heat.colors(5), ...)
}
