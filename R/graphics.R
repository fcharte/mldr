#' Generates graphic representations of an mldr object
#' @description Generates graphic representations of an \code{mldr} object
#' @param x The mldr object whose features are to be drawn
#' @param type Indicates the type of plot to be produced. Possible types are:\itemize{
#'  \item \code{"LC"} Draws a circular plot with sectors representing each label
#' and links between them depicting label co-occurrences
#'  \item \code{"LH"} for label histogram
#'  \item \code{"LB"} for label bar plot
#'  \item \code{"CH"} for cardinality histogram
#'  \item \code{"AT"} for attributes by type pie chart
#'  \item \code{"LSH"} for labelset histogram
#'  \item \code{"LSB"} for labelset bar plot
#'  }
#' @param title A title to be shown above the plot. Defaults to the name of the dataset passed as first argument
#' @param labelCount Samples the labels in the dataset to show information of only \code{labelCount} of them
#' @param labelIndices Establishes the labels to be shown in the plot
#' @param ... Additional parameters to be given to barplot, hist, etc.
#' @examples
#'
#' library(mldr)
#'\dontrun{
#' # Label concurrence plot
#' plot(genbase, type = "LC") # Plots all labels
#' plot(genbase) # Same as above
#' plot(genbase, title = "genbase dataset") # Changes the title
#' plot(genbase, labelCount = 10) # Randomly selects 10 labels to plot
#' plot(genbase, labelIndices = genbase$label$index[1:10]) # Plots info of first 10 labels
#'
#' # Label histogram plot
#' plot(emotions, type = "LH")
#'
#' # Cardinality histogram plot
#' plot(emotions, type = "CH")
#'
#' # Attributes by type
#' plot(emotions, type = "AT", cex = 0.85)
#'
#' # Labelset histogram
#' plot(emotions, type = "LSH")
#' }
#' @import circlize
#' @export
plot.mldr <- function(x, type = "LC", labelCount, labelIndices, title = NULL, ...)  {
  if(x$measures$num.instances == 0) return()

  if(missing(title))
    title <- substitute(x)

  if(missing(labelIndices)) {
    labelIndices <- if(!missing(labelCount))
      sample(x$labels$index, labelCount)
    else
      x$labels$index
  }

  switch(type,
         LC = labelCoocurrencePlot(x, title, labelIndices, ...),
         LH = labelHistogram(x, title, ...),
         LB = labelBarPlot(x, title, labelIndices, ...),
         CH = cardinalityHistogram(x, title, ...),
         AT = attributeByType(x, title, ...),
         LSH = labelsetHistogram(x, title, ...),
         LSB = labelsetBarPlot(x, title, ...)
  )
}

# Generates a circular label concurrence plot
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

  if(length(which(tbl != 0)) == 0) return() # There's nothing to plot

  tbl <- tbl[apply(tbl, 1, function(r) !all(r == 0)), ]
  tbl <- tbl[,apply(tbl, 2, function(r) !all(r == 0))]

  if(class(tbl) != "matrix") return() # Nothing to plot

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

# Generates a barplot with label counters
labelBarPlot <- function(mld, title, labelIndices, ...) {
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

# Generates a histogram with label counters
labelHistogram <- function(mld, title, ...) {
  hist(mld$labels$count,
       breaks = length(mld$labels$count) / 3,
       col = 'blue', border = 'white',
       main = paste(title, "- Labels histogram"),
       xlab = "Number of instances",
       ylab = "Number of labels", ...)
}

# Generates a histogram with cardinality information
cardinalityHistogram <- function(mld, title, ...) {
  hist(mld$dataset$.labelcount,
       breaks = max(mld$dataset$.labelcount),
       col = 'blue', border = 'white',
       main = paste(title, "- Labels per instance histogram"),
       xlab = "Number of labels per instance",
       ylab = "Number of instances", ...)
}

# Generates a pie chart with attribute types distribucion
attributeByType <- function(mld, title, ...) {
  data <- rbind(as.data.frame(table(sapply(mld$dataset[ , mld$attributesIndexes], class))),
                data.frame(Var1 = "label", Freq = mld$measures$num.labels))

  pie(data$Freq, labels = paste(data$Var1, data$Freq, sep = "\n"),
      main = title, sub = "Type and number of attributes", col = heat.colors(5), ...)
}

# Generates a barplot with labelset counters
labelsetBarPlot <- function(mld, title, ...) {
  labelsets <- mld$labelsets
  nls <- length(labelsets)

  if(nls > 50) {
    labelsets <- c(labelsets[nls:(nls-50)], others = sum(labelsets[1:(nls-49)]))
  }

  end_point = 0.5 + length(labelsets) + length(labelsets)-1
  interval = round(max(labelsets) / 25)
  barplot(labelsets, axes = FALSE,
          ylab = "Number of samples",
          col = rainbow(length(labelsets)),
          space = 1, axisnames = FALSE, ...)
  axis(2, at = seq(0, max(labelsets), interval), las = 2, cex = 1.25)
  title(main = paste(title, "Instances per labelset", sep = "\n"))
  text(seq(1.5, end_point, by=2), par("usr")[3]-0.25,
       srt = 60, adj= 1, xpd = TRUE,
       labels = names(labelsets), cex = 0.5)
}

# Generates a histogram with labelset counters
labelsetHistogram <- function(mld, title, ...) {
  hist(mld$labelsets,
       col = 'blue', border = 'white',
       main = paste(title, "- Labelsets histogram"),
       xlab = "Number of instances",
       ylab = "Number of labelsets", ...)
}
