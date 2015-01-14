library(shiny)
library(mldr)

shinyServer(function(input, output, session) {
  selected <- NULL  # mldr selected by the user in the drop-down list

  observe({
    if(input$loadButton != 0) {
      isolate({
        arfffile <- input$arffname
        xmlfile <- input$xmlname
        if(!is.null(arfffile) && !is.null(xmlfile)) {
          # Load the dataset in the global environment
          .GlobalEnv[[arfffile$name]] <-
            mldr(arfffile$datapath, auto_extension = FALSE, xml_file = xmlfile$datapath)

          selected <- arfffile$name
        }
      })
    }

    # Make sample datasets in package mldr available in global environment
    for(obj in ls("package:mldr"))
      if(class(get(obj, "package:mldr")) == "mldr")
        assign(obj, get(obj, "package:mldr"), .GlobalEnv)

    # Get available mldr objects in the global environment
    availableMLDs <- as.list(
      ls(.GlobalEnv)[unlist(sapply(ls(.GlobalEnv),
                                   function(obj) class(get(obj)) == "mldr"))
                     ]
      )

    if(is.null(selected)) selected <- availableMLDs[[1]]
    updateSelectInput(session, "mldrs",
                      choices = availableMLDs,
                      selected = selected)
  })

  # Table with summary information about the mldr
  summaryTable <- reactive({
    input$loadButton

    if(!is.null(input$mldrs) && input$mldrs != "") {
      mld <- get(input$mldrs)
      table <- data.frame(Measure = names(mld$measures), Value = unlist(mld$measures))
      table
    }
  })
  output$summary <- renderTable(summaryTable(), include.rownames = FALSE, digits = 4)

  # Table with data about the labels in the mldr
  labelsTable <- reactive({
    if(!is.null(input$mldrs) && input$mldrs != "") {
      mld <- get(input$mldrs)
      tbl <-  cbind(Label = rownames(mld$labels), mld$labels)
      tbl
    }
  })
  output$labels <- renderDataTable(labelsTable())

  labelsNum <- reactive({
    if(!is.null(input$mldrs) && input$mldrs != "") {
      mld <- get(input$mldrs)
      mld$measures$num.labels
    }
  })
  output$labelRange <- renderUI({
    if(!is.null(input$mldrs) && input$mldrs != "") {
      sliderInput("labelRange", label = h5("Choose range of labels to plot"),
                min = 1, max = labelsNum(), step = 1,
                value = c(1, if(labelsNum() < 25) labelsNum() else 25),
                width = "100%")
    }
  })

  labelHC <- reactive({
    if(!is.null(input$mldrs) && input$mldrs != "" && !is.null(input$labelRange)) {
      mld <- get(input$mldrs)
      labelRange <- input$labelRange
      plot(mld, title = mld$name, type = "LH",
           labelIndices = (labelRange[1] + mld$labels$index[1] - 1):(labelRange[2] + mld$labels$index[1] -1))
    }
  })
  output$labelHC <- renderPlot(labelHC(), height = 800, width = 1024)

  # Table with data about labelsets in the mldr
  labelsetsTable <- reactive({
    if(!is.null(input$mldrs) && input$mldrs != "") {
      mld <- get(input$mldrs)
      data.frame(LabelSet = names(mld$labelsets), Count = mld$labelsets)
    }
  })
  output$labelsets <- renderDataTable(labelsetsTable())

  # Table with data about the attributes in the mldr
  attributesTable <- reactive({
    if(!is.null(input$mldrs) && input$mldrs != "") {
      mld <- get(input$mldrs)
      tbl <- mld$attributes[-mld$labels$index]
      sum <- lapply(names(mld$dataset[-c(mld$labels$index,(length(mld$dataset)-1):length(mld$dataset))]),
                    function(column.name) {
                      tmpsum <- if(mld$attributes[column.name] == 'numeric')
                        summary(mld$dataset[,column.name])
                      else
                        summary(as.factor(mld$dataset[,column.name]))
                      paste('<table><tr><td><b>',
                            paste(names(tmpsum), collapse = '</b></td><td><b>'),
                            '</td></tr><tr><td>',
                            paste(tmpsum, collapse = '</td><td>'),
                            '</td></tr></table>')
                    })

      tbl <- data.frame(Attribute = names(tbl), Type = tbl, Summary = unlist(sum))
      tbl
    }
  })
  output$attributes <- renderDataTable(attributesTable())

  # Table with data about the labels in the mldr
  concurrenceTable <- reactive({
    if(!is.null(input$mldrs) && input$mldrs != "") {
      mld <- get(input$mldrs)
      tbl <- data.frame(Index = mld$labels$index,
                        Label = rownames(mld$labels),
                        Count = mld$labels$count,
                        SCUMBLE = mld$labels$SCUMBLE)
      tbl <- tbl[order(tbl$Count, tbl$SCUMBLE),]

      tbl
    }
  })

  highScumbleLabels <- reactive({
    if(!is.null(input$mldrs) && input$mldrs != "") {
      mld <- get(input$mldrs)
      tbl <- data.frame(Label = rownames(mld$labels),
                        Count = mld$labels$count,
                        SCUMBLE = mld$labels$SCUMBLE)
      tbl <- tbl[order(tbl$Count, tbl$SCUMBLE),]

      ScumbleList <- sort(tbl$SCUMBLE, decreasing = TRUE)
      ScumbleList <- ScumbleList[1:10]
      ScumbleList <- ScumbleList[!is.na(ScumbleList)]
      ScumbleList <- paste(which(tbl$SCUMBLE %in% ScumbleList) - 1, collapse = ",")

      paste("function(settings, json) {
            $('.dataTable').DataTable().rows([", ScumbleList, "]).nodes().to$().addClass('selected');
            Shiny.onInputChange('labels', $('.dataTable').DataTable().rows('.selected').indexes().toArray());
            }")
    }
  })
  tblConcurrenceOptions <- reactive({
    list(paging = FALSE, searching = FALSE,
         ordering = FALSE, info = FALSE,
         initComplete = I(highScumbleLabels()))
  })

  output$tblConcurrence <- renderDataTable(
    concurrenceTable()[,-1],
    options = tblConcurrenceOptions,
    callback = "function(table) {
      table.on('click.dt', 'tr', function() {
        $(this).toggleClass('selected');
        Shiny.onInputChange('labels',
          table.rows('.selected').indexes().toArray());
      });
    }")

  labelLC <- reactive({
    if(!is.null(input$mldrs) && input$mldrs != "" && !is.null(input$labels)) {
      mld <- get(input$mldrs)
      labels <- concurrenceTable()[input$labels+1,1]
      plot(mld, title = mld$name, labelIndices = labels)
    }
  })
  output$labelLC <- renderPlot(labelLC(), height = 800, width = 800)

  output$selectedLabels <- renderText({
    paste(c('You selected these labels:', input$labels),
          collapse = ' ')
  })

  observe({
    if (is.null(input$pages) || input$pages != "finish")
      return()

    stopApp(0)
  })
})
