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
})
