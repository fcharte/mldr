library(shiny)
library(mldr)

shinyServer(function(input, output, session) {
  selected <- NULL

  observe({
    if(input$loadButton != 0) {
      isolate({
        arfffile <- input$arffname
        xmlfile <- input$xmlname
        if(!is.null(arfffile) && !is.null(xmlfile)) {
          .GlobalEnv[[arfffile$name]] <-
            mldr(arfffile$datapath, auto_extension = FALSE, xml_file = xmlfile$datapath)

          selected <- arfffile$name
        }
      })
    }

    availableMLDs <- as.list(ls(.GlobalEnv)[unlist(sapply(ls(.GlobalEnv), function(obj) class(get(obj)) == "mldr"))])

    if(is.null(selected)) selected <- availableMLDs[[1]]
    updateSelectInput(session, "mldrs",
                      choices = availableMLDs,
                      selected = selected)
  })

  summaryTable <- reactive({
    input$loadButton

    if(!is.null(input$mldrs) && input$mldrs != "") {
      mld <- get(input$mldrs)
      table <- data.frame(Measure = names(mld$measures), Value = unlist(mld$measures))
      table
    }
  })
  output$summary <- renderTable(summaryTable(), include.rownames = FALSE, digits = 4)

  labelsTable <- reactive({
    if(!is.null(input$mldrs) && input$mldrs != "") {
      mld <- get(input$mldrs)
      mld$labels
    }
  })
  output$labels <- renderTable(labelsTable())

  attributesTable <- reactive({
    if(!is.null(input$mldrs) && input$mldrs != "") {
      mld <- get(input$mldrs)
      tbl <- as.matrix(mld$attributes[-mld$labels$index])
      dimnames(tbl)[[2]] <- "Type"
      tbl
    }
  })
  output$attributes <- renderTable(attributesTable())
})
