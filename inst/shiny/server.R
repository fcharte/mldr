library(shiny)
library(mldr)

shinyServer(function(input, output, session) {
  observe({
    if(input$loadButton != 0) {
      arfffile <- input$arffname
      xmlfile <- input$xmlname
      if(!is.null(arfffile) && !is.null(xmlfile)) {
        .GlobalEnv[[arfffile$name]] <- mldr(arfffile$datapath, auto_extension = FALSE, xml_file = xmlfile$datapath)
        updateSelectInput(session, "mldrs",
                          choices = availableMLDs,
                          selected = arfffile$name)
      }
    }

    availableMLDs <- as.list(ls(.GlobalEnv)[unlist(sapply(ls(.GlobalEnv), function(obj) class(get(obj)) == "mldr"))])

    updateSelectInput(session, "mldrs",
                      choices = availableMLDs,
                      selected = availableMLDs[[1]])
  })

  loader <- reactive({
    print(input$loadButton)
    if(input$loadButton != 0) {
      print("loadButton")
      arfffile <- input$arffname
      xmlfile <- input$xmlname
      if(!is.null(arffile) && !is.null(xmlfile)) {
        print(paste(arfffile$datapath, xmlfile$datapath))
      }
    }
  })

  summaryTable <- reactive({
    input$loadButton

    if(!is.null(input$mldrs) && input$mldrs != "") {
      mld <- get(input$mldrs)
      table <- data.frame(Measure = names(mld$measures), Value = unlist(mld$measures))
      table
    }
  })
  output$summary <- renderTable(summaryTable(), include.rownames = FALSE)

  labelsTable <- reactive({
    if(!is.null(input$mldrs) && input$mldrs != "") {
      mld <- get(input$mldrs)
      mld$labels
    }
  })
  output$labels <- renderTable(labelsTable())
})
