library(shiny)
library(mldr)

shinyServer(function(input, output, session) {
  observe({
    availableMLDs <- as.list(ls(.GlobalEnv)[unlist(sapply(ls(.GlobalEnv), function(obj) class(get(obj)) == "mldr"))])

    updateSelectInput(session, "mldrs",
                      choices = availableMLDs,
                      selected = availableMLDs[[1]])
  })

  summaryTable <- reactive({
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
