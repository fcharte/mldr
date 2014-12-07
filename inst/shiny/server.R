shinyServer(function(input, output, session) {
  output$hist <- renderPlot(
    hist(emotions$dataset$.labelcount, breaks = input$n, col = "skyblue", border = "white")
  )
})
