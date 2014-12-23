library(shiny)

shinyUI(
  navbarPage("mldr: EDA for multilabel datasets",
    tabPanel("Main", fluidPage(
      titlePanel("Basic information"),
      sidebarLayout(
        sidebarPanel(
          selectInput("mldrs", "Select a dataset", c()),
          hr(),
          h4("Load a dataset"),
          fileInput('arffname', 'Select the ARFF file'),
          fileInput('xmlname', 'Select the XML file'),
          actionButton("loadButton", "Load dataset")
        ),
        mainPanel(tableOutput("summary"))
      )
    )),

    tabPanel("Labels", fluidPage(
      titlePanel("Label information"),
      mainPanel(tableOutput("labels"))
    ))
  )
)
