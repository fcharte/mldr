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
      titlePanel("Labels information"),
      fluidRow(
        column(5, wellPanel(dataTableOutput("labels"))),
        column(7, wellPanel(
          uiOutput("labelRange"),
          plotOutput("labelHC",height="auto"))
               )
      )
    )),

    tabPanel("Labelsets", fluidPage(
      titlePanel("Labelsets information"),
      mainPanel(dataTableOutput("labelsets"))
    )),

    tabPanel("Attributes", fluidPage(
      titlePanel("Attributes information"),
      mainPanel(dataTableOutput("attributes"))
    ))
  )
)
