library(shiny)

shinyUI(
  navbarPage("mldr: EDA for multilabel datasets",
    tabPanel("Main", fluidPage(
      titlePanel("Basic information"),
      #sidebarLayout(
        sidebarPanel(selectInput("mldrs", "Select a dataset", c())),
        mainPanel(tableOutput("summary"))
    )),

    tabPanel("Labels", fluidPage(
      titlePanel("Label information"),
      mainPanel(tableOutput("labels"))
    ))
  )
)
