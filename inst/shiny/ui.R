shinyUI(navbarPage("mldr: EDA for multilabel datasets",
                   tabPanel("Main", fluidPage(
                     sidebarLayout(
                       sidebarPanel(sliderInput("n", "Bins", 5, 100, 20)),
                       mainPanel(plotOutput("hist"))
                     )
                   )),
                   tabPanel("Labels", fluidPage(
                     ))
))
