library(shiny)

shinyUI(
  fluidPage(
    tags$head(
      tags$style(
        HTML(".tab-pane {
             border: 2px solid black;
             background: #FAF8D9 !important;
             box-shadow: 10px 10px 5px #888888;
             margin-bottom: 3em;")
      )),
    tagList(
      singleton(tags$head(tags$script(src='//cdn.datatables.net/1.10.2/js/jquery.dataTables.min.js',type='text/javascript'))),
      singleton(tags$head(tags$script(src='//cdn.datatables.net/tabletools/2.2.2/js/dataTables.tableTools.min.js',type='text/javascript'))),
      singleton(tags$head(tags$link(href='//cdn.datatables.net/tabletools/2.2.2/css/dataTables.tableTools.css',rel='stylesheet',type='text/css')))
    ),
    titlePanel("mldrGUI - EDA for multilabel datasets"),
    h4(textOutput("selectedMLD")),
    hr(),
    mainPanel(
      tabsetPanel(id = "pages", type = "pills", selected = "Main",
                  tabPanel(HTML("<b>EXIT</b>"), value = "finish"),
                  tabPanel("Main", fluidPage(
                    titlePanel("Basic information"),
                    fluidRow(
                      column(4,
                             wellPanel(
                               h3('Active MLD'),
                               selectInput("mldrs", "Select a dataset", c()),
                               hr(),
                               h4("Load a dataset"),
                               fileInput('arffname', 'Select the ARFF file'),
                               fileInput('xmlname', 'Select the XML file'),
                               actionButton("loadButton", "Load dataset")
                             ),
                             wellPanel(
                               h3("How to use mldrGUI"),
                               tags$small(paste0(
                                 "mldrGUI is an EDA tool for multilabel datasets (MLDs).")), br(),
                               tags$small(paste0("- Use the controls above to select one of the MLDs included in the package, ",
                                                 "or select an .arff and .xml file in your system to load any MLD.")), br(),
                               tags$small(paste0("- Once the MLD has been loaded, you will see its basic traits in this page.")), br(),
                               tags$small(paste0("- Use the tabs at the top of the page to explore other information, such as label",
                                                 " distribution, frequency of labelsets, data about attributes or label concurrence information.")), br(),
                               tags$small(paste0("- Use the 'EXIT' option to close the application."
                               ))
                             )),
                      column(6,
                             fluidRow(
                               wellPanel(
                                 h3("General summary"),
                                 tableOutput("summaryGeneral")
                               ),
                               wellPanel(
                                 h3("Label summary"),
                                 tableOutput("summaryLabels")
                               ),
                               wellPanel(
                                 h3("Labelset summary"),
                                 tableOutput("summaryLabelsets")
                               )
                             )
                      )))),
                  tabPanel("Labels", fluidPage(
                    titlePanel("Labels information"),
                    fluidRow(
                      column(6, wellPanel(dataTableOutput("labels"))),
                      column(6, wellPanel(
                        downloadButton("saveLabels", "Save plot"),
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
                  )),
                  tabPanel("Concurrence", fluidPage(
                    titlePanel("Label concurrence information"),
                    fluidRow(
                      column(5,
                             wellPanel(
                               h3("Select the labels to plot"),
                               dataTableOutput("tblConcurrence"))
                      ),
                      column(7,
                             downloadButton("saveConcurrence", "Save plot"),
                             wellPanel(plotOutput("labelLC",height="auto"))
                      )
                    )
                  )),
                  tabPanel("About", fluidPage(
                    p('mldrGUI is an EDA GUI for multilabel datasets developed on top of the mldr package.'),
                    p('(c) 2015 - Francisco Charte Ojeda (fcharte@ugr.es), David Charte Luque (fdavidcl@outlook.com)'),
                    p('See the package LICENSE file for license information')
                  ))
      ), width = 12)
  ))
