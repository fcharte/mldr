#
# This file belongs to mldrGUI, an EDA GUI for multilabel datasets developed on top of the mldr package.
# (c) 2015 - Francisco Charte Ojeda (fcharte@ugr.es), David Charte Luque (fdavidcl@outlook.com)
# See the package LICENSE file for license information
#

shinyUI(
  fluidPage(
    tags$head(
      tags$style(
        HTML(".tab-pane {
                border: 2px solid #003955;
                background: #ddf4ff !important;
                margin-bottom: 3em;
                box-shadow: 8px 8px 8px rgba(0,0,0,0.2);
                border-radius: 4px;
              }
              .nav-pills a b {
                color: #333;
              }
              table.table-bordered{
                background: #fbfbfb;
              }
              .tab-content {
                overflow: inherit;
              }
              [class*='span'] {
                margin-left: 0;
              }
              .well {
                overflow-x: auto;
              }
              .shiny-plot-output img {
                max-width: 100%;
                height: auto !important;
              }")
      ),
      tags$script(HTML("
          const ONECOL_WIDTH = 767;

          $(document).ready(function () {
            var graph = $('#labelLC').parent();
            var origOffsetY = $('.well').offset().top + $(window).scrollTop();

            document.onscroll = function () {
                if ($(window).scrollTop() >= origOffsetY && $(window).width() > ONECOL_WIDTH) {
                    graph.css('margin-top', ($(window).scrollTop() - origOffsetY) + 'px');
                } else {
                    graph.css('margin-top', '0px');
                }
            };
        });
      "))),
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
                      column(5,
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
                               tags$small(HTML(paste0("<ul><li>Use the controls above to select one of the MLDs included in the package, ",
                                                 "or select an .arff and .xml file in your system to load any MLD.</li>"))),
                               tags$small(HTML(paste0("<li>Once the MLD has been loaded, you will see its basic traits in this page.</li>"))),
                               tags$small(HTML(paste0("<li>Use the tabs at the top of the page to explore other information, such as label",
                                                 " distribution, frequency of labelsets, data about attributes or label concurrence information.</li>"))),
                               tags$small(HTML(paste0("<li>Use the 'EXIT' option to close the application.</li></ul>"
                               )))
                             )),
                      column(6,
                             fluidRow(
                               wellPanel(
                                 h3("Visual summary"),
                                 downloadButton("saveAT", "Save plot"),
                                 plotOutput("attributeByType", height = "auto"),
                                 hr(),
                                 downloadButton("saveCH", "Save plot"),
                                 plotOutput("cardHistogram", height = "auto"),
                                 hr(),
                                 downloadButton("saveLH", "Save plot"),
                                 plotOutput("labelHistogram", height = "auto"),
                                 hr(),
                                 downloadButton("saveLSH", "Save plot"),
                                 plotOutput("labelsetHistogram", height = "auto")
                               ),
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
                    fluidRow(
                      column(6, wellPanel(dataTableOutput("labelsets"))),
                      column(6, wellPanel(
                        downloadButton("saveLabelsets", "Save plot"),
                        plotOutput("labelsetHC",height="auto"))
                      )
                    )
                  )),
                  tabPanel("Attributes", fluidPage(
                    titlePanel("Attributes information"),
                    wellPanel(dataTableOutput("attributes"))
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
                             wellPanel(
                               downloadButton("saveConcurrence", "Save plot"),
                               plotOutput("labelLC",height="auto"))
                      )
                    )
                  )),
                  tabPanel("About", fluidPage(
                    titlePanel("About mldrGUI"),
                    wellPanel(p('mldrGUI is an EDA GUI for multilabel datasets developed on top of the mldr package.'),
                    p(HTML('&copy; 2015 &mdash; Francisco Charte Ojeda (fcharte@ugr.es), David Charte Luque (fdavidcl@outlook.com)')),
                    p('See the package LICENSE file for license information'))
                  ))
      ), width = 12)
  ))
