# load dependencies
source('dependencies.R')
# source supporting functions. These should be an independent package
for (i in list.files('./auxFunctions')) {
  source(paste0('./auxFunctions/', i))
}

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
shinyUI(fluidPage(
  mainPanel(
            tabsetPanel(
              tabPanel("Main",
                       fluidRow(
                         
                         column(6,
                                wellPanel(
                                  fileInput('file1', 'Choose TCX File',
                                          accept = ".tcx"),
                                  p("If you do not have a Garmin tcx file, you can download one",
                                    tags$a(href = "https://drive.google.com/drive/folders/1m-nQ3mfDI0oM0KwITwsNWkTUYVqpIcGy?usp=sharing", " here",
                                           target = "_blank")),
                                  uiOutput("ftpUI")
                                  )
                                )#,
                         
                         #column(6,
                                #wellPanel(
                                #  uiOutput("xVariableUI"),
                                #  uiOutput("toSmoothUI"),
                                #  plotOutput("smoothPlotUI"),
                                #  uiOutput("confirmSmoothUI")
                                #)
                                #)
                         
                       ),
                       wellPanel(
                         uiOutput("resultsFullTitleUI"),
                         tableOutput("resultsFullUI")
                         ),
                       
                       fluidRow(
                         
                           column(6,
                                  plotOutput("metricsFullUI")
                           ),
                           
                           column(6,
                                  leafletOutput("leafletFullUI")
                           ) 
                         
                       )
              #         
              #         ),
              #
              #tabPanel("Splits",
              #         fluidRow(
              #           
              #           column(6,
              #                  wellPanel(strong("Splits creator"),
              #                    uiOutput("methodSplitUI"),
              #                    uiOutput("SplitGeneratorUI"),
              #                    uiOutput("assistedUI"),
              #                    uiOutput("SplitsButtonUI")
              #                  ),
              #                  wellPanel(
              #                            uiOutput("EliminateSplitsUI"),
              #                            uiOutput("EliminateSplitsButtonUI")
              #                  )
              #                  ),
              #           
              #           column(6,
              #                  plotOutput("assistedPlot")
              #                  )
              #         ),
              #         
              #         fluidRow(
              #           column(6,
              #                  plotOutput("comparisonPlotUI")
              #                  ),
              #           column(6,
              #                  leafletOutput("leafletSplitUI")
              #                  )
              #         ),
              #         
              #         plotOutput("metricsSplitsUI"),
              #         
              #         wellPanel(
              #           uiOutput("resultsSplitsTitleUI"),
              #           tableOutput("resultsSplitsUI")
              #           )
              #         
              #         
              #),
              #tabPanel("Decoupling",
              #         fluidRow(
              #           column(3,
              #                  wellPanel(strong("Settings"),
              #           fluidRow(
              #           column(12,
              #                  uiOutput("sliderDecouplingUI"))
              #           ),
              #         fluidRow(
              #           column(12,
              #                  uiOutput("variableDecouplingUI"))
              #         ))),
              #           column(9,
              #                  plotOutput("decouplingPlotUI"))
              #         ),
              #         
              #         
              #         
              #           
              #                  wellPanel(strong("Comparison of heart rate across the two halves"),
              #                            tableOutput("compareHRUI")),
              #                  wellPanel(strong("Comparison of selected variables across the two halves"),
              #                            tableOutput("compareVarUI")),
              #                  wellPanel(strong("Comparison of heart rate and selected variable across the entire selection"),
              #                            tableOutput("compareSlopesUI"))
              #                  
              #         
            #
            #           
            #           )
            #)
            ),
            tabPanel('About',
                     includeMarkdown('vignettes/about.md'))
  )
  )))
