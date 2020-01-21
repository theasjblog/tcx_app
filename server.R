
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output) {
  
  loaded<-reactiveValues(data = NULL, splitsValues =NULL, splits = NULL,
                         splitsToEliminate = NULL,
                         xVariable = "Time", temp = NULL,
                         tempSmooth = NULL,
                         assistedSplits = NULL, checkBoxOptions = NULL, decoplingRes = NULL)
    
  
  observeEvent(input$file1, {
    loaded$data <- NULL
    loaded$splits <- NULL
    #loaded$data <- dataLoader(input$file1$datapath)
    tryCatchLoad()
    if (!is.null(loaded$data)){
      tryCatchAutoSp()
    }
      })
    
    
  LoadMsgSP <- function(cond){
    showModal(modalDialog(title = "Cannot do automatic activity splitting",
                          "If interest in splits, please create splits manually in the 'Splits' tab.")
    )
  }
  
  
  tryCatchAutoSp <- function(){
    tryCatch({
      withProgress(message = 'Creating splits. Please be patient', value = 1, {
        loaded$splits <- autoSplits(loaded$data)})
    },
    error = LoadMsgSP
    )
  }
  
  LoadMsg <- function(cond){
  showModal(modalDialog(title = "Invalid file",
                        "This activity file cannot be read. Is it a TCX?")
  )
}


tryCatchLoad <- function(){
  tryCatch({
    withProgress(message = 'Loading data. Please be patient', value = 1, {
    loaded$data <- dataLoader(input$file1$datapath)})
    },
    error = LoadMsg
)
}
  
  output$leafletFullUI <- renderLeaflet({
    req(loaded$data)
    req("LatitudeDegrees" %in% colnames(loaded$data))
    m<-generateMap(data = loaded$data)
    withProgress(message = 'Generating map. Please be patient', value = 1, {
      m})
  })
  
  output$resultsFullUI <- renderTable({
    req(loaded$data)
    withProgress(message = 'Generating summary table. Please be patient', value = 1, {
      res <- compareSplits(data = loaded$data,
                           ftp = input$ftp,
                           ftpType = input$ftpType)
    })
      res
  })
  
  output$resultsFullTitleUI <- renderUI({
    req(loaded$data)
    strong("Activity summary table")
  })
  
  output$metricsFullUI <- renderPlot({
    req(loaded$data)
    withProgress(message = 'Making plot. Please be patient', value = 1, {
      doPlots(data = loaded$data, xVariable = loaded$xVariable, doFacet = TRUE,
            sp = NULL)
    })
  })
  
  output$methodSplitUI <- renderUI({
    req(loaded$data)
    radioButtons("methodSplit", label = "How do you wan to create splits?",
                 choices = c("Manual", "Assisted"), selected = "Manual")
  })
  
  

  output$assistedUI <- renderUI({
    req(loaded$data)
    req(input$methodSplit)
    req(input$methodSplit == "Assisted")
    ch <- "Time"
    if("Distance" %in% colnames(loaded$data)){
      ch <- c(ch, "Distance")
    }
    tagList(
      selectInput("assistedX", label = "X axis", choices = ch, selected = "Time"),#no, only time or distance
      selectInput("assistedY", label = "Y axis", choices = colnames(loaded$data), selected = colnames(loaded$data)[1]),
      textInput("assistedSplits", "Proposed splits",
                value = NULL,
                placeholder = "Comma separated values")
      
      )
  })
  
  observeEvent(input$assistedSplits,{
    assistedSplits <- as.numeric(unlist(stringr::str_split(input$assistedSplits,",")))
    assistedSplits <- assistedSplits[!is.na(assistedSplits)]
    assistedSplits <- assistedSplits[assistedSplits > 0]
    assistedSplits <- assistedSplits[assistedSplits < max(loaded$data[,input$assistedX])]
    assistedSplits <- sort(unique(assistedSplits))
    if (length(assistedSplits) == 0){
      assistedSplits <- NULL
    }
    loaded$assistedSplits <- assistedSplits
  })
  
  output$assistedPlot <- renderPlot({
    req(input$assistedX)
    validate(need(!is.null(loaded$assistedSplits),
                  "Input some split values"))
    validate(need(!is.na(loaded$assistedSplits),
                  "Input some split values"))
    addSeparator(loaded$data, loaded$assistedSplits, input$assistedX, input$assistedY)
  })
  
  
  output$SplitGeneratorUI <- renderUI({
    req(loaded$data)
    req(input$methodSplit)
    req(input$methodSplit == "Manual")
    tagList(textInput("SplitsText", "Splits",
                 value = NULL,
                 placeholder = "Comma separated values"),
            selectInput("SplitsType", "Type of splitting",
                        c("Every m", "At specified m",
                          "Every min", "At specified min"),
                        selected = c("Every Km")
            )
    )
  })
  
  observeEvent(input$SplitsText,{
    loaded$splitsValues <- as.numeric(unlist(stringr::str_split(input$SplitsText,",")))
  })
  
  output$SplitsButtonUI <- renderUI({
    req(loaded$data)
    actionButton("doSplits", "Do splits")
    
    
  })
  
  observeEvent(input$doSplits,{
    loaded$splits <- NULL
    if(input$methodSplit == "Manual"){
      if (any(is.na(loaded$splitsValues)) | any(diff(loaded$splitsValues) <=0) | any(loaded$splitsValues <= 0)){
        warnMsg()
      } else {
        if(input$SplitsType == "Every m"){
          type <- "everyKm"
          loaded$splitsValues<-loaded$splitsValues[1]
        } else if (input$SplitsType == "At specified m"){
          type <- "thisKm"
        } else if (input$SplitsType == "Every min"){
          type <- "everyMin"
          loaded$splitsValues<-loaded$splitsValues[1]
        } else if (input$SplitsType == "At specified min"){
          type <- "thisMin"
        }
        args <- list(data = loaded$data,
                     splitValues = loaded$splitsValues,
                     type = type)
        loaded$splits <- do.call(createSplits, args)
      }
    } else if (input$methodSplit == "Assisted"){
      if (!is.null(loaded$assistedSplits)){
        if(!is.na(loaded$assistedSplits) | any(diff(loaded$assistedSplits) <=0) | any(loaded$assistedSplits <= 0)){
          type <- ifelse(input$assistedX == "Time", "thisMin", "thisKm")
          loaded$splits <- createSplits(loaded$data, splitValues = loaded$assistedSplits, type = type)
        } else {
          warnMsg()
        }
      } else {
          warnMsg()
        }
    }

    })
  

  warnMsg <- function(cond){
    showModal(modalDialog(title = "Missing information",
                          paste("Please input some valid split values",
                                sep = "")
    ))
  }
  

  output$leafletSplitUI <- renderLeaflet({
    req(loaded$splits)
    req("LatitudeDegrees" %in% colnames(loaded$splits[[1]]))
    m<-generateMap(data = loaded$splits)
    m
  })
  

  output$EliminateSplitsUI <- renderUI({
    req(loaded$splits)
    tagList(textInput("EliminateSplitsText", "Splits to eliminate",
                      value = NULL,
                      placeholder = "Comma separated values")
    )
  })
  
  observeEvent(input$EliminateSplitsText,{
    loaded$candidateSplitsToEliminate <- as.numeric(unlist(stringr::str_split(input$EliminateSplitsText,",")))
  })
  
  
  output$EliminateSplitsButtonUI <- renderUI({
    req(loaded$splits)
    req(loaded$candidateSplitsToEliminate)
    validate(need(!any(is.na(loaded$candidateSplitsToEliminate)),
                  "Only comma separated values allowed"))
    validate(need(!any(diff(loaded$candidateSplitsToEliminate) <=0) ,
                  "Unique and increasing numbers"))
    validate(need(!any(loaded$candidateSplitsToEliminate <= 0), 
                  "Positive numbers only"))
    validate(need(!any(loaded$candidateSplitsToEliminate > length(loaded$splits)),
                  "Max number of splits avilable exceeded"))
    actionButton("eliminateSplits", "Eliminate splits")
  })
  observeEvent(input$eliminateSplits,{
    loaded$splitsToEliminate <- loaded$candidateSplitsToEliminate
    loaded$splits<-purgeSplits(loaded$splits, loaded$splitsToEliminate)
    
  })
  


 output$xVariableUI <- renderUI({
   req(loaded$data)
   ch <- "Time"
   if("Distance" %in% colnames(loaded$data)){
     ch <- c(ch, "Distance")
   }
   radioButtons("xVariable", "x axis variable",
                choices = ch,
                selected = "Time")
 })
 
 observeEvent(input$xVariable,{
   loaded$xVariable <- input$xVariable
 })
 

 output$resultsSplitsTitleUI <- renderUI({
   req(loaded$splits)
   strong("Activity splits summary table")
 })
 
 
 output$resultsSplitsUI <- renderTable({
   req(loaded$splits)
   res <- compareSplits(data = loaded$splits,
                 ftp = input$ftp,
                 ftpType = input$ftpType)
   res
 })
 
 output$comparisonPlotUI <- renderPlot({
   req(loaded$splits)
   args<-list(data = loaded$splits,
              xVariable = loaded$xVariable,
              ftp = input$ftp,
              ftpType = input$ftpType)
   
   do.call(plotCompareSplit, args)
 })

  output$metricsSplitsUI <- renderPlot({
   req(loaded$splits)
    doPlots(data = loaded$data, xVariable = loaded$xVariable, doFacet = TRUE,
            sp = loaded$splits)
 })
 
output$ftpUI <- renderUI({
  req(loaded$data)
  fluidRow(
    column(6,
           numericInput("ftp", label = "FTP/LTH", min = 0, value = 0)
    ),
    
    column(6,
           radioButtons("ftpType", label = "FTP type", choices = c(#"power", 
             "pace", "HR"),
                        selected = "pace")
           )
    
    )
    
})

observeEvent(list(input$variableDecoupling, input$timeDecoupling),{
  req(loaded$data)
  req("Heart rate" %in% colnames(loaded$data))
  req(input$timeDecoupling)
  req(input$variableDecoupling)
  loaded$decouplingRes <- decoupling(loaded$data, input$variableDecoupling, input$timeDecoupling[1], input$timeDecoupling[2])
})


output$compareHRUI <- renderTable({
  req(!is.null(loaded$decouplingRes))
  loaded$decouplingRes$compareHR
})

output$compareVarUI <- renderTable({
  req(!is.null(loaded$decouplingRes))
  loaded$decouplingRes$compareVar
})

output$compareSlopesUI <- renderTable({
  req(!is.null(loaded$decouplingRes))
  loaded$decouplingRes$compareSlopes
})

output$decouplingPlotUI <- renderPlot({
  req(!is.null(loaded$decouplingRes))
  loaded$decouplingRes$p
})

output$sliderDecouplingUI <- renderUI({
  req(loaded$data)
  req("Heart rate" %in% colnames(loaded$data))
  sliderInput("timeDecoupling", "Time min and max", 0, round(max(loaded$data$Time),digits = 1), value = c(0, round(max(loaded$data$Time), digits = 1)))
})
 
output$variableDecouplingUI <- renderUI({
  req(loaded$data)
  req("Heart rate" %in% colnames(loaded$data))
  selectInput("variableDecoupling", label = "Decoupling variable", choices = colnames(loaded$data), selected = colnames(loaded$data)[1])
})

output$startTextUI <- renderUI({
  validate(need(!is.null(loaded$data),
                "Load some data"))
})


  

output$toSmoothUI <- renderUI({
  req(loaded$data)
  tagList(
    selectInput("toSmoothVar", "Variable to smooth",
              choices = colnames(loaded$data)),
    numericInput("kVal", "Amount", value = 1, min = 1, step = 2)
  )
})

observeEvent(list(input$toSmoothVar, input$kVal),{
  kVal <- as.numeric(input$kVal)
  validate(need(kVal>=1,
                "Amount must be at least 1"))
  validate(need(kVal%%2 == 1,
                "Amount must be odd"))
  loaded$tempSmooth <- gpxSmooth(data = loaded$data,
                                 k = kVal)
})

output$smoothPlotUI <- renderPlot({
  req(loaded$tempSmooth)
  plot(loaded$data$Time, loaded$data[,input$toSmoothVar], col = "blue",
       ylim = c(min(loaded$tempSmooth[,input$toSmoothVar]),
              max(loaded$tempSmooth[,input$toSmoothVar]))
       )
  lines(loaded$tempSmooth$Time, loaded$tempSmooth[,input$toSmoothVar], col = "red")
  
})

output$confirmSmoothUI <- renderUI({
  req(loaded$tempSmooth)
  actionButton("confirmSmooth", "Confirm")
})

observeEvent(input$confirmSmooth,{
  loaded$data <- loaded$tempSmooth
  loaded$tempSmooth <- NULL
})


})#end of server