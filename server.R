#!/usr/bin/env Rscript
options(shiny.maxRequestSize=3000*1024^2)

library(shiny)
library(DT)
source('helper.R')

shinyServer(function(input, output, session) {

  rawData <- reactive({
    if (is.null(input$file)) return(NULL)
    #validate(need(file_ext(input$file$datapath) %in% c('txt', 'csv'),
     #        input$file$datapath))

    #if (file_ext(input$file$datapath) == 'txt') {
      txt <- readLines(con=input$file$datapath) %>% gsub('^\\s*|\\s*$', '', .)
      pageStart <- grep("Run Date: ", txt, fixed=T)
      pageFinish <- c(pageStart[-1] - 1, length(txt))
      dat <- NULL

      withProgress(message="Generating data", detail="Page 1", value=0, {
        for (i in 1:length(pageStart)) {
          textBlock <- txt[pageStart[i]:pageFinish[i]]
          dat <- rbind(dat, parseTextBlock(textBlock, i))
          incProgress(1/length(pageStart), detail=paste("Page", i))
        }
      })
    #} else {
     # dat <- read.csv(input$file$datapath, stringsAsFactors=F)
  #  }
      as.list(dat)
  })

  rawDataDF <- reactive({
    if (is.null(input$file)) return(NULL)
    as.data.frame(rawData(), stringsAsFactors=F)
  })

  output$fileUploaded <- reactive({
    return(!is.null(rawData()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  freqData <- reactive({
    if (is.null(input$file)) return(NULL)
    rawDataDF() %>% aggTable()
  })

  commonData <- reactive({
    if (is.null(input$file)) return(NULL)
    freqData() %>% genList()
  })

  output$raw <- renderDataTable({
    if (is.null(rawData())) return(NULL)
    rawDataDF() %>% datatable()
  })

  output$freq <- renderDataTable({
    if (is.null(rawData())) return(NULL)
    freqData() %>% datatable
  })

  output$common <- renderPrint({
    commonData()
  })

  output$plot <- renderPlot({
    if (any(input$target == "Select...",
            input$year == "Select...",
            input$month == "Select...")) return(NULL)
    p <- plotGraph(rawDataDF(), input$target, input$month, input$year)
    print(p)
  })

  output$network <- renderSimpleNetwork({
    if (is.null(rawData())) return(NULL)
    networkData <- generateNetwork(rawDataDF())
    simpleNetwork(networkData)
  })

  output$exportData <- downloadHandler(
    filename = function() {paste0('C:\\Phone Record Data ', Sys.Date(), '.csv')},
    content = function(file) {write.csv(rawData(), file, row.names=F)}
  )

  output$exportFreq <- downloadHandler(
    filename = function() { paste0('C:\\Call Frequency Report ', Sys.Date(), '.csv') },
    content = function(file) {write.csv(freqData(), file, row.names=F)}
  )

  observe({
    targets <- rawData()$Target %>%
      unique() %>%
      sapply(formatNumber) %>%
      unlist() %>%
      unname()
    if (!is.null(rawData()$Date)) {
      years <- rawData()$Date %>% as.POSIXlt(format="%m/%d/%y") %>% .$year + 1900
      years %<>% as.numeric() %>% sort()
    } else {
      years <- 2015
    }
    updateSelectInput(session, 'target', choices=c('Select...', targets))
    updateSelectInput(session, 'year', choices=c('Select...', years))
  })

})

