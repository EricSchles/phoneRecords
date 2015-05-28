#!/usr/bin/env Rscript
options(shiny.maxRequestSize=3000*1024^2)
colorsIDX <- sort(sample(1:length(colors())))

source('helper.R')

shinyServer(function(input, output, session) {

  ########################
   #Create All Datasets#
  ########################

  rawData <- reactive({
    if (is.null(input$file)) return(NULL)
    #validate(need(input$file$type == "text/plain",
     #             message=paste("Currently, this app can only take TXT files.",
      #                          "CSV functionality will be added later.",
       #                         "Please reload the app and try again.", sep=" ")))

    if (!(input$file$type %in% c("text/plain", "text/csv", "text/comma-separated-values", ".csv", ".txt"))) {
      session$sendCustomMessage(type="showalert", "File type not supported")
      return(NULL)
    }

    if (input$file$type %in% c('text/plain', '.txt')) {
      txt <- readLines(con=input$file$datapath) %>% gsub('^\\s*|\\s*$', '', .)
      pageStart <- grep("Run Date: ", txt, fixed=T)
      pageFinish <- c(pageStart[-1] - 1, length(txt))
      dat <- NULL

      withProgress(message="Generating data", detail="Page 1", value=0, {
        for (i in 1:length(pageStart)) {
          textBlock <- txt[pageStart[i]:pageFinish[i]]
          if (input$complianceType == "AT&T Text File (2015)") {
            if (input$method == "I created the file myself") {
              dat <- rbind(dat, parseAttTextBlock1(textBlock, i))
            } else {
              dat <- rbind(dat, parseAttTextBlock2(textBlock))
            }
          }
          incProgress(1/length(pageStart), detail=paste("Page", i))
        }
      })
    } else {
      dat <- read.csv(input$file$datapath, stringsAsFactors=F)
      if (!(all(c("Target", "Date", "Originating", "Terminating") %in% names(dat)))) {
        session$sendCustomMessage(type="showalert", paste("The CSV file is not formatted properly.",
                                                          "At the very least you must have the",
                                                          "Target, Date, Originating, and Terminating",
                                                          "names in your spreadsheet. Please review",
                                                          "the documentation for further information.",
                                                          sep=" "))
        return(NULL)
      }
      dat <- prepCSV(dat)
    }
      dat$Target <- dat$Target %>% sapply(formatNumber) %>% unlist() %>% unname()
      dat$Number_Dialed <- dat$Number_Dialed %>% sapply(formatNumber) %>% unlist() %>% unname()
      as.list(dat)
  })

  rawDataDF <- reactive({
    if (is.null(input$file)) return(NULL)
    as.data.frame(rawData(), stringsAsFactors=F)
  })

  freqData <- reactive({
    if (is.null(input$file)) return(NULL)
    rawDataDF() %>% aggTable()
  })

  networkData <- reactive({
    deg <- input$degree
    generateNetwork(rawDataDF(), input$degree)
  })

  #########################
   #Create Condition Vars#
  #########################

  output$fileUploaded <- reactive({
    return(!is.null(rawData()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  output$showNetwork <- reactive({
    if (is.null(input$file)) return()
    rawData()$Target %>% unique() %>% length()
  })
  outputOptions(output, 'showNetwork', suspendWhenHidden=FALSE)

  #########################
   #Create Plot Functions#
  #########################

  chartInput <- function() {
    if (any(input$target == "Select...",
            input$year == "Select...",
            input$month == "Select...")) return(NULL)
    p <- plotGraph(rawDataDF(), input$target, input$month, input$year)
    print(p)
  }

  graphInput <- function() {
    if (is.null(rawData())) return(NULL)
    g <- graph.data.frame(networkData(), directed=FALSE)
    l1 <- layout.fruchterman.reingold(g)
    badVertices <- V(g)[degree(g)<input$degree]
    g <- delete.vertices(g, badVertices)
    V(g)$size <- input$nodeSize
    V(g)$color <- input$nodeColor
    V(g)$label.cex <- input$labelSize
    V(g)$label.color <- input$labelColor
    V(g)$label.dist <- input$offsetValue
    if (input$showLabel == FALSE) V(g)$label <- NA
    plot(g, layout=l1)
  }

  ########################
      #Create Outputs#
  ########################

  output$logo <- renderImage({
    list(src="new_dany_seal.png", alt="DANY Logo", width=400, height=400)
  }, deleteFile=F)

  output$raw <- DT::renderDataTable({
    if (is.null(rawData())) return(NULL)
    rawDataDF() %>% datatable()
  })

  output$freq <- DT::renderDataTable({
    if (is.null(rawData())) return(NULL)
    freqData() %>% datatable()
  })

  output$network <- renderPlot({
    graphInput()
  })

  output$noShow <- renderImage({
    list(src="404.jpg", alt="This page is useless. Go the the Call Frequency tab.", style="height:100%; width:100%")
  }, deleteFile=F)

  output$plot <- renderPlot({
    chartInput()
  })

  ########################
    #Create Dynamic UIs#
  ########################

  output$fileShow <- renderUI({
    if (any(input$complianceType == "Select...",
            input$method == "Select...")) return()
    list(
      fileInput('file', 'Choose a file', multiple=F,
                accept=c('text/csv', 'text/comma-separated-values',
                         'text/plain', '.csv', '.txt')),
      h6("The data can take a while to load and display, depending on size, so please be patient.",
         style="color:red; font-weight:bold")
    )
  })

  output$labelShow <- renderUI({
    if (input$showLabel == FALSE) return()
    list(
      sliderInput('labelSize', 'Label Size:',
                  min=1, max=5, value=1.2, step=.1, round=F, ticks=F),
      selectInput('labelColor', 'Label Color:',
                  choices=colors()[colorsIDX], selected="black"),
      sliderInput('offsetValue', 'Offset:',
                  min=0, max=1, value=0, step=.1, round=F, ticks=F)
    )
  })

  output$common <- renderUI({
    n <- rawData()$Target %>% unique() %>% length()
    if (n < 2) return()
    list(
      checkboxInput('common', 'Show only numbers dialed by two targets or more', value=F))
  })

  observe({
    targets <- rawData()$Target %>% unique() %>% unlist() %>% unname()
    if (!is.null(rawData()$Date)) {
      years <- rawData()$Date %>% as.POSIXlt(format="%m/%d/%y") %>% .$year + 1900
      years %<>% as.numeric() %>% sort()
    } else {
      years <- 2015
    }
    updateSelectInput(session, 'target', choices=c('Select...', targets))
    updateSelectInput(session, 'year', choices=c('Select...', years))
  })

  observe({
    if (is.null(rawData())) return()
    dates <- rawData()$Date
    dates <- dates[paste0("20", substr(dates, 7, 8)) == input$year]
    months <- dates %>% unique() %>% as.Date(format="%m/%d/%y") %>% sort() %>% format("%B")
    updateSelectInput(session, 'month', choices=c('Select...', months))
  })

  ##########################
  #Create Download Handlers#
  ##########################

  output$exportData <- downloadHandler(
    filename = paste0('C:\\Phone Record Data ', Sys.Date(), '.csv'),
    content = function(file) {write.csv(rawData(), file, row.names=F)}
  )

  output$exportFreq <- downloadHandler(
    filename = paste0('C:\\Call Frequency Report ', Sys.Date(), '.csv'),
    content = function(file) {write.csv(freqData(), file, row.names=F)}
  )

  output$exportGraph <- downloadHandler(
    filename = paste0('C:\\Network Graph ', Sys.Date(), '.png'),
    content = function(file) {
      png(file)
      graphInput()
      dev.off()
    })

  output$exportSingle <- downloadHandler(
    filename = paste0('C:\\', input$target, ' ', input$month, ' ', input$year, '.png'),
    content = function(file) {
      png(file)
      chartInput()
      dev.off()
    })

})

