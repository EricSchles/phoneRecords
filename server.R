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

    if (input$file$type != "text/plain") {
      session$sendCustomMessage(type="showalert", "File type not supported")
      return(NULL)
    }

    #if (input$file$type) == 'text/plain') {
      txt <- readLines(con=input$file$datapath) %>% gsub('^\\s*|\\s*$', '', .)
      pageStart <- grep("Run Date: ", txt, fixed=T)
      pageFinish <- c(pageStart[-1] - 1, length(txt))
      dat <- NULL

      withProgress(message="Generating data", detail="Page 1", value=0, {
        for (i in 1:length(pageStart)) {
          textBlock <- txt[pageStart[i]:pageFinish[i]]
          if (input$complianceType == "AT&T - 2015") {
            dat <- rbind(dat, parseAttTextBlock1(textBlock, i))
          } else if (input$complianceType == "AT&T/Cingular - 2015") {
            dat <- rbind(dat, parseAttTextBlock2(textBlock))
          } else {
            dat <- data.frame("Error"="The data could not be parsed",
                              "Solution"="Email Bryan at brittenb@dany.nyc.gov or call at 212-335-4309",
                              stringsAsFactors=F)
          }
          incProgress(1/length(pageStart), detail=paste("Page", i))
        }
      })
    #} else {
     # dat <- read.csv(input$file$datapath, stringsAsFactors=F)
  #  }
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
  })

  output$noShow <- renderImage({
    list(src="404.jpg", alt="This page is useless. Go the the Call Frequency tab.", style="height:100%; width:100%")
  }, deleteFile=F)

  output$plot <- renderPlot({
    if (any(input$target == "Select...",
            input$year == "Select...",
            input$month == "Select...")) return(NULL)
    p <- plotGraph(rawDataDF(), input$target, input$month, input$year)
    print(p)
  })

  ########################
    #Create Dynamic UIs#
  ########################

  output$fileShow <- renderUI({
    if (input$complianceType == "Select...") return()
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
    if (n <= 1) return()
    list(
      checkboxInput('common', 'Show only numbers dialed by two targets or more', value=F))
  })

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

  ##########################
  #Create Download Handlers#
  ##########################

  output$exportData <- downloadHandler(
    filename = function() {paste0('C:\\Phone Record Data ', Sys.Date(), '.csv')},
    content = function(file) {write.csv(rawData(), file, row.names=F)}
  )

  output$exportFreq <- downloadHandler(
    filename = function() { paste0('C:\\Call Frequency Report ', Sys.Date(), '.csv') },
    content = function(file) {write.csv(freqData(), file, row.names=F)}
  )
})

