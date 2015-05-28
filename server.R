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

    if (!(input$file$type %in% c("text/plain", "text/csv", "text/comma-separated-values",
                                 ".csv", ".txt", "application/vnd.ms-excel"))) {
      session$sendCustomMessage(type="showalert", paste(input$file$type, "file type not supported"))
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
      dat <- dat[, -2]
    } else {
      dat <- read.csv(input$file$datapath, stringsAsFactors=F)
      if (!(all(c("target", "date", "number_dialed") %in% tolower(names(dat))))) {
        session$sendCustomMessage(type="showalert", paste("The CSV file is not formatted properly.",
                                                          "At the very least you must have",
                                                          "Target, Date, and Number_Dialed",
                                                          "variables in your spreadsheet. Please review",
                                                          "the documentation for further information.",
                                                          sep=" "))
        return(NULL)
      }
      #dat <- prepCSV(dat)
    }
      dat$Target <- dat$Target %>% sapply(formatNumber) %>% unlist() %>% unname()
      dat$Number_Dialed <- dat$Number_Dialed %>% sapply(formatNumber) %>% unlist() %>% unname()
      #removing Item_Number" from the dataframe for readability and usage reasons
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
    generateNetwork(rawDataDF())
  })

  #########################
   #Create Condition Vars#
  #########################

  output$fileUploaded <- reactive({
    return(!is.null(rawData()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  output$showNetwork <- reactive({
    if (is.null(rawData())) return()
    n <- rawData()$Target %>% unique() %>% length()
    return(n > 1)
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
    l1 <- layout.fruchterman.reingold(g, niter=1000, area=vcount(g)^2.3, repuserad=vcount(g)^2.8)
    V(g)$size <- input$nodeSize
    V(g)$color <- ifelse(V(g)$name %in% rawData()$Target, input$targetColor, input$otherColor)
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
    rawDataDF() %>% datatable(rownames=FALSE)
  })

  output$freq <- DT::renderDataTable({
    if (is.null(rawData())) return(NULL)
    freqData() %>% datatable(rownames=FALSE)
  })

  output$common <- renderDataTable({
    if (is.null(rawData())) return(NULL)
    dat <- rawDataDF() %>% group_by(Number_Dialed) %>%
      mutate(Count=length(unique(Target)), Targets=paste(unique(Target), collapse=' '), Number_of_Calls=n()) %>%
      filter(Count > 1 & nchar(Number_Dialed) > 4) %>%
      arrange(Number_Dialed) %>%
      slice(1) %>%
      select(Number_Dialed, Targets, Number_of_Calls) %>%
      datatable(rownames=FALSE)
    dat
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

  output$commonUI <- renderUI({
    n <- rawData()$Target %>% unique() %>% length()
    if (n < 2) return()
    list(
      checkboxInput('commonFlag', 'View underlying data', value=F))
  })

  output$labelShow <- renderUI({
    if (input$showLabel == FALSE) return()
    list(
      sliderInput('labelSize', 'Label Size:',
                  min=1, max=5, value=1.2, step=.1, round=F, ticks=F),
      selectInput('labelColor', 'Label Color:',
                  choices=colors()[colorsIDX], selected="black"),
      sliderInput('offsetValue', 'Label Offset:',
                  min=0, max=1, value=0, step=.1, round=F, ticks=F)
    )
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
    filename = paste0('Phone Record Data ', Sys.Date(), '.csv'),
    content = function(file) {write.csv(rawData(), file, row.names=F)}
  )

  output$exportFreq <- downloadHandler(
    filename = paste0('Call Frequency Report ', Sys.Date(), '.csv'),
    content = function(file) {write.csv(freqData(), file, row.names=F)}
  )

  output$exportGraph <- downloadHandler(
    filename = paste0('Network Graph ', Sys.Date(), '.pdf'),
    content = function(file) {
      pdf(file, height=8.5, width=11, paper="a4r")
      graphInput()
      dev.off()
    })

  output$exportSingle <- downloadHandler(
    #function() is necessary in order to access the input variables
    filename = function() {paste0(input$target, ' ', input$month, ' ', input$year, '.pdf')},
    content = function(file) {
      pdf(file, height=8.5, width=11, paper="a4r")
      chartInput()
      dev.off()
    })

  output$exportMultiple <- downloadHandler(
    filename = "pdfs.zip",
    content = function(file) {
      if (input$target == 'Select...') {
        session$sendCustomMessage(type="showalert", "Need to choose a target first.")
        dev.off()
        return(NULL)
      }
      setwd(tempdir())
      fileNames <- c()
      target <- input$target
      dates <- rawData()$Date[rawData()$Target == target]
      years <- dates %>% as.Date(format="%m/%d/%y") %>% format("%Y") %>% unique()
      for (year in years) {
        months <- dates[paste0("20", substr(dates, 7, 8)) == year] %>%
          as.Date(format="%m/%d/%y") %>% sort %>% format("%B") %>% unique()
          for (month in months) {
            fileName <- paste0(target, ' ', month, ' ', year, '.pdf')
            fileNames <- c(fileNames, fileName)
            pdf(fileName, height=8.5, width=11, paper="a4r")
            chartInput()
            dev.off()
          }
      }
      zip(zipfile="pdfs.zip", files=fileNames)
    },
    contentType = "application/zip"
  )
})

