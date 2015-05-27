#!/usr/bin/env Rscript

library(shiny)
library(DT)
library(tools)
library(magrittr)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(igraph)

aggTable <- function(data) {
  x1 <- data %>% filter(Direction == 'Outgoing') %>%
    group_by(Target, Number_Dialed) %>%
    summarise(Outgoing = n())
  names(x1) <- c('Target', 'Number', 'Outgoing')
  x2 <- data %>% filter(Direction == 'Incoming') %>% group_by(Target, Number_Dialed) %>%
              summarise(Incoming = n())
  names(x2) <- c('Target', 'Number', 'Incoming')
  x3 <- full_join(x1, x2)
  return(x3)
}

parseTextBlock <- function(textBlock, pageNumber) {
  target <- grep("Voice Usage For: ", textBlock, value=T) %>%
    str_extract(., "\\([0-9]{3}\\)[0-9]{3}.[0-9]{4}") %>%
    gsub("\\(|\\)|-", "", .) %>%
    paste("1", ., sep='')

  #Block off call logs
  callLogStart <- ifelse(any(grepl("^\\(UTC\\)$", textBlock)),
                         grep("^\\(UTC\\)$", textBlock) + 1,
                         grep("[0-9]+(?=[\\s,]+[0-9]{2}/[0-9]{2}/[0-9]{2})", textBlock, perl=T))
  callLogFinish <- grep("AT&T\\s+Proprietary", textBlock) - 1

  #Get index of each call record so we can later parse out what we don't need
  #Check to see if call records are all in one block
  if (grep("Item[\\]s+[,]?Conn(?=.*)", textBlock, perl=T) == callLogStart) {
    itemNumbers <- dates <- times <- durations <- directions <- numbersDialed <- flagNumbers <- NA
    flags <- paste("Error found on page", pageNumber, sep=' ')
  } else {
    callRecordIDX <- grep("[0-9]+(?=\\s+[0-9]{2}/[0-9]{2}/[0-9]{2})", textBlock, perl=T)

    #Initialize variables
    itemNumbers <- dates <- times <- durations <- directions <- numbersDialed <- flags <- flagNumbers <- NULL

    for (i in 1:length(callRecordIDX)) {
      idx <- callRecordIDX[i]
      itemNumber <- str_extract(textBlock[idx], "[0-9]+(?=\\s+[0-9]{2}/[0-9]{2}/[0-9]{2})")
      date <- str_extract(textBlock[idx], "[0-9]{2}/[0-9]{2}/[0-9]{2}")
      duration <- str_extract_all(textBlock[idx], "[0-9]{1,2}:[0-9]{2}")[[1]][3]
      time <- str_extract_all(textBlock[idx], "[0-9]{1,2}:[0-9]{2}")[[1]][1]
      originating <- str_extract_all(textBlock[idx], "[0-9]{11}")[[1]][1]
      terminating <- str_extract_all(textBlock[idx], "[0-9]{11}")[[1]][2]
      direction <- ifelse(originating == target, "Outgoing", "Incoming")
      numberDialed <- ifelse(originating == target, terminating, originating)

      #Check for additional numbers listed below that may be flagged with codes
      if (i != length(callRecordIDX)) {
        if ((callRecordIDX[i + 1] - callRecordIDX[i]) > 1) {
          callRecordStart <- callRecordIDX[i] + 1
          callRecordFinish <- callRecordIDX[i + 1] - 1
        }
      } else {
        if ((grep("AT&T\\s+Proprietary", textBlock) - callRecordIDX[i]) > 1) {
          callRecordStart <- callRecordIDX[i] + 1
          callRecordFinish <- grep("AT&T\\s+Proprietary", textBlock) - 1
        }
      }

      if (exists("callRecordStart")) {
        line <- textBlock[callRecordStart:callRecordFinish] %>% paste(collapse=' ')
        if (grepl("[0-9]{7,}\\(F\\)", line)) {
          flag <- "F"
          flagNumber <- str_extract(line, "[0-9]{7,}")
        } else if (grepl("[0-9]{7,}\\(D\\)", line)) {
          flag <- "D"
          flagNumber <- str_extract(line, "[0-9]{7,}")
        } else if (grepl("[0-9]{7,}\\(OO\\)", line)) {
          flag <- "OO"
          flagNumber <- str_extract(line, "[0-9]{7,}")
        } else {
          flag <- ""
          flagNumber <- NA
        }
      } else {
        flag <- ""
        flagNumber <- NA
      }
      itemNumbers <- c(itemNumbers, itemNumber)
      dates <- c(dates, date)
      times <- c(times, time)
      durations <- c(durations, duration)
      directions <- c(directions, direction)
      numbersDialed <- c(numbersDialed, numberDialed)
      flags <- c(flags, flag)
      flagNumbers <- c(flagNumbers, flagNumber)
    }
  }
  dataRows <- data.frame("Target"=target, "Item_Number"=itemNumbers, "Date"=dates, "Time"=times,
                   "Duration"=durations, "Direction"=directions,
                   "Number_Dialed"=numbersDialed, "Flag"=flags, "Flagged_Number"=flagNumbers,
                   stringsAsFactors=F)

  return(dataRows)
}

plotGraph <- function(data, target, month, year) {
  monNames <- c('January', 'February', 'March', 'April', 'May', 'June',
                 'July', 'August', 'September', 'October', 'November', 'December')
  monDigits <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:12)
  endDays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  idx <- which(monNames == month)

  data$Date %<>% as.Date(format="%m/%d/%y")
  startDate <- paste0(year, '-', monDigits[idx], '-01') %>% as.Date()
  endDate <- paste0(year, '-', monDigits[idx], '-', endDays[idx]) %>% as.Date()
  plotData <- data %>%
    filter(between(Date, startDate, endDate)) %>%
    group_by(Date, Direction) %>%
    summarise(Freq=n())

  plotTitle <- paste0('No. of Calls by Day for ', target, ' in ', month, ' ', year)
  yMax <- max(plotData$Freq)
  yMax <- ceiling(yMax/10) * 10 + 10

  p <- ggplot(data=plotData, aes(x=Date, y=Freq, fill=Direction)) +
    geom_bar(stat='identity') +
    labs(x='Date', y='Count', title=plotTitle) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, vjust=-0.0001),
          axis.title.x = element_text(vjust = -0.25, size=rel(1.1)),
          axis.title.y = element_text(vjust = 1.2, size=rel(1.1)),
          panel.grid.minor = element_blank(),
          plot.title = element_text(vjust=1.6),
          legend.position='bottom',
          legend.title=element_blank()) +
    scale_x_date(labels=date_format("%a %d"),
                 breaks=date_breaks("day"),
                 expand=c(-0.02, 0.9)) +
    scale_y_continuous(limits=c(0, yMax), breaks=seq(from=0, to=yMax, by=5)) +
    scale_fill_manual(values=c('light blue', 'light green'))

  return(p)
}

formatNumber <- function(number) {
  if (nchar(number) == 11) tmp <- substr(number, 2, 11) else tmp <- number
  newNumber <- paste("(", substr(tmp, 1, 3), ") ", substr(tmp, 4, 6), "-", substr(tmp, 7, 10), sep='')
  return(newNumber)
}

generateNetwork <- function(data, degree) {
  filteredData <- data %>% filter(!is.na(Number_Dialed))
  networkData <- filteredData %>% group_by(Target, Number_Dialed) %>% summarise(Count=n())
  counts <- sapply(networkData$Number_Dialed,
                   function(z) length(networkData$Target[networkData$Number_Dialed == z]))
  numbersOfInterest <- names(counts[counts >= degree])
  filteredNetworkData <- networkData %>% filter(Number_Dialed %in% numbersOfInterest)
  return(filteredNetworkData)
}



