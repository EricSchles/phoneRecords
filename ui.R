#!/usr/bin/env Rscript

shinyUI(pageWithSidebar(
  headerPanel("Phone Record Analyzer"),

  sidebarPanel(
    conditionalPanel(condition='input.myTabs == "Raw Data" && !output.fileUploaded',
                    h3('Upload Data'),
                    h6("To begin, please upload your phone record file, keeping in mind that only one file can be
                        uploaded at a time. The file can either be a TXT file that will be parsed into a data table,
                        or a CSV file that has already been parsed and cleaned up."),
                    h6("If you have any issues with this tool, please contact Bryan at brittenb@dany.nyc.gov or 212-335-4309"),
                    fileInput('file', 'Choose a file', multiple=F,
                              accept=c('text/csv', 'text/comma-separated-values',
                                       'text/plain', '.csv', '.txt')),
                    tags$hr()),

    conditionalPanel(condition='!(input.myTabs == "Raw Data") && !output.fileUploaded',
                     h3('Warning:'),
                     h5('In order to use the "Call Frequency", "Common Call" and "Call Network" features, you
                              must first load a TXT or CSV file.')),

    conditionalPanel(condition='input.myTabs == "Raw Data" && output.fileUploaded',
                     h3('Raw Data View'),
                     h5('This sheet shows the data in the raw form. You can reorder the data
                              based on name, phone number, type of call or number of times that
                              number was called. You can also use the search boxes (at the top and
                              bottom of the table) to search for specific numbers or names.'),
                     downloadButton('exportData', 'Export')),

    conditionalPanel(condition='input.myTabs == "Call Frequency" && output.fileUploaded',
                     h3('Call Frequency Table'),
                     h5('This tab shows how many times each number was an incoming or
                              outgoing call, grouped by the target and number. As an example,
                              John Doe may have received a call (Incoming) from 555-5555 16 times and
                              called (Outgoing) that same number 23 times. You can use the search
                              boxes to look for specific targets or phone numbers.'),
                     downloadButton('exportFreq', 'Export')),

    conditionalPanel(condition='input.myTabs == "Common Call Report" && output.fileUploaded',
                     h3('Common Call Report'),
                     h5('This report shows the common phone calls made between individuals.
                              For example, the report may show that John Doe and Jane Smith both
                              called an attorney named Ann Hall 46 and 54 times, respectively. All
                              relationships are based on outgoing calls only.')),

    conditionalPanel(condition='input.myTabs == "Call Record Graphs" && output.fileUploaded',
                     h3('Call Record Graphs'),
                     selectInput("target", "Target Number", choices=''),
                     selectInput("year", "Year", choices=''),
                     selectInput("month", "Month", choices=c("Select...", "January", "February", "March", "April", "May",
                                                             "June", "July", "August", "September", "October", "November", "December"))),
                     #submitButton()),

    conditionalPanel(condition='input.myTabs == "Call Network" && output.fileUploaded',
                     h3('Call Network Analysis'),
                     h5('This analysis shows a graphical representation of the Common Call Report.
                              Lines represent phone calls made to an individual and are based on outgoing
                              calls only. For example, if John Doe and Jane Smith are connected by a straight
                              line, it means that John Doe called Jane Smith with the width of the line
                              graphically representing how many times.'))
  ),

  mainPanel(
    tabsetPanel(type='tabs', id='myTabs',
                tabPanel('Raw Data', dataTableOutput('raw')),
                tabPanel('Call Frequency', dataTableOutput('freq')),
                tabPanel('Common Call Report', verbatimTextOutput('common')),
                tabPanel('Call Record Graphs', plotOutput('plot')),
                tabPanel('Call Network', simpleNetworkOutput('network'))
    )
  )
))

