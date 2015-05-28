#!/usr/bin/env Rscript
colorsIDX <- sort(sample(1:length(colors())))

shinyUI(pageWithSidebar(
  headerPanel("Phone Record Analyzer"),


  sidebarPanel(
    conditionalPanel(condition='input.myTabs == "Raw Data" && !output.fileUploaded',
                    h3('Upload Data'),
                    h6("To begin, please upload your phone record file, keeping in mind that only one file can be
                        uploaded at a time. The file can either be a TXT file that will be parsed into a data table,
                        or a CSV file that has already been parsed and cleaned up."),
                    h6("If you have any issues with this tool, please contact Bryan at brittenb@dany.nyc.gov or 212-335-4309"),
                    br(),
                    br(),
                    selectInput('complianceType', 'What type of file are you uploading?',
                                choices=c("Select...", "AT&T Text File (2015)", "Raw CSV File")),
                    selectInput("method", "How was the file created?",
                                choices=c("Select...", "I created the file myself", "It was sent to me as compliance"),
                                selected="Select..."),
                    uiOutput('fileShow'),
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

    conditionalPanel(condition='input.myTabs == "Common Call Data" && output.fileUploaded',
                     h3('Common Call Report'),
                     h5('This report shows the common phone calls made between individuals.
                              For example, the report may show that John Doe and Jane Smith both
                              called an attorney named Ann Hall 46 and 54 times, respectively. All
                              relationships are based on outgoing calls only.')),


    conditionalPanel(condition='input.myTabs == "Common Call Network" && output.fileUploaded && !output.showNetwork',
                     h3('Call Network Analysis'),
                     h6(paste("The Common Call output is only worthwhile if you have more than one target.",
                              "With one target, the graph merely shows everyone the target called, which",
                              "is better represented in tabular format. You'll want to check the Call Frequency",
                              "tab to get a better sense of the call patterns.", sep=" "))),

    conditionalPanel(condition='input.myTabs == "Common Call Network" && output.fileUploaded && output.showNetwork',
                     h3('Call Network Analysis'),
                     uiOutput('commonUI'),
                     selectInput('targetColor', 'Target Node Color:',
                                 choices=colors()[colorsIDX], selected="lightgreen"),
                     selectInput('otherColor', 'Other Node Color:',
                                 choices=colors()[colorsIDX], selected="lightblue"),
                     sliderInput('nodeSize', 'Node Size:',
                                 min=1, max=50, value=20, step=1, round=F, ticks=F),
                     checkboxInput('showLabel', 'Show Labels', value=TRUE),
                     uiOutput('labelShow'),
                     uiOutput('offsetShow'),
                     downloadButton('exportGraph', 'Export Graph')),

    conditionalPanel(condition='input.myTabs == "Call Record Graphs" && output.fileUploaded',
                     h3('Call Record Graphs'),
                     selectInput("target", "Target Number", choices=''),
                     selectInput("year", "Year", choices=''),
                     selectInput("month", "Month", choices=c("Select...", "January", "February", "March", "April", "May",
                                                             "June", "July", "August", "September", "October", "November", "December")),
                     br(),
                     p('To download the current graph press this button:'),
                     downloadButton('exportSingle', 'Export Single Graph'),
                     br(),
                     br(),
                     p('To download a graph for each month in each year for the current target, press this button:'),
                     actionButton('exportMultiple', 'Export Multiple Graphs', icon("download")))

  ),

  mainPanel(
    tags$head(tags$script(src="alert.js")),
    conditionalPanel(condition="!output.fileUploaded",
                     tags$style(type="text/css", "div #logo{text-align:center}"),
                     imageOutput('logo')),

    conditionalPanel(condition="output.fileUploaded",
                     tabsetPanel(type='tabs', id='myTabs',
                                 tabPanel('Raw Data', DT::dataTableOutput('raw')),
                                 tabPanel('Call Frequency', DT::dataTableOutput('freq')),
                                 tabPanel('Common Call Network',
                                          conditionalPanel(condition="!output.showNetwork",
                                                           tags$style(type="text/css",
                                                                      "div #noShow{text-align:center}"),
                                                           h1("Uh-Oh!", style="text-align:center"),
                                                           p("Just kidding, things are fine. Read the panel to the left.", style="text-align:center"),
                                                           imageOutput('noShow')),
                                          conditionalPanel(condition="output.showNetwork && !input.commonFlag",
                                                           plotOutput('network')),
                                          conditionalPanel(condition="output.showNetwork && input.commonFlag",
                                                           dataTableOutput('common'))),
                                 tabPanel('Call Record Graphs', plotOutput('plot'))))
  )
))

