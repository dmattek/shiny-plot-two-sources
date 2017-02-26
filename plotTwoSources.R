# 2017-02-26
# This is a follow-up to Stephen McDaniel's response to Shiny Google Groups thread.
#
# Main issue:
#   By and large my Shiny apps process data loaded from a file. However, for
#   testing and/or demonstration purposes it is often convenient to include
#   an option that would generate an equivalent dataset. 
#
# In this example, we have two buttons that generate random numbers from 
# Gaussian and Poisson distributions, respectively. Additinally, we have
# an option to load a single-column text file with numbers. A histogram 
# will be plotted for any of these datasets.
#
# The code relies on:
# 1. Stephen McDaniel's solution to track usage of the action buttons 
#    and determine which data to utilize.
# 
# 2. ShinyJS library by Dean Attali to reset the state of fileInput
#    such that the file with the same name can be re-loaded consecutively.
# 
# Original problem posted by me on Google Groups:
# https://groups.google.com/d/msg/shiny-discuss/9yG7hPMT5Qc/Ujdh6L87FQAJ
#
# Discussion on the topic initiated by Stephen McDaniel
# https://groups.google.com/d/msg/shiny-discuss/gkeuyPAZndM/rLV_L-cvFgAJ
# 
# Steven's code:
# https://github.com/Stephen-McDaniel/reactives-used-if-else-logic-shiny
# 
#
# Program: plotTwoSources.R
#    Data: randomly generated or loaded from a text file with ONE column (with or without header)
#
# License: MIT License
# Attribution, package authors for shiny on CRAN.

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/

ui <- shinyUI(fluidPage(
  useShinyjs(), # Include shinyjs
  
  titlePanel("1 Histogram: 2 sources of data"),
  sidebarLayout(
    sidebarPanel(
      actionButton('inDataGen1', 'Generate normal distribution'),
      actionButton('inDataGen2', 'Generate Poisson distribution'),
      tags$hr(),
      fileInput(
        'inFileLoad',
        'Choose text file with 1 column of numbers',
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      ),
      checkboxInput("inChbHeader", "1st row of the file is a header", TRUE),
      actionButton("inButReset", "Reset file input")
      
    ),
    mainPanel(plotOutput("plotHist", width = "100%"))
  )
))

server <- shinyServer(function(input, output, session) {
  # This is only set at session start
  # we use this as a way to determine which input was
  # clicked in the dataInBoth reactive
  counter <- reactiveValues(
    # The value of inDataGen1,2 actionButton is the number of times they were pressed
    dataGen1 = isolate(input$inDataGen1),
    dataGen2 = isolate(input$inDataGen2),
    
    # The value of inFileLoad is a dataframe, thus to assignthe number of presses we need to perform this check 
    dataLoad = isolate(ifelse(is.null(input$inFileLoad), 0, 1))
  )
  
  # This button will reset the inFileLoad
  observeEvent(input$inButReset, {
    reset("inFileLoad")  # reset is a shinyjs function
  })
  
  # generate random dataset 1
  dataIn1 <- eventReactive(input$inDataGen1, {
    cat("dataIn1\n")
    rnorm(1000)
  })
  
  # generate random dataset 2
  dataIn2 <- eventReactive(input$inDataGen2, {
    cat("dataIn2\n")
    rpois(1000, 2)
  })
  
  # load the text file
  dataLoad <- eventReactive(input$inFileLoad, {
    cat("inFileLoad\n")
    loc.x = read.csv(input$inFileLoad$datapath, header = input$inChbHeader)
    return(loc.x[, names(loc.x)])
  })
  
  dataInBoth <- reactive({
    # Without direct references to inDataGen1,2 and inFileLoad, inDataGen2
    #    does not trigger running this reactive once inDataGen1 is used.
    # This is one of the more nuanced areas of reactive programming in shiny
    #    due to the if else logic, it isn't fetched once inDataGen1 is available
    # The morale is use direct retrieval of inputs to guarantee they are available
    #    for if else logic checks!
    
    locInGen1 = input$inDataGen1
    locInGen2 = input$inDataGen2
    locInLoad = ifelse(is.null(input$inFileLoad), 0, isolate(counter$dataLoad) + 1)
    
    cat("dataInBoth\n1: ",
        locInGen1,
        "\n2: ",
        locInGen2,
        "\n3: ",
        locInLoad,
        "\n")
    
    # isolate the checks of counter reactiveValues
    # as we set the values in this same reactive
    if (locInGen1 != isolate(counter$dataGen1)) {
      cat("dataInBoth if inDataGen1\n")
      dm = dataIn1()
      # no need to isolate updating the counter reactive values!
      counter$dataGen1 <- locInGen1
    } else if (locInGen2 != isolate(counter$dataGen2)) {
      cat("dataInBoth if inDataGen2\n")
      dm = dataIn2()
      # no need to isolate updating the counter reactive values!
      counter$dataGen2 <- locInGen2
    } else if (locInLoad != isolate(counter$dataLoad)) {
      cat("dataInBoth if inDataLoad\n")
      dm = dataLoad()
      # no need to isolate updating the counter reactive values!
      counter$dataLoad <- locInLoad
    } else {
      cat("dataInBoth else\n")
      dm = NULL
    }
    return(dm)
  })
  
  output$plotHist <- renderPlot({
    cat("plotHist\n")
    dm = dataInBoth()
    cat("plotHist on to plot\n\n")
    if (is.null(dm))
      return(NULL)
    else
      return(plot(hist(dm)))
    
  })
})

shinyApp(ui = ui, server = server)