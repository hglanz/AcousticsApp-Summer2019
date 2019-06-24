library(shiny)
library(plotly)
library(shinyalert)
library(shinyjs)
library(shinyBS)


# useShinyalert()

pageWithSidebar(

    headerPanel("",
              windowTitle = "Acoustic Analysis"),
    
    sidebarPanel(width = 3,
        h1("Acoustic Analysis"),
        br(),
          
        # Upload Data File
        uiOutput('resettableInput'),
        br(),
        actionButton("resetAll", "Reset Inputs"),

        conditionalPanel("output.filechosen == true",
          # h4("Native Information About .wav File:"),
          br(),
          
          uiOutput("wavinfo"),
          
          br(),
          uiOutput("mintimelimit"),
          br(),
          uiOutput("maxtimelimit"),
          uiOutput("minfreqlimit"),
          uiOutput("maxfreqlimit"),
          
          ## Specrum
          div(style="display:inline-block", uiOutput("spectrumcheck")),
          div(style="display:inline-block", actionButton("specthelp", "", icon = icon("question-circle"))),
          uiOutput("spectrummin"),
          uiOutput("spectrummax"),
          
          ## Sampling Rate
          div(style="display:inline-block", uiOutput("sampcheck")),
          div(style="display:inline-block", actionButton("samphelp", "", icon = icon("question-circle"))),
          uiOutput("samplingrate"),
          
          ## Window Function
          div(style="display:inline-block", uiOutput("windowcheck")),
          div(style="display:inline-block", actionButton("windowhelp", "", icon = icon("question-circle"))),
          uiOutput("window"),
          
          ## Overlapping
          div(style="display:inline-block", uiOutput("ovlpcheck")),
          div(style="display:inline-block", actionButton("ovlphelp", "", icon = icon("question-circle"))),
          uiOutput("ovlp"),
          
          ## Zero Padding
          div(style="display:inline-block", uiOutput("zpcheck")),
          div(style="display:inline-block", actionButton("zphelp", "", icon = icon("question-circle"))),
          uiOutput("zp")
        )
    ),
    
    mainPanel(width = 9,
        plotOutput("spectro",
                   width = "auto",
                   height = "800px"),
        
        uiOutput("SpecHelpInfo"),
        
        uiOutput("SampHelpInfo"),
        
        uiOutput("WindowHelpInfo"),
        
        uiOutput("OvlpHelpInfo"),
        
        uiOutput("ZpHelpInfo")

        # textOutput("file1"),
        # textOutput("file2"),
        # textOutput("windowchk"),
        # textOutput("zpchk"),
        # textOutput("zpval")
        # textOutput("windowval"),
        # textOutput("sampchk"),
        # textOutput("samprateval"),
        # textOutput("speccheck"),
        # textOutput("specmin"),
        # textOutput("specmax")
        # uiOutput("audioplay")
    )
)
