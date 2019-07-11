library(shiny)
library(plotly)
library(shinyalert)
library(shinyjs)
library(shinyBS)

shinyUI(navbarPage("Acoustic Analysis",
                   tabPanel("Exploration",
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
                                                          uiOutput("audioplay"),
                                                          
                                                          br(),
                                                          uiOutput("mintimelimit"),
                                                          uiOutput("maxtimelimit"),
                                                          uiOutput("minfreqlimit"),
                                                          uiOutput("maxfreqlimit"),
                                                          
                                                          ## Spectrum
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
                                      tags$style(type="text/css",
                                                 ".shiny-output-error { visibility: hidden; }",
                                                 ".shiny-output-error:before { visibility: hidden; }"
                                      ),
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
                                      
                            )
                   ),
                   tabPanel("Segmentaiton",
                            sidebarPanel(width = 3,
                                         h3("Segmentation"),
                                         br(),
                                         conditionalPanel("output.filechosen == true",
                                                          div(style="display:inline-block", uiOutput("minDurlimit")),
                                                          div(style="display:inline-block", 
                                                              actionButton("minDurhelp", "", icon = icon("question-circle"))),
                                                          uiOutput("MinDurHelpInfo"),
                                                          div(style="display:inline-block", uiOutput("maxDurlimit")),
                                                          div(style="display:inline-block", 
                                                              actionButton("maxDurhelp", "", icon = icon("question-circle"))),
                                                          uiOutput("MaxDurHelpInfo"),
                                                          uiOutput("minbplimit"),
                                                          uiOutput("maxbplimit"),
                                                          div(style="display:inline-block", 
                                                              actionButton("bphelp", "", icon = icon("question-circle"))),
                                                          uiOutput("bpHelpInfo"),
                                                          div(style="display:inline-block", uiOutput("threshold")),
                                                          div(style="display:inline-block", 
                                                              actionButton("threshelp", "", icon = icon("question-circle"))),
                                                          uiOutput("thresHelpInfo"))
                                         
                            ),
                            mainPanel(width = 9,
                                      imageOutput("segment", width = 30, height = 10))
)))



