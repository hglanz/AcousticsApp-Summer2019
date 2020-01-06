library(shiny)
library(plotly)
library(shinyalert)
library(shinyjs)
library(shinyBS)
library(DT)

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
                                                          h3("General Information:"),
                                                          
                                                          div(style="display:inline-block", uiOutput("mintimelimit")),
                                                          div(style="display:inline-block", uiOutput("maxtimelimit")),
                                                          br(),
                                                          div(style="display:inline-block", uiOutput("minfreqlimit")),
                                                          div(style="display:inline-block", uiOutput("maxfreqlimit")),
                                                          br(),
                                                          
                                                          
                                                          ## Spectrum
                                                          div(style="display:inline-block", uiOutput("spectrumcheck")),
                                                          div(style="display:inline-block", actionButton("specthelp", "", icon = icon("question-circle"))),
                                                          br(),
                                                          
                                                          div(style="display:inline-block", uiOutput("spectrummin")),
                                                          div(style="display:inline-block", uiOutput("spectrummax")),
                                                          br(),
                                                          
                                                          ## Sampling Rate
                                                          div(style="display:inline-block", uiOutput("sampcheck")),
                                                          div(style="display:inline-block", actionButton("samphelp", "", icon = icon("question-circle"))),
                                                          uiOutput("samplingrate"),
                                                          br(),
                                                          
                                                          ## Window Function
                                                          div(style="display:inline-block", uiOutput("windowcheck")),
                                                          div(style="display:inline-block", actionButton("windowhelp", "", icon = icon("question-circle"))),
                                                          uiOutput("window"),
                                                          br(),
                                                          
                                                          ## Overlapping
                                                          div(style="display:inline-block", uiOutput("ovlpcheck")),
                                                          div(style="display:inline-block", actionButton("ovlphelp", "", icon = icon("question-circle"))),
                                                          uiOutput("ovlp"),
                                                          br(),
                                                          
                                                          ## Zero Padding
                                                          div(style="display:inline-block", uiOutput("zpcheck")),
                                                          div(style="display:inline-block", actionButton("zphelp", "", icon = icon("question-circle"))),
                                                          uiOutput("zp")
                                         )
                            ),
                            mainPanel(width = 8,
                                      conditionalPanel("output.filechosen == true",
                                          tags$style(type="text/css",
                                                     ".shiny-output-error { visibility: hidden; }",
                                                     ".shiny-output-error:before { visibility: hidden; }"
                                          ),
                                          plotOutput("spectro",
                                                    width = "auto",
                                                    height = "100px"),
                                          plotlyOutput("plotly",  width = "auto", height = "600px"),
                                          
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
                            )
                   ),
                   tabPanel("Segmentation",
                            sidebarPanel(width = 4,
                                         h3("Segmentation"),
                                         br(),
                                         conditionalPanel("output.filechosen == true",
                                                          uiOutput("shortestSyl"),
                                                          div(style="display:inline-block",uiOutput("threshold")),
                                                          div(style="display:inline-block", actionButton("thresHelp", "", icon = icon("question-circle"))),
                                                          uiOutput("thresHelpInfo"),
                                                          actionButton("seghelp", "Segmentation Help")
                                                          )
                                         
                            ),
                            mainPanel(width = 8,
                                      conditionalPanel("output.filechosen == true",
                                                      ## Spectrogram 
                                                      plotOutput("spectro_seg"),
                                                      
                                                      ## Segmentation
                                                      plotOutput("segment"),
                                                      
                                                      ## Segments DataTable
                                                      div(style="display:inline-block", div("Segments")),
                                                      img(src = "segment.jpg", width = "5%", height = "5%"),
                                                      div(style="display:inline-block", 
                                                          actionButton("segmentsHelp", "", icon = icon("question-circle"))),
                                                      uiOutput("segmentsHelpInfo"),
                                                      div(DT::dataTableOutput("segments"), style = "font-size: 75%; width: 75%"),
                                                      br(),
                                                      downloadButton("downloadSegments", "Download Segments Data"),
                                                      br(),
                                                      
                                                      
                                                      ## Segmentation Help
                                                      uiOutput("SegHelpInfo")))),
                               
                   
            tabPanel("About", uiOutput("aboutInfo"))
))



