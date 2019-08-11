library(shiny)
library(seewave)
library(tuneR)
library(plotly)
library(shinyalert)
library(grid)
library(filesstrings)
library(warbleR)
library(DT)
library(soundgen)
library(dplyr)
library(purrr)


seed = as.numeric(Sys.time())
load("DefaultWavFiles-ForApp.RData")
source("spectro_hg.R")
source("seg_functions.R")

shinyServer(function(input, output, session) {
    
#### Reactive Values ####
filevalues <- reactiveValues(
    file1 = NULL,
    file2 = 1
)

observeEvent(input$file1, {
    filevalues$file1 <- input$file1
})

observeEvent(input$file2, {
    filevalues$file2 <- input$file2
})

observeEvent(input$resetAll, {
    filevalues$file1 <- NULL
    filevalues$file2 <- 1
})


output$resettableInput <- renderUI({
    times <- input$resetAll
    div(id=letters[(times %% length(letters)) + 1],
        fileInput("file1", "Choose .wav File to Upload", accept = c(".wav")),
        selectInput("file2", label = "Or Choose a Preloaded .wav File",
                    choices = list("None" = 1,
                                   "Sine1" = 2,
                                   "Sine2" = 3,
                                   "Sine3" = 4,
                                   "Square" = 5,
                                   "Triangle" = 6,
                                   "Whale" = 7,
                                   "Dolphin" = 8,
                                   "Noise" = 9),
                    selected = 1))
    
})

output$filechosen <- reactive({
    return(!is.null(filevalues$file1) | filevalues$file2 != 1)
})
outputOptions(output, 'filechosen', suspendWhenHidden = FALSE)


#### UI & Input Elements ####
output$wavinfo <- renderUI({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile)) {
        wav <- readWave(inFile)
    } else {
        wav <- switch(filevalues$file2,
                      "2" = sine1,
                      "3" = sine2,
                      "4" = sine3,
                      "5" = square,
                      "6" = triangle,
                      "7" = whale,
                      "8" = dolphin,
                      "9" = noise)
    }
    
    if (exists("wav")) {
        sp <- spectro(wav, plot = F)
        
        maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
        if (length(maxfreq_pos) == 0) {
            maxf <- round(rev(sp$freq)[1], 2)
        } else {
            maxf <- round(min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq)), 2)
        }
        
        wavtext <- paste("<p>Sample Rate:", wav@samp.rate, " Hz</p>",
                         "<p>Duration of Signal: ", floor(length(wav@left)/wav@samp.rate*100)/100, " seconds</p>",
                         "<p>Maximum Freqency: ", maxf, " kHz</p>",
                         "<p>Spectrum Duration: ", floor(length(wav@left)/wav@samp.rate*100)/100, " seconds</p>",
                         "<p>Window Function: Hanning</p>",
                         "<p>Overlapping: 0%</p>",
                         "<p>Zero Padding: 0</p>")
        
        tags$span(
            popify(bsButton("wavinfo", "Hover Here for Native Info About .wav File"),
                   "",
                   wavtext)
        )
        
    }
})

output$mintimelimit <- renderUI({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile)) {
        wav <- readWave(inFile)
        
        numericInput("mintime", label = "Min Time (s):",
                     value = 0, min = 0, max = length(wav@left)/wav@samp.rate)
    } else {
        wav <- switch(filevalues$file2,
                      "2" = sine1,
                      "3" = sine2,
                      "4" = sine3,
                      "5" = square,
                      "6" = triangle,
                      "7" = whale,
                      "8" = dolphin,
                      "9" = noise)
        
        numericInput("mintime", label = "Min Time (s):",
                     value = 0, min = 0, max = length(wav@left)/wav@samp.rate)
    }
})

output$maxtimelimit <- renderUI({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile)) {
        wav <- readWave(inFile)
        
        numericInput("maxtime", label = "Max Time (s):",
                     value = round(length(wav@left)/wav@samp.rate,3), min = 0, max = length(wav@left)/wav@samp.rate)
    } else {
        wav <- switch(filevalues$file2,
                      "2" = sine1,
                      "3" = sine2,
                      "4" = sine3,
                      "5" = square,
                      "6" = triangle,
                      "7" = whale,
                      "8" = dolphin,
                      "9" = noise)
        
        numericInput("maxtime", label = "Max Time (s):",
                     value = round(length(wav@left)/wav@samp.rate,3), min = 0, max = length(wav@left)/wav@samp.rate)
    }
})


output$minfreqlimit <- renderUI({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile)) {
        wav <- readWave(inFile)
        
        sp <- spectro(wav, plot = F)
        
        maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
        if (length(maxfreq_pos) == 0) {
            numericInput("minfreq", label = "Min Frequency (kHz):",
                         value = 0, min = 0, max = rev(sp$freq)[1])
        } else {
            numericInput("minfreq", label = "Min Frequency (kHz):",
                         value = 0, min = 0, max = min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq)))
        }
    } else {
        wav <- switch(filevalues$file2,
                      "2" = sine1,
                      "3" = sine2,
                      "4" = sine3,
                      "5" = square,
                      "6" = triangle,
                      "7" = whale,
                      "8" = dolphin,
                      "9" = noise)
        
        sp <- spectro(wav, plot = F)
        
        maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
        if (length(maxfreq_pos) == 0) {
            numericInput("minfreq", label = "Min Frequency (kHz):",
                         value = 0, min = 0, max =  rev(sp$freq)[1])
        } else {
            numericInput("minfreq", label = "Min Frequency (kHz):",
                         value = 0, min = 0, max = min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq)))
        }
    }
})

output$maxfreqlimit <- renderUI({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile)) {
        wav <- readWave(inFile)
        
        sp <- spectro(wav, plot = F)
        
        maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
        if (length(maxfreq_pos) == 0) {
            numericInput("maxfreq", label = "Max Frequency (kHz):",
                         value = round(rev(sp$freq)[1]),3)
        } else {
            numericInput("maxfreq", label = "Max Frequency (kHz):",
                         value = round(min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq)),3))
        }
    } else {
        wav <- switch(filevalues$file2,
                      "2" = sine1,
                      "3" = sine2,
                      "4" = sine3,
                      "5" = square,
                      "6" = triangle,
                      "7" = whale,
                      "8" = dolphin,
                      "9" = noise)
        
        sp <- spectro(wav, plot = F)
        
        maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
        if (length(maxfreq_pos) == 0) {
            numericInput("maxfreq", label = "Max Frequency (kHz):",
                         value = round(rev(sp$freq)[1],3))
        } else {
            numericInput("maxfreq", label = "Max Frequency (kHz):",
                         value = round(min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq)),3))
        }
    }
})

output$sampcheck <- renderUI({
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile) | filevalues$file2 > 1) {
        checkboxInput("sampcheck", label = "Change Sampling Rate?",
                      value = F)
    }
})

output$samplingrate <- renderUI({
    
    if (!is.null(input$sampcheck)) {
        if (input$sampcheck) {
            inFile <- filevalues$file1$datapath
            ### Read .wav file in ###
            if (!is.null(inFile)) {
                wav <- readWave(inFile)
                
                selectInput("samprate", label = "Sampling Rate (Hz):",
                            choices = wav@samp.rate/(1:20),
                            selected = wav@samp.rate)
                
            } else {
                wav <- switch(filevalues$file2,
                              "2" = sine1,
                              "3" = sine2,
                              "4" = sine3,
                              "5" = square,
                              "6" = triangle,
                              "7" = whale,
                              "8" = dolphin,
                              "9" = noise)
                
                selectInput("samprate", label = "Sampling Rate (Hz):",
                            choices = wav@samp.rate/(1:20),
                            selected = wav@samp.rate)
            }
        }
    }
})


output$windowcheck <- renderUI({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile) | filevalues$file2 > 1) {
        checkboxInput("windowcheck", label = "Change Window Function?",
                      value = F)
    }
})

output$window <- renderUI({
    
    if (!is.null(input$windowcheck)) {
        if (input$windowcheck) {
            selectInput("window", label = "Choose a Window Function:",
                        choices = list("Rectangular" = 1,
                                       "Bartlett (Triangular)" = 2,
                                       "Hamming" = 3,
                                       "Hanning (Default)" = 4,
                                       "Blackman" = 5,
                                       "Flattop" = 6),
                        selected = 4)
        }
    }
})

output$zpcheck <- renderUI({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile) | filevalues$file2 > 1) {
        checkboxInput("zpcheck", label = "Choose Zero Padding?",
                      value = F)
    }
})

output$zp <- renderUI({
    
    if (!is.null(input$zpcheck)) {
        if (input$zpcheck) {
            selectInput("zp", label = "Choose Zero Padding:",
                        choices = c(0, 2^(1:17)),
                        selected = 0)
        }
    }
})

output$ovlpcheck <- renderUI({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile) | filevalues$file2 > 1) {
        checkboxInput("ovlpcheck", label = "Choose Overlapping?",
                      value = F)
    }
})

output$ovlp <- renderUI({
    
    if (!is.null(input$zpcheck)) {
        if (input$ovlpcheck) {
            sliderInput("ovlp", label = "Choose Overlapping (%):",
                        min = 0, max = 99, value = 0, step = 1)
        }
    }
})

output$spectrumcheck <- renderUI({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile) | filevalues$file2 > 1) {
        checkboxInput("spectrumcheck", label = "Change Spectrum Duration?",
                      value = F)
    }
})

output$spectrummin <- renderUI({
    
    if (!is.null(input$spectrumcheck)) {
        if (input$spectrumcheck) {
            inFile <- filevalues$file1$datapath
            
            if (!is.null(inFile)) {
                wav <- readWave(inFile)
                
                numericInput("specmin", label = "Spectrum Min Time (s):",
                             value = 0, min = 0, max = length(wav@left)/wav@samp.rate)
            } else {
                wav <- switch(filevalues$file2,
                              "2" = sine1,
                              "3" = sine2,
                              "4" = sine3,
                              "5" = square,
                              "6" = triangle,
                              "7" = whale,
                              "8" = dolphin,
                              "9" = noise)
                
                numericInput("specmin", label = "Spectrum Min Time (s):",
                             value = 0, min = 0, max = length(wav@left)/wav@samp.rate)
            }
        }
    }
})

output$spectrummax <- renderUI({
    
    if (!is.null(input$spectrumcheck)) {
        if (input$spectrumcheck) {
            inFile <- filevalues$file1$datapath
            
            if (!is.null(inFile)) {
                wav <- readWave(inFile)
                
                numericInput("specmax", label = "Spectrum Max Time (s):",
                             value = round(length(wav@left)/wav@samp.rate,3), min = 0, max = length(wav@left)/wav@samp.rate)
            } else {
                wav <- switch(filevalues$file2,
                              "2" = sine1,
                              "3" = sine2,
                              "4" = sine3,
                              "5" = square,
                              "6" = triangle,
                              "7" = whale,
                              "8" = dolphin,
                              "9" = noise)
                
                numericInput("specmax", label = "Spectrum Max Time (s):",
                             value = round(length(wav@left)/wav@samp.rate,3), min = 0, max = length(wav@left)/wav@samp.rate)
            }
        }
    }
})


#### Printed UI Outputs ####
output$file1 <- renderPrint(
    print(filevalues$file1)
)
output$file2 <- renderPrint(
    print(filevalues$file2)
)
output$windowchk <- renderPrint(
    print(input$windowcheck)
)
output$zpchk <- renderPrint(
    print(input$zpcheck)
)
output$zpval <- renderPrint(
    print(input$zp)
)
output$windowval <- renderPrint(
    print(input$window)
)
output$sampchk <- renderPrint(
    print(input$sampcheck)
)
output$samprateval <- renderPrint(
    print(input$samprate)
)
output$speccheck <- renderPrint(
    print(input$spectrumcheck)
)
output$specmin <- renderPrint(
    print(input$specmin)
)
output$specmax <- renderPrint(
    print(input$specmax)
)

#### Window Function Help ####
output$WindowHelpInfo <- renderText({
    times <- input$windowhelp
    if (times %% 2 == 1) {
        return("<p><b>Window Function Help</b></p>
               <p>In order to create a spectrum of an input signal (or
               some subset of the signal) the Discrete Fourier Transform (DFT)
               assumes that the captured input signal is periodic in nature
               with an integer number of periods.</p>
               <p>In reality, not all signals satisfy this assumption and thus
               when the captured input signal is repeated (to meet the criteria
               of periodicity for the DFT) many discontinuities will be present.
               The appearance of these discontinuities causes the energy at
               the frequency of the original signal to leak across other frequencies.
               In other words, the spectrum of a non-periodic signal where
               leakage has occured will display energy (amplitude) at frequencies
               that were not present in the original signal.</p>
               <p><img src='non_periodic.jpg' width= 60% height=60%></p>
               <p>Windowing a signal reduces DFT leakage. To avoid the sharp
               discontinuities that appear by taking a non-periodic signal
               and repeating it, a window function can be be applied to the signal
               in the time domain to create a tapering effect of amplitude at both
               the beginning and end of the sampling window. The windowed signal
               is then repeated with no discontinuities and the DFT is applied.</p>
               <p><img src='windowing.jpg' width= 100% height=100%></p>
               <p>There are several different types of windows that can be used
               to reduce spectral leakage. A few common ones and their spectrum
               can be seen below. The choice of which window to use is usually
               based on a trade off between side lobe effects (i.e. leakage)
               and main lobe width (i.e. loss in frequency resolution).</p>
               <p><img src='WindowFn-Pic.jpeg' width= 100% height=100%></p>
               <p>It is important to remember that the time and frequency resolution 
               of a spectrogram are inversely related and dependent on window 
               length (the number of samples used for the DFT). If the window 
               length is small there will be poor frequency resolution and good 
               time resolution. However, if the window length is large, although 
               the frequency resolution improves, the time resolution becomes 
               poor.</p>")
        
    }
})

#### Sampling Rate Help ####
output$SampHelpInfo <- renderText({
    times <- input$samphelp
    if (times %% 2 == 1) {
        return("<p><b>Sampling Rate Help</b></p>
               <p>Sampling rate (or sampling frequency), f<sub>s</sub>, is 
               the number of sampled points per second taken from a continuous
               signal to create a digital signal. It is typically measured in Hz (cycles per second).</p>
               <p>Perfect reconstruction or representation of a signal is possible
               when the sampling frequency is greater than twice the maximum
               frequency of the signal being sampled (Nyquist Principle).
               This will help avoid aliasing.</p>
               <p>Aliasing is a sampling effect that leads to signal frequencies
               being falsely interpreted as other frequencies. Consider sampling
               of a 7 kHz sine wave. As can be seen in the image, the sample
               values would not change at all if, instead, we were sampling a 1 kHz
               sine wave. The continuous 7 kHz sinusoid is aliased by a 1 kHz
               digitized wave. This will result in a misrepresentation of the frequency
               content of the signal.</p>
               <p><img src='SamplingRateHelpPic.jpeg' width= 80% height=80%></p>")
        
    }
})

#### Spectrum Help ####
output$SpecHelpInfo <- renderText({
    times <- input$specthelp
    if (times %% 2 == 1) {
        return("<p><b>Spectrum Help</b></p>
               <p>The frequency spectrum of a signal is the presentation
               of the signal in the frequency domain based on the Discrete Fourier
               Transform (DFT) of it's time domain function.</p>
               <p>Any signal is comprised of a number of sinusiodal signals
               with varied amplitude, frequency, and phase. The spectrum displays
               the distribution of amplitudes of each frequncy component within
               a specified portion of the signal.</p>")
        
    }
})

#### Overlapping Help ####
output$OvlpHelpInfo <- renderText({
    times <- input$ovlphelp
    if (times %% 2 == 1) {
        return("<p><b>Overlapping Help</b></p>
               <p>The time and frequency resolution of a spectrogram are inversely 
               related and dependent on window length (the number of samples 
               used for the DFT). If the window length is small there will 
               be poor frequency resolution and good time resolution. However, 
               if the window length is large, although the frequency resolution 
               improves, the time resolution becomes poor.</p>
               <p>One way to manage this and to increase the time resolution without 
               reducing the frequency resolution is to apply an overlap of 
               succesive windows. By overlapping windows and summing the amplitudes
               the tappering of the amplitude at the ends of the windowed signal 
               will be cancelled out by the addition of the next successive 
               window. This result is a better approximation of the orginal 
               signal without the negative impacts of spectral leakage.</p>
               <p>As can be seen below, three successive Hanning windows with a 
               50% overlap can be summed together in order to create a more 
               appropriate representation of the input signal. Unfortunately, 
               while useful, the overlap solution also increases the number of
               DFTs to compute by a factor of <b>100/(100-overlap)</b>. This can 
               take a lot of computing power.</p>
               <p><img src='OverlappingHelpPic.jpeg' width= 70% height=70%></p>")
        
    }
})

#### Zero Padding Help ####
output$ZpHelpInfo <- renderText({
    times <- input$zphelp
    if (times %% 2 == 1) {
        return("<p><b>Zero Padding Help</b></p>
               <p>The time and frequency resolution of a spectrogram are inversely 
               related and dependent on window length (the number of samples 
               used for the DFT). If the window length is small there will 
               be poor frequency resolution and good time resolution. However, 
               if the window length is large, although the frequency resolution 
               improves, the time resolution becomes poor.</p>
               <p>One way to manage this and to increase the frequency resolution
               without losing time resolution is to extend a signal (artificially)
               by adding zeros to the end of the signal in the time domain. 
               Although this zero-padding process will not change the result of 
               the DFT, it will increase the density of samples producing 
               a smoother looking spectrum when plotted without further interpolation.</p>")
        
    }
})

#### Spectrogram ####
output$spectro <- renderPlot({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile)) {
        
        wav <- readWave(inFile)
        sp <- spectro(wav, plot = F)
        
        filetitle <- filevalues$file1$name
        
    } else if (filevalues$file2 != 1) {
        
        ### Assign wav File & Title ###
        wav <- switch(filevalues$file2,
                      "2" = sine1,
                      "3" = sine2,
                      "4" = sine3,
                      "5" = square,
                      "6" = triangle,
                      "7" = whale,
                      "8" = dolphin,
                      "9" = noise)
        sp <- spectro(wav, plot = F)
        filetitle <- switch(filevalues$file2,
                            "2" = "Sine1",
                            "3" = "Sine2",
                            "4" = "Sine3",
                            "5" = "Square",
                            "6" = "Triangle",
                            "7" = "Whale",
                            "8" = "Dolphin",
                            "9" = "Noise")
    }
    
    if (exists("wav")) {
        ### Determine Window Function ###
        if (!is.null(input$windowcheck)) {
            if (input$windowcheck) {
                if (!is.null(input$window)) {
                    window_choice <- switch(input$window,
                                            "1" = "rectangle",
                                            "2" = "bartlett",
                                            "3" = "hamming",
                                            "4" = "hanning",
                                            "5" = "blackman",
                                            "6" = "flattop")
                } else {
                    window_choice <- "hanning"
                }
            } else {
                window_choice <- "hanning"
            }
        } else {
            window_choice <- "hanning"
        }
        
        ### Determine Zero Padding ###
        if (!is.null(input$zpcheck)) {
            if (input$zpcheck) {
                if (!is.null(input$zp)) {
                    zp_choice <- as.numeric(input$zp)
                } else {
                    zp_choice <- 0
                }
            } else {
                zp_choice <- 0
            }
        } else {
            zp_choice <- 0
        }
        
        
        ### Determine Overlapping ###
        if (!is.null(input$ovlpcheck)) {
            if (input$ovlpcheck) {
                if (!is.null(input$ovlp)) {
                    ovlp_choice <- as.numeric(input$ovlp)
                } else {
                    ovlp_choice <- 0
                }
            } else {
                ovlp_choice <- 0
            }
        } else {
            ovlp_choice <- 0
        }
        
        ### Determine Spectrum Duration ###
        if (!is.null(input$spectrumcheck)) {
            if (input$spectrumcheck) {
                if (!is.null(input$specmin)) {
                    specmin_choice <- input$specmin
                    specmax_choice <- input$specmax
                } else {
                    specmin_choice <- 0
                    specmax_choice <- length(wav@left)/wav@samp.rate
                }
            } else {
                specmin_choice <- 0
                specmax_choice <- length(wav@left)/wav@samp.rate
            }
        } else {
            specmin_choice <- 0
            specmax_choice <- length(wav@left)/wav@samp.rate
        }
        
        ### Determine Sampling Rate ###
        if (!is.null(input$sampcheck)) {
            if (input$sampcheck) {
                if (!is.null(input$samprate)) {
                    samprate_choice <- as.numeric(input$samprate)
                } else {
                    samprate_choice <- wav@samp.rate
                }
            } else {
                samprate_choice <- wav@samp.rate
            }
        } else {
            samprate_choice <- wav@samp.rate
        }
        
        ### Determine Time Limits ###
        if (!is.null(input$mintime) & !is.null(input$maxtime)) {
            mintime_choice <- input$mintime
            maxtime_choice <- input$maxtime
        } else {
            mintime_choice <- 0
            maxtime_choice <- length(wav@left)/wav@samp.rate
        }
        
        ### Determine Freq Limits ###
        if (!is.null(input$minfreq) & !is.null(input$maxfreq)) {
            minfreq_choice <- input$minfreq
            maxfreq_choice <- input$maxfreq
        } else {
            minfreq_choice <- 0
            
            maxfreq_pos <- which(diff(rev(apply(sp$amp, 1, max) <= -29)) < 0)[1]
            if (length(maxfreq_pos) == 0) {
                maxfreq_choice <- rev(sp$freq)[1]
            } else {
                maxfreq_choice <- min(rev(sp$freq)[maxfreq_pos]+.5, max(sp$freq))
            }
            
        }
        
        ### Plot Spectrogram ###
        myinput <- inputw(wave = wav, f = samprate_choice)
        wave <- myinput$w
        f <- myinput$f
        wave <- cutw(wave, f = f, from = mintime_choice, to = maxtime_choice)
        wl <- 512
        ovlp <- ovlp_choice
        wn <- window_choice
        zp <- zp_choice
        fftw <- FALSE
        norm <- TRUE
        complex <- FALSE
        correction <- "none"
        palette <- spectro.colors
        scalefontlab <- 1
        scalelab <- "Amplitude\n(dB)"
        scalecexlab <- 0.85
        collab <- "black"
        colaxis <- "black"
        rm(myinput)
        
        layout(matrix(c(1,0,2,3,4,0),ncol = 2, byrow = T),widths=c(3,.8), heights = c(.65, 4, 1.5))
        
        ## Amplitude Gradient Legend ##
        n <- nrow(wave)
        step <- seq(1, n + 1 - wl, wl - (ovlp * wl/100))
        z <- stdft(wave = wave, f = samprate_choice, wl = wl, zp = zp, step = step, 
                   wn = wn, fftw = fftw, scale = norm, complex = complex, 
                   correction = correction)
        fl1 <- minfreq_choice * nrow(z) * 2000/f
        fl2 <- maxfreq_choice * nrow(z) * 2000/f
        z <- z[(fl1:fl2) + 1, ]
        z <- 20 * log10(z)
        maxz <- round(max(z, na.rm = TRUE))
        collevels <- seq(maxz - 30, maxz, by = 1)
        par(mar=c(2,4,4,0.5))
        dBscale(collevels = collevels, palette = palette, 
                fontlab = scalefontlab, cexlab = scalecexlab, 
                collab = collab, textlab = scalelab, colaxis = colaxis, side = 1)
        
        ## Spectrogram ##
        par(mar=c(4,4,2,0.5))
        spectro(wav,
                tlab = "",
                # xaxt = "n",
                f = samprate_choice,
                tlim = c(input$mintime, input$maxtime),
                flim = c(input$minfreq, input$maxfreq),
                main = paste("Spectrogram of", filetitle),
                font.main = 1,
                cex.main= 1.7,
                cex.lab=1.3,
                wn = window_choice,
                zp = zp_choice,
                ovlp = ovlp_choice,
                scale = FALSE)
        abline(v = specmin_choice, col = "red", lty = 2, lwd=3)
        abline(v = specmax_choice, col = "red", lty = 2, lwd=3)
        
        ## Spectrum ##
        par(mar=c(4,1,2,0.5))
        specvals <- spec(wav,
                         f = samprate_choice,
                         col = "red",
                         lwd=3,
                         plot = 2,
                         wn = window_choice,
                         flab = "", yaxt = "n",
                         from = specmin_choice,
                         to = specmax_choice,
                         flim = c(input$minfreq, input$maxfreq),
                         dB = "max0",
                         xaxt = "n",
                         main = "Spectrum",
                         alab = "Amplitude (dB)",
                         cex.axis = 1.5
        )
        spectcks <- seq(from = round(min(specvals[,2])), to = 0, by = 5)
        axis(1, at = spectcks, labels = spectcks, tck = -.025, pos = input$minfreq)
        axis(side = 1, at = c(0), tck = -.025)
        #abline(h = input$maxfreq, col = "blue", lty = 2, lwd=3)
        # text(median(spectcks), -.05, "Amplitude (dB)", cex = 1.5)
        
        ## Oscillogram ##
        par(mar=c(4,4,2,0.5))
        oscillo(wav, 
                f = samprate_choice,
                from = input$mintime,
                to = input$maxtime,
                cexlab = 0.87)
        abline(v = specmin_choice, col = "red", lty = 2, lwd=3)
        abline(v = specmax_choice, col = "red", lty = 2, lwd=3)
        
        #### Segmentation ####
        output$segment <- renderPlot({
            
            inFile <- filevalues$file1$datapath
            ### Read .wav file in ###
            if (!is.null(inFile)) {
                
                wav <- readWave(inFile)
                sp <- spectro(wav, plot = F)
                
                filetitle <- filevalues$file1$name
                
            } else if (filevalues$file2 != 1) {
                
                ### Assign wav File & Title ###
                wav <- switch(filevalues$file2,
                              "2" = sine1,
                              "3" = sine2,
                              "4" = sine3,
                              "5" = square,
                              "6" = triangle,
                              "7" = whale,
                              "8" = dolphin,
                              "9" = noise)
                sp <- spectro(wav, plot = F)
                filetitle <- switch(filevalues$file2,
                                    "2" = "Sine1",
                                    "3" = "Sine2",
                                    "4" = "Sine3",
                                    "5" = "Square",
                                    "6" = "Triangle",
                                    "7" = "Whale",
                                    "8" = "Dolphin",
                                    "9" = "Noise")
            }
            
            if (exists("wav")) {
                ### Determine Sampling Rate ###
                if (!is.null(input$samprate)) {
                    samprate_choice <- as.numeric(input$samprate)
                } else {
                    samprate_choice <- wav@samp.rate
                    
                    ### Determine Shortest Segment ###
                    if (!is.null(input$shortestSyl)) {
                        shortestSyl_choice <- input$shortestSyl
                        
                    } else {
                        shortestSyl_choice <- 40
                        
                    }
                    
                    ### Determine Threshold Limits ###
                    if (!is.null(input$threshold)) {
                        threshold_choice <- input$threshold
                    } else {
                        threshold_choice <- 0.9
                    }
                    
                    ### Plot Segmentation ###
                    savewav(wav, filename = "tempFile.wav")
                    segment <- segment("tempFile.wav", 
                                       plot = TRUE, 
                                       main = filetitle,
                                       shortestSyl = shortestSyl_choice,
                                       sylThres = threshold_choice,
                                       xlab = "Time (ms)")
                    segments <- segment$syllables
                    segments <- round(segments %>% 
                                          rowwise() %>% 
                                          mutate("Min Frequency" = freq_range("tempFile.wav", start/1000, end/1000)[1,1],
                                                 "Max Frequency" = freq_range("tempFile.wav", start/1000, end/1000)[2,1],
                                                 "Peak Frequency" = freq_range("tempFile.wav", start/1000, end/1000)[3,1])%>%
                                          rename(Segment = syllable, "Start (ms)" = start,
                                                 "End (ms)" = end, "Segment Duration (ms)" = sylLen) %>%
                                          select(-pauseLen),3)
                    
                    
                    output$segments <- DT::renderDataTable({
                        segments},
                        rownames = FALSE,
                        options = list(pageLength = 5))
                    
                    output$spectro_seg <- renderPlot({
                        input_cols <- input$segments_rows_selected
                        spectro(wav,
                                tlab = "",
                                f = samprate_choice,
                                tlim = c(input$mintime, input$maxtime),
                                flim = c(input$minfreq, input$maxfreq),
                                main = paste("Spectrogram of", filetitle),
                                font.main = 1,
                                cex.main= 1.7,
                                cex.lab=1.3,
                                wn = window_choice,
                                zp = zp_choice,
                                ovlp = ovlp_choice,
                                scale = FALSE)
                        plot.title = title(main = "", xlab = "Time (s)",
                                            ylab = "Frequency (kHz)")
                    
                        segment_box(segments, input_cols)
                    })
                    
                    # Downloadable csv of selected dataset
                    output$downloadSegments <- downloadHandler(
                        filename <- function() {
                            paste(filetitle, "-segments", ".csv", sep="")
                        },
                        content <- function(file) {
                            write.csv(segments, file)
                        }
                    )
                    
                }
            }})
        
        
        
    }
})

output$audioplay <- renderUI({
    inFile <- filevalues$file1
    if (!is.null(inFile$datapath) & input$file2 == 1) {
        filetitle <- filevalues$file1$name
        wav <- readWave(inFile$datapath)
        
        ### Determine Time Limits ###
        if (!is.null(input$mintime) & !is.null(input$maxtime)) {
            mintime_choice <- input$mintime
            maxtime_choice <- input$maxtime
        } else {
            mintime_choice <- 0
            maxtime_choice <- length(wav@left)/wav@samp.rate
        }
        savewav(wav, filename = "www/tempFile.wav")
        cut_wav <- readWave("www/tempFile.wav", from = mintime_choice, to = maxtime_choice, units = "seconds")
        file.remove("www/tempFile.wav")
        savewav(cut_wav, filename = "www/tempFile.wav")
        h3("Audio Playback",
           br(),
           tags$small(paste("Start:", round(mintime_choice,3))),
           tags$small(paste("End:", round(maxtime_choice,3))),
           tags$audio(src = "tempFile.wav", 
                      type = "audio/wav", 
                      controls = "controls"))
            
    } else if (input$file2 != 1) {
        wav <- switch(input$file2,
                          "2" = sine1,
                          "3" = sine2,
                          "4" = sine3,
                          "5" = square,
                          "6" = triangle,
                          "7" = whale,
                          "8" = dolphin,
                          "9" = noise)
        
        ### Determine Time Limits ###
        if (!is.null(input$mintime) & !is.null(input$maxtime)) {
            mintime_choice <- input$mintime
            maxtime_choice <- input$maxtime
        } else {
            mintime_choice <- 0
            maxtime_choice <- length(wav@left)/wav@samp.rate
        }
        savewav(wav, filename = "www/tempFile.wav")
        cut_wav <- readWave("www/tempFile.wav", from = mintime_choice, to = maxtime_choice, units = "seconds")
        file.remove("www/tempFile.wav")
        savewav(cut_wav, filename = "www/tempFile.wav")
        h3("Audio Playback",
           br(),
           tags$small(paste("Start:", round(mintime_choice,3))),
           tags$small(paste("End:", round(maxtime_choice,3))),
           tags$audio(src = "tempFile.wav", 
                      type = "audio/wav", 
                      controls = "controls"))
    }
    })
    

output$shortestSyl <- renderUI({
    
    inFile <- filevalues$file1$datapath
    ### Read .wav file in ###
    if (!is.null(inFile)) {
        wav <- readWave(inFile)
        
        numericInput("shortestSyl", label = "Min Length of Segments (ms):",
                     value = 40, min = 0, max = length(wav@left)/wav@samp.rate*1000)
    } else {
        wav <- switch(filevalues$file2,
                      "2" = sine1,
                      "3" = sine2,
                      "4" = sine3,
                      "5" = square,
                      "6" = triangle,
                      "7" = whale,
                      "8" = dolphin,
                      "9" = noise)
        
        numericInput("shortestSyl", label = "Min Length of Segments (ms):",
                     value = 40, min = 0, max = length(wav@left)/wav@samp.rate*1000)
    }
})


output$threshold <- renderUI({
        inFile <- filevalues$file1$datapath
        ### Read .wav file in ###
        if (!is.null(inFile)) {
            wav <- readWave(inFile)
            
            numericInput("threshold", label = "Amplitude Threshold (proportion):",
                         value = 0.9, max = 1, step = 0.1)
            
        } else {
            wav <- switch(filevalues$file2,
                          "2" = sine1,
                          "3" = sine2,
                          "4" = sine3,
                          "5" = square,
                          "6" = triangle,
                          "7" = whale,
                          "8" = dolphin,
                          "9" = noise)
            
            numericInput("threshold", label = "Amplitude Threshold: (proportion)",
                         value = 0.9,max = 1, step = 0.1)
        }
})


output$segmentsHelpInfo <- renderText({
    times <- input$segmentsHelp
    if (times %% 2 == 1) {
        return("<p>Segments are defined as continuous 
               fragments with amplitude above a given threshold, 
               seperated by silence (or background noise).</p>")
        
    }
})


output$thresHelpInfo <- renderText({
    times <- input$thresHelp
    if (times %% 2 == 1) {
        return("<p>Amplitude threshold for segment detection 
               as a proportion of global mean amplitude of smoothed envelope.</p>")
        
    }
})

#### Segmentation Help ####
output$SegHelpInfo <- renderText({
    times <- input$seghelp
    if (times %% 2 == 1) {
        return("<p><b>Segmentation Help</b></p>
                <p>The following tab divides a sound file into separate segments 
                - continuous acoustic fragments separated by what we consider to be “silence”. 
                This “silence” can often contain background noise. The function finds bursts of acoustic energy - local maxima in amplitude envelope 
                that are high enough both in absolute terms (relative to the global maximum) and 
                with respect to the surrounding region (relative to local mimima)
                which are denoted by the red stars. 
                The function segment() used here, looks for both segments and bursts. Segments are found first,
                and then the median length of a segment becomes the expected peak interval, 
                guiding burst detection. The method operates with amplitude 
                envelopes - smoothed contours of sound intensity. 
                See vignette('acoustic_analysis', package = 'soundgen') for more details.</p>")
    
    }
})


    
    

# delete temporary files created
session$onSessionEnded(function() {
    if (file.exists("www/tempFile.wav")) 
        file.remove("www/tempFile.wav")
    if (file.exists("tempFile.wav")) 
        file.remove("tempFile.wav")
    })
})