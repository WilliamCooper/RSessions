#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

print (getwd())
options (stringsAsFactors=FALSE)
library(shiny)
suppressMessages (suppressWarnings (
  library(Ranadu, quietly=TRUE, warn.conflicts=FALSE))
)
formatTime <- function (time) {
  t <- as.POSIXlt (time, tz='UTC')
  tt <- sprintf ("%d:%02d:%02d", t$hour, t$min, t$sec)
  return (tt)
}
pageN <- data.frame(session=3, low=1, max=3, start=2)
pageN <- rbind(pageN, data.frame(session=3, low=4, max=14, start=4))
pageN <- rbind(pageN, data.frame(session=3, low=15, max=18, start=15))
pageN <- rbind(pageN, data.frame(session=3, low=19, max=24, start=19))
pageN <- rbind(pageN, data.frame(session=4, low=1, max=11, start=2))
pageN <- rbind(pageN, data.frame(session=4, low=12, max=31, start=12))
pageN <- rbind(pageN, data.frame(session=5, low=1, max=2, start=2))
pageN <- rbind(pageN, data.frame(session=5, low=3, max=7, start=3))
pageN <- rbind(pageN, data.frame(session=5, low=8, max=9, start=8))
pageN <- rbind(pageN, data.frame(session=5, low=10, max=15, start=10))
pageN <- rbind(pageN, data.frame(session=5, low=16, max=24, start=16))
pageN <- rbind(pageN, data.frame(session=6, low=1, max=2, start=2))
pageN <- rbind(pageN, data.frame(session=6, low=3, max=10, start=3))
pageN <- rbind(pageN, data.frame(session=6, low=11, max=16, start=11))
pageN <- rbind(pageN, data.frame(session=6, low=17, max=18, start=17))
pageN <- rbind(pageN, data.frame(session=6, low=19, max=22, start=19))
pageN <- rbind(pageN, data.frame(session=7, low=1, max=3, start=2))
pageN <- rbind(pageN, data.frame(session=7, low=4, max=9, start=4))
pageN <- rbind(pageN, data.frame(session=7, low=10, max=14, start=10))
pageN <- rbind(pageN, data.frame(session=8, low=1, max=4, start=2))
pageN <- rbind(pageN, data.frame(session=8, low=5, max=9, start=5))
pageN <- rbind(pageN, data.frame(session=8, low=10, max=13, start=10))
pageN <- rbind(pageN, data.frame(session=8, low=14, max=14, start=14))
pageN <- rbind(pageN, data.frame(session=8, low=15, max=17, start=15))
pageN <- rbind(pageN, data.frame(session=9, low=1, max=5, start=2))

ui <- fluidPage(
  
  # Application title
  titlePanel("R Sessions"),
  tabsetPanel (id='whichTab', type='pills',
    tabPanel ('TOC',
      includeHTML('TOC/TOC.html')),
    tabPanel ('1. Introduction',
      tabsetPanel (id='S1tab', type='pills',
        tabPanel ('Getting Started',
          includeHTML('./Session1/Session1a.html')),
        tabPanel ('RStudio Tour',
          includeHTML('./Session1/Session1b.html')),
        tabPanel ('Some Examples',
          includeHTML('./Session1/Session1c.html'),
          tabsetPanel (id='S1ex', type='pills',
            tabPanel ('simple plot',
              includeHTML ('./Session1/E1Code.html'),
              sidebarLayout(
                sidebarPanel(
                  selectInput (inputId='S1Var', label='variable to plot', 
                    choices=c('Temperature'='ATX',
                      'Wind Speed'='WSC',
                      'Pressure'='PSXC'))
                ),
                mainPanel(
                  plotOutput ('S1E1Plot')
                )
              )),
            tabPanel ('sounding',
              includeHTML ('Session1/E2Code.html'),
              plotOutput ('S1E2Plot', width="50%")),
            tabPanel ('stats',
              includeHTML ('Session1/E3Code.html'),
              dataTableOutput ('S1Stats')),
            tabPanel ('recovery factor',
              includeHTML ('Session1/E4Code.html'))
          )
        ),
        tabPanel ('Text-with-Code',
          includeHTML ('Session1/Session1d.html'),
          radioButtons ('Contrast', 'show in window below:', choices=c('none', 'PDF document', 'source code')),
          # pre(includeText ('Session1/CONTRASTcalAOA.Rnw')),
          htmlOutput('pdfviewer'),
          htmlOutput('RnwViewer')
          ),
        tabPanel ('Getting Ranadu',
          includeHTML ('Session1/Session1e.html')))),
    tabPanel ('2. Objects and the data.frame',
      tabsetPanel (id='S2tab', type='pills',
        tabPanel ('Vectors and Matrices',
          includeHTML('~/RStudio/RSessions/RSessions/Session2/Session2a.html'),
          htmlOutput('txtS2a'),
          radioButtons ('selS2a', label=NULL, choices=c(
            'select a button below'=1,
            'a <- 1:12; print(a)'=2,
            'dim(a) <- c(3,4); print(a)'=3,
            'print (t(a))'=4
          ), width='400px')
        ),
        tabPanel ('The data.frame',
          includeHTML('~/RStudio/RSessions/RSessions/Session2/Session2b.html')),
        tabPanel ('Addressing and Subsetting data.frames',
          includeHTML('~/RStudio/RSessions/RSessions/Session2/Session2c1.html'),
          radioButtons ('selS2c1', label=NULL, choices=c(
            'select a button below'=1,
            'Data$ATX[5]'=2,
            'Data[5, 2]'=3,
            'Data[5, ]'=4,
            'Data[5, "ATX"]'=5,
            'Data$ATX'=6,
            'attach(Data); ATX[5]'=7,
            'with(Data, print(ATX[5])'=8
          ), width='800px'),
          htmlOutput ('txtS2c1'),
          includeHTML('~/RStudio/RSessions/RSessions/Session2/Session2c2.html'),
          actionButton ('XS2a', label='See an answer')
        ),
        tabPanel ('Some Basic Operations',
          includeHTML ('~/RStudio/RSessions/RSessions/Session2/Session2d.html')))),
    tabPanel ('3. Basics',
      tabsetPanel (id='S3tab', type='pills',
        tabPanel ('R as a Calculator'),
        tabPanel ('Basic Operators and Precedence'),
        tabPanel ('Vectorized Operations'),
        tabPanel ('Using Variables')
      ),
      fluidRow (
        column (1, numericInput ('S3frame', label='frame #',
          pageN$start[1], min=pageN$low[1], max=pageN$max[4], step=1)),
        column (1, actionButton('S3Back', label='previous')),
        column (1, actionButton('S3Next', label='next')),
        column (2, downloadButton('S3PDF', label='download PDF for Tab 3'))
      ),
      # imageOutput('RS3png')
      htmlOutput('RS3png', inline=TRUE)
    ),
    tabPanel ('4. R packages',
      tabsetPanel (id='S4tab', type='pills',
        tabPanel ('General Info re Packages'),
        tabPanel ('Ranadu')
      ),
      fluidRow (
        column (1, numericInput ('S4frame', label='frame #',
          pageN$start[5], min=pageN$low[5], max=pageN$max[6], step=1)),
        column (1, actionButton('S4Back', label='previous')),
        column (1, actionButton('S4Next', label='next')),
        column (2, downloadButton('S4PDF', label='download PDF for Tab 4'))
      ),
      htmlOutput('RS4png', inline=TRUE)
    ),
    tabPanel ('5. Plotting',
      tabsetPanel (id='S5tab', type='pills',
        tabPanel ('Overview'),
        tabPanel ('Base Graphics'),
        tabPanel ('plotWAC Plots'),
        tabPanel ('ggplot Graphics'),
        tabPanel ('Other Examples')
      ),
      fluidRow (
        column (1, numericInput ('S5frame', label='frame #',
          pageN$start[7], min=pageN$low[7], max=pageN$max[11], step=1)),
        column (1, actionButton('S5Back', label='previous')),
        column (1, actionButton('S5Next', label='next')),
        column (2, downloadButton('S5PDF', label='download PDF for Tab 5'))
      ),
      htmlOutput('RS5png', inline=TRUE)
    ),
    tabPanel ('6. Fitting',
      tabsetPanel (id='S6tab', type='pills',
        tabPanel ('Overview'),
        tabPanel ('Linear Fits'),
        tabPanel ('Regression and the Deming Fit'),
        tabPanel ('Nonlinear Fits'),
        tabPanel ('Maximum Likelihood')
      ),
      fluidRow (
        column (1, numericInput ('S6frame', label='frame #',
          pageN$start[12], min=pageN$low[12], max=pageN$max[16], step=1)),
        column (1, actionButton('S6Back', label='previous')),
        column (1, actionButton('S6Next', label='next')),
        column (2, downloadButton('S6PDF', label='download PDF for Tab 6'))
      ),
      htmlOutput('RS6png', inline=TRUE)
    ),
    tabPanel ('7. Reproducible Research',
      tabsetPanel (id='S7tab', type='pills', 
        tabPanel ('Overview'),
        tabPanel ('Using knitr'),
        tabPanel ('An Example')
      ),
      fluidRow (
        column (1, numericInput ('S7frame', label='frame #',
          pageN$start[17], min=pageN$low[17], max=pageN$max[19], step=1)),
        column (1, actionButton('S7Back', label='previous')),
        column (1, actionButton('S7Next', label='next')),
        column (2, downloadButton('S7PDF', label='download PDF for Tab 7'))
      ),
      htmlOutput('RS7png', inline=TRUE)
    ),
    tabPanel ('8. RAF Special Topics',
      tabsetPanel (id='S8tab', type='pills',
        tabPanel ('Data Review'),
        tabPanel ('Ranadu data.frames'),
        tabPanel ('Operations on netCDF Files'),
        tabPanel ('Ranadu Processing Functions'),
        tabPanel ('Hints and Suggestions')
      ),
      fluidRow (
        column (1, numericInput ('S8frame', label='frame #',
          pageN$start[20], min=pageN$low[20], max=pageN$max[24], step=1)),
        column (1, actionButton('S8Back', label='previous')),
        column (1, actionButton('S8Next', label='next')),
        column (2, downloadButton('S8PDF', label='download PDF for Tab 8'))
      ),
      htmlOutput('RS8png', inline=TRUE)
    ),
    tabPanel ('9. Shiny apps',
      fluidRow (
        column (1, numericInput ('S9frame', label='frame #',
          pageN$start[25], min=pageN$low[25], max=pageN$max[25], step=1)),
        column (1, actionButton('S9Back', label='previous')),
        column (1, actionButton('S9Next', label='next')),
        column (2, downloadButton('S9PDF', label='download PDF for Tab 9'))
      ),
      htmlOutput('RS9png', inline=TRUE)
    ),
    tabPanel ('10. Variance Spectra',
      includeHTML('HTML/VarSpec.html')
    )
  )
  
)

server <- function(input, output, session) {
  
  output$S1E1Plot <- renderPlot ({
    V <- input$S1Var
    nm <- c('Temperature [deg. C]', 'Wind Speed [m/s]', 'Pressure [hPa]')
    names (nm) <- c('ATX', 'WSC', 'PSXC')    ## these are the actual variables in the data file
    # Data <- getNetCDF('/Data/DEEPWAVE/DEEPWAVErf20.nc', c('ATX', 'WSC', 'PSXC'))
    load ('Session1/Data.Rdata')
    plot (Data$Time, Data[, V], type='l', col='blue', lwd=2, xlab='Time [UTC]', ylab=nm[V])
    title ("DEEPWAVE flight 20")
    # with (Data, plotWAC (data.frame (Time, Data[, V]), ylab=nm[V]))
  })
  
  output$S1E2Plot <- renderPlot ({
    # Directory <- DataDirectory ()    # for portability; sets the local data directory
    # Flight <- "rf20"                 # select a flight
    # Project = "DEEPWAVE"             # select a project
    # fname = sprintf("%s%s/%s%s.nc", Directory,Project,Project,Flight)
    # # XXX set variables needed, here a standard list including DPX and EWX
    # # preliminary look shows that final descent was from 84400 to 91100
    # Data <- getNetCDF (fname, c("Time", "DPXC", "ATX", "PALT"), 84400, 91100)
    saveDataFile <- 'Session1/Data2.RData'
    # save (Data, file = saveDataFile) 
    # for future runs, it will be much faster to use:
    load(saveDataFile)
    plot (Data$DPXC, Data$PALT, type='l', lwd=1.5, # type='l': line plot
      xlab='Temperature or Dew Point [deg C]', ylab='pressure altitude [m]')   
    lines (Data$ATX, Data$PALT, col='forestgreen', lwd=2) # add temperature
    s <- Data$DPXC > Data$ATX
    lines (Data$DPXC[s], Data$PALT[s], col='red', lwd=3)
    # will show how to add legends, titles, axis labels, etc, later
    
  })
  
  output$S1Stats <- renderDataTable ({
    Dstats <- data.frame()
    VarList <- c('WIC', 'ATX', 'DPXC', 'PSXC', 'GGALT', 'PALT')
    Ds <- getNetCDF ('/Data/DEEPWAVE/DEEPWAVErf20.nc', VarList)
    ## FL400 means pressure altitude of 40000 ft
    Ds <- Ds[Ds$PALT/0.3048 > 40000, ]  ## select only points above 40000 ft
    save (Ds, file='Session1/Data3.Rdata')
    load ('Session1/Data3.Rdata')
    Dstats['Time', 1] <- 'Time'
    Dstats['Time', 2] <- NA
    Dstats['Time', 3] <- NA
    Dstats['Time', 4] <- formatTime (Ds$Time[1])
    Dstats['Time', 5] <- formatTime (Ds$Time[nrow(Ds)])
    for (nm in names(Ds)) {
      if (nm == 'Time') {next}
      Dstats[nm, 1] <- nm
      Dstats[nm, 2] <- mean (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 3]   <- sd   (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 4]  <- min  (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 5]  <- max  (Ds[, nm], na.rm=TRUE)
    }
    names(Dstats) <- c('variable', 'mean', 'sd', 'min', 'max')
    row.names (Dstats) <- names(Ds)
    for (k in 2:5) {
      Dstats[2:nrow(Dstats), k] <- sprintf('%.3f', as.numeric(Dstats[2:nrow(Dstats), k]))
    }
    Dstats
  })
  
  # output$RS3png <- renderImage ({
  #   list(src = sprintf('~/RStudio/RSessions/RSessions/Session3/S%02d.png', input$S3frame),
  #     contentType = 'image/png',
  #     width = 1000,
  #     height = 700,
  #     alt = "RSessions image goes here")
  # }, deleteFile = FALSE)
  
  observeEvent (input$S3Next, {
    n <- isolate(input$S3frame)
    in1 <- which(pageN$session == 3)[1]
    in2 <- which(pageN$session == 3)
    in2 <- in2[length(in2)]
    if (n < pageN$max[in2]) {
      n <- n + 1
      updateNumericInput(session, 'S3frame', value=n)
      if (n >= pageN$low[in1] && n <= pageN$max[in1]) {
        updateTabsetPanel (session, 'S3tab', 'R as a Calculator')
      } else if (n >= pageN$low[in1+1] && n <= pageN$max[in1+1]) {
        updateTabsetPanel(session, 'S3tab', 'Basic Operators and Precedence')
      } else if (n >= pageN$low[in1+2] && n <= pageN$max[in1+2]) {
        updateTabsetPanel(session, 'S3tab', 'Vectorized Operations') 
      } else if (n >= pageN$low[in1+3] && n <= pageN$max[in1+3]) {
        updateTabsetPanel(session, 'S3tab', 'Using Variables')
      }
    }
  })
  observeEvent (input$S3Back, {
    n <- isolate(input$S3frame)
    in1 <- which(pageN$session == 3)[1]
    in2 <- which(pageN$session == 3)
    in2 <- in2[length(in2)]
    if (n > pageN$low[in1]) {
      n <- n - 1
      updateNumericInput(session, 'S3frame', value=n)
      if (n >= pageN$low[in1] && n <= pageN$max[in1]) {
        updateTabsetPanel (session, 'S3tab', 'R as a Calculator')
      } else if (n >= pageN$low[in1+1] && n <= pageN$max[in1+1]) {
        updateTabsetPanel(session, 'S3tab', 'Basic Operators and Precedence')
      } else if (n >= pageN$low[in1+2] && n <= pageN$max[in1+2]) {
        updateTabsetPanel(session, 'S3tab', 'Vectorized Operations') 
      } else if (n >= pageN$low[in1+3] && n <= pageN$max[in1+3]) {
        updateTabsetPanel(session, 'S3tab', 'Using Variables')
      }
    }
  })
  exprS3tab <- quote ({
    S3tab <- input$S3tab
    if (S3tab == 'R as a Calculator') {
      updateNumericInput(session, 'S3frame', value=2)
    } else if (S3tab == 'Basic Operators and Precedence') {
      updateNumericInput(session, 'S3frame', value=4)
    } else if (S3tab == 'Vectorized Operations') {
      updateNumericInput(session, 'S3frame', value=15)
    } else if (S3tab == 'Using Variables') {
      updateNumericInput(session, 'S3frame', value=19)
    }
  })
  obsS3tab <- observe (exprS3tab, quoted=TRUE)
  
  output$S3PDF <- downloadHandler(
    filename = 'Session3.pdf',
    content = function (file) {
      file.copy("www/Session3.pdf", file)
    }
  )
  
  observeEvent (input$S4Next, {
    n <- isolate(input$S4frame)
    in1 <- which(pageN$session == 4)[1]
    in2 <- which(pageN$session == 4)
    in2 <- in2[length(in2)]
    if (n < pageN$max[in2]) {
      n <- n + 1
      updateNumericInput(session, 'S4frame', value=n)
      if (n >= pageN$low[in1] && n <= pageN$max[in1]) {
        updateTabsetPanel (session, 'S4tab', 'General Info re Packages')
      } else if (n >= pageN$low[in1+1] && n <= pageN$max[in1+1]) {
        updateTabsetPanel(session, 'S4tab', 'Ranadu')
      } 
    }
  })
  observeEvent (input$S4Back, {
    n <- isolate(input$S4frame)
    in1 <- which(pageN$session == 4)[1]
    in2 <- which(pageN$session == 4)
    in2 <- in2[length(in2)]
    if (n > pageN$low[in1]) {
      n <- n - 1
      updateNumericInput(session, 'S4frame', value=n)
      if (n >= pageN$low[in1] && n <= pageN$max[in1]) {
        updateTabsetPanel (session, 'S4tab', 'General Info re Packages')
      } else if (n >= pageN$low[in1+1] && n <= pageN$max[in1+1]) {
        updateTabsetPanel(session, 'S4tab', 'Ranadu')
      } 
    }
  })
  exprS4tab <- quote ({
    S4tab <- input$S4tab
    if (S4tab == 'General Info re Packages') {
      updateNumericInput(session, 'S4frame', value=2)
    } else if (S4tab == 'Ranadu') {
      updateNumericInput(session, 'S4frame', value=12)
    } 
  })
  obsS4tab <- observe (exprS4tab, quoted=TRUE)
  
  output$S4PDF <- downloadHandler(
    filename = 'Session4.pdf',
    content = function (file) {
      file.copy("www/Session4.pdf", file)
    }
  )
  
  observeEvent (input$S5Next, {
    n <- isolate(input$S5frame)
    in1 <- which(pageN$session == 5)[1]
    in2 <- which(pageN$session == 5)
    in2 <- in2[length(in2)]
    if (n < pageN$max[in2]) {
      n <- n + 1
      updateNumericInput(session, 'S5frame', value=n)
      if (n >= pageN$low[in1] && n <= pageN$max[in1]) {
        updateTabsetPanel (session, 'S5tab', 'Overview')
      } else if (n >= pageN$low[in1+1] && n <= pageN$max[in1+1]) {
        updateTabsetPanel(session, 'S5tab', 'Base Graphics')
      } else if (n >= pageN$low[in1+2] && n <= pageN$max[in1+2]) {
        updateTabsetPanel(session, 'S5tab', 'plotWAC Plots')
      } else if (n >= pageN$low[in1+3] && n <= pageN$max[in1+3]) {
        updateTabsetPanel(session, 'S5tab', 'ggplot Graphics')
      } else if (n >= pageN$low[in1+4] && n <= pageN$max[in1+4]) {
        updateTabsetPanel(session, 'S5tab', 'Other Examples')
      } 
    }
  })
  observeEvent (input$S5Back, {
    n <- isolate(input$S5frame)
    in1 <- which(pageN$session == 5)[1]
    in2 <- which(pageN$session == 5)
    in2 <- in2[length(in2)]
    if (n > pageN$low[in1]) {
      n <- n - 1
      updateNumericInput(session, 'S5frame', value=n)
      if (n >= pageN$low[in1] && n <= pageN$max[in1]) {
        updateTabsetPanel (session, 'S5tab', 'Overview')
      } else if (n >= pageN$low[in1+1] && n <= pageN$max[in1+1]) {
        updateTabsetPanel(session, 'S5tab', 'Base Graphics')
      } else if (n >= pageN$low[in1+2] && n <= pageN$max[in1+2]) {
        updateTabsetPanel(session, 'S5tab', 'plotWAC Plots')
      } else if (n >= pageN$low[in1+3] && n <= pageN$max[in1+3]) {
        updateTabsetPanel(session, 'S5tab', 'ggplot Graphics')
      } else if (n >= pageN$low[in1+4] && n <= pageN$max[in1+4]) {
        updateTabsetPanel(session, 'S5tab', 'Other Examples')
      } 
    }
  })
  exprS5tab <- quote ({
    S5tab <- input$S5tab
    if (S5tab == 'Overview') {
      updateNumericInput(session, 'S5frame', value=2)
    } else if (S5tab == 'Base Graphics') {
      updateNumericInput(session, 'S5frame', value=3)
    } else if (S5tab == 'plotWAC Plots') {
      updateNumericInput(session, 'S5frame', value=8)
    } else if (S5tab == 'ggplot Graphics') {
      updateNumericInput(session, 'S5frame', value=10)
    } else if (S5tab == 'Other Examples') {
      updateNumericInput(session, 'S5frame', value=16)
    }
  })
  obsS5tab <- observe (exprS5tab, quoted=TRUE)
  
  output$S5PDF <- downloadHandler(
    filename = 'Session5.pdf',
    content = function (file) {
      file.copy("www/Session5.pdf", file)
    }
  )
  
  observeEvent (input$S6Next, {
    n <- isolate(input$S6frame)
    in1 <- which(pageN$session == 6)[1]
    in2 <- which(pageN$session == 6)
    in2 <- in2[length(in2)]
    if (n < pageN$max[in2]) {
      n <- n + 1
      updateNumericInput(session, 'S6frame', value=n)
      if (n >= pageN$low[in1] && n <= pageN$max[in1]) {
        updateTabsetPanel (session, 'S6tab', 'Overview')
      } else if (n >= pageN$low[in1+1] && n <= pageN$max[in1+1]) {
        updateTabsetPanel(session, 'S6tab', 'Linear Fits')
      } else if (n >= pageN$low[in1+2] && n <= pageN$max[in1+2]) {
        updateTabsetPanel(session, 'S6tab', 'Regression and the Deming Fit')
      } else if (n >= pageN$low[in1+3] && n <= pageN$max[in1+3]) {
        updateTabsetPanel(session, 'S6tab', 'Nonlinear Fits')
      } else if (n >= pageN$low[in1+4] && n <= pageN$max[in1+4]) {
        updateTabsetPanel(session, 'S6tab', 'Maximum Likelihood')
      } 
    }
  })
  observeEvent (input$S6Back, {
    n <- isolate(input$S6frame)
    in1 <- which(pageN$session == 6)[1]
    in2 <- which(pageN$session == 6)
    in2 <- in2[length(in2)]
    if (n > pageN$low[in1]) {
      n <- n - 1
      updateNumericInput(session, 'S6frame', value=n)
      if (n >= pageN$low[in1] && n <= pageN$max[in1]) {
        updateTabsetPanel (session, 'S6tab', 'Overview')
      } else if (n >= pageN$low[in1+1] && n <= pageN$max[in1+1]) {
        updateTabsetPanel(session, 'S6tab', 'Linear Fits')
      } else if (n >= pageN$low[in1+2] && n <= pageN$max[in1+2]) {
        updateTabsetPanel(session, 'S6tab', 'Regression and the Deming Fit')
      } else if (n >= pageN$low[in1+3] && n <= pageN$max[in1+3]) {
        updateTabsetPanel(session, 'S6tab', 'Nonlinear Fits')
      } else if (n >= pageN$low[in1+4] && n <= pageN$max[in1+4]) {
        updateTabsetPanel(session, 'S6tab', 'Maximum Likelihood')
      } 
    }
  })
  exprS6tab <- quote ({
    S6tab <- input$S6tab
    if (S6tab == 'Overview') {
      updateNumericInput(session, 'S6frame', value=2)
    } else if (S6tab == 'Linear Fits') {
      updateNumericInput(session, 'S6frame', value=3)
    } else if (S6tab == 'Regression and the Deming Fit') {
      updateNumericInput(session, 'S6frame', value=11)
    } else if (S6tab == 'Nonlinear Fits') {
      updateNumericInput(session, 'S6frame', value=17)
    } else if (S6tab == 'Maximum Likelihood') {
      updateNumericInput(session, 'S6frame', value=19)
    }
  })
  obsS6tab <- observe (exprS6tab, quoted=TRUE)
  
  output$S6PDF <- downloadHandler(
    filename = 'Session6.pdf',
    content = function (file) {
      file.copy("www/Session6.pdf", file)
    }
  )
  
  observeEvent (input$S7Next, {
    n <- isolate(input$S7frame)
    in1 <- which(pageN$session == 7)[1]
    in2 <- which(pageN$session == 7)
    in2 <- in2[length(in2)]
    if (n < pageN$max[in2]) {
      n <- n + 1
      updateNumericInput(session, 'S7frame', value=n)
      if (n >= pageN$low[in1] && n <= pageN$max[in1]) {
        updateTabsetPanel (session, 'S7tab', 'Overview')
      } else if (n >= pageN$low[in1+1] && n <= pageN$max[in1+1]) {
        updateTabsetPanel(session, 'S7tab', 'Using knitr')
      } else if (n >= pageN$low[in1+2] && n <= pageN$max[in1+2]) {
        updateTabsetPanel(session, 'S7tab', 'An Example')
      } 
    }
  })
  observeEvent (input$S7Back, {
    n <- isolate(input$S7frame)
    in1 <- which(pageN$session == 7)[1]
    in2 <- which(pageN$session == 7)
    in2 <- in2[length(in2)]
    if (n > pageN$low[in1]) {
      n <- n - 1
      updateNumericInput(session, 'S7frame', value=n)
      if (n >= pageN$low[in1] && n <= pageN$max[in1]) {
        updateTabsetPanel (session, 'S7tab', 'Overview')
      } else if (n >= pageN$low[in1+1] && n <= pageN$max[in1+1]) {
        updateTabsetPanel(session, 'S7tab', 'Using knitr')
      } else if (n >= pageN$low[in1+2] && n <= pageN$max[in1+2]) {
        updateTabsetPanel(session, 'S7tab', 'An Example')
      } 
    }
  })
  
  exprS7tab <- quote ({
    S7tab <- input$S7tab
    if (S7tab == 'Overview') {
      updateNumericInput(session, 'S7frame', value=2)
    } else if (S7tab == 'Using knitr') {
      updateNumericInput(session, 'S7frame', value=4)
    } else if (S7tab == 'An Example') {
      updateNumericInput(session, 'S7frame', value=10)
    } 
  })
  obsS7tab <- observe (exprS7tab, quoted=TRUE)
  output$S7PDF <- downloadHandler(
    filename = 'Session7.pdf',
    content = function (file) {
      file.copy("www/Session7.pdf", file)
    }
  )
  
  observeEvent (input$S8Next, {
    n <- isolate(input$S8frame)
    in1 <- which(pageN$session == 8)[1]
    in2 <- which(pageN$session == 8)
    in2 <- in2[length(in2)]
    if (n < pageN$max[in2]) {
      n <- n + 1
      updateNumericInput(session, 'S8frame', value=n)
      if (n >= pageN$low[in1] && n <= pageN$max[in1]) {
        updateTabsetPanel (session, 'S8tab', 'Data Review')
      } else if (n >= pageN$low[in1+1] && n <= pageN$max[in1+1]) {
        updateTabsetPanel(session, 'S8tab', 'Ranadu data.frames')
      } else if (n >= pageN$low[in1+2] && n <= pageN$max[in1+2]) {
        updateTabsetPanel(session, 'S8tab', 'Operations on netCDF Files')
      } else if (n >= pageN$low[in1+3] && n <= pageN$max[in1+3]) {
        updateTabsetPanel(session, 'S8tab', 'Ranadu Processing Functions')
      } else if (n >= pageN$low[in1+4] && n <= pageN$max[in1+4]) {
        updateTabsetPanel(session, 'S8tab', 'Hints and Suggestions')
      } 
    }
  })
  observeEvent (input$S8Back, {
    n <- isolate(input$S8frame)
    in1 <- which(pageN$session == 8)[1]
    in2 <- which(pageN$session == 8)
    in2 <- in2[length(in2)]
    if (n > pageN$low[in1]) {
      n <- n - 1
      updateNumericInput(session, 'S8frame', value=n)
      if (n >= pageN$low[in1] && n <= pageN$max[in1]) {
        updateTabsetPanel (session, 'S8tab', 'Data Review')
      } else if (n >= pageN$low[in1+1] && n <= pageN$max[in1+1]) {
        updateTabsetPanel(session, 'S8tab', 'Ranadu data.frames')
      } else if (n >= pageN$low[in1+2] && n <= pageN$max[in1+2]) {
        updateTabsetPanel(session, 'S8tab', 'Operations on netCDF Files')
      } else if (n >= pageN$low[in1+3] && n <= pageN$max[in1+3]) {
        updateTabsetPanel(session, 'S8tab', 'Ranadu Processing Functions')
      } else if (n >= pageN$low[in1+4] && n <= pageN$max[in1+4]) {
        updateTabsetPanel(session, 'S8tab', 'Hints and Suggestions')
      } 
    }
  })
  exprS8tab <- quote ({
    S8tab <- input$S8tab
    if (S8tab == 'Data Review') {
      updateNumericInput(session, 'S8frame', value=2)
    } else if (S8tab == 'Ranadu data.frames') {
      updateNumericInput(session, 'S8frame', value=5)
    } else if (S8tab == 'Operations on netCDF Files') {
      updateNumericInput(session, 'S8frame', value=10)
    } else if (S8tab == 'Ranadu Processing Functions') {
      updateNumericInput(session, 'S8frame', value=14)
    } else if (S8tab == 'Hints and Suggestions') {
      updateNumericInput(session, 'S8frame', value=15)
    }
  })
  obsS8tab <- observe (exprS8tab, quoted=TRUE)
  
  output$S8PDF <- downloadHandler(
    filename = 'Session8.pdf',
    content = function (file) {
      file.copy("www/Session8.pdf", file)
    }
  )
  
  observeEvent (input$S9Next, {
    n <- isolate(input$S9frame)
    in1 <- which(pageN$session == 9)[1]
    in2 <- which(pageN$session == 9)
    in2 <- in2[length(in2)]
    if (n < pageN$max[in2]) {
      n <- n + 1
      updateNumericInput(session, 'S9frame', value=n)
    }
  })
  observeEvent (input$S9Back, {
    n <- isolate(input$S9frame)
    in1 <- which(pageN$session == 9)[1]
    in2 <- which(pageN$session == 9)
    in2 <- in2[length(in2)]
    if (n > pageN$low[in1]) {
      n <- n - 1
      updateNumericInput(session, 'S9frame', value=n)
    }
  })
  
  output$S9PDF <- downloadHandler(
    filename = 'Session9.pdf',
    content = function (file) {
      file.copy("www/Session9.pdf", file)
    }
  )
 
  # output$RS3png <- renderImage ({
  #   list(src = sprintf('Session3/S%02d.png', input$S3frame),
  #     contentType = 'image/png',
  #     width = 800,
  #     height = 600,
  #     alt = "RSessions image goes here")
  # }, deleteFile = FALSE)
  
  output$RS3png <- renderUI({
    PDFfile=sprintf("Session3/S%02d.pdf#zoom=160,0,0&toolbar=0", input$S3frame)
    tags$iframe(
      src=PDFfile,
      width="800",
      height="670")
  })
  output$RS4png <- renderUI({
    PDFfile=sprintf("Session4/S%02d.pdf#zoom=160,0,0&toolbar=0", input$S4frame)
    tags$iframe(
      src=PDFfile,
      width="800",
      height="670")
  })
  output$RS5png <- renderUI({
    PDFfile=sprintf("Session5/S%02d.pdf#zoom=160,0,0&toolbar=0", input$S5frame)
    tags$iframe(
      src=PDFfile,
      width="800",
      height="670")
  })
  output$RS6png <- renderUI({
    PDFfile=sprintf("Session6/S%02d.pdf#zoom=160,0,0&toolbar=0", input$S6frame)
    tags$iframe(
      src=PDFfile,
      width="800",
      height="670")
  })
  output$RS7png <- renderUI({
    PDFfile=sprintf("Session7/S%02d.pdf#zoom=160,0,0&toolbar=0", input$S7frame)
    tags$iframe(
      src=PDFfile,
      width="800",
      height="670")
  })
  output$RS8png <- renderUI({
    PDFfile=sprintf("Session8/S%02d.pdf#zoom=160,0,0&toolbar=0", input$S8frame)
    tags$iframe(
      src=PDFfile,
      width="800",
      height="670")
  })
  output$RS9png <- renderUI({
    PDFfile=sprintf("Session9/S%02d.pdf#zoom=160,0,0&toolbar=0", input$S9frame)
    tags$iframe(
      src=PDFfile,
      width="800",
      height="670")
  })
  
  # output$RS4png <- renderImage ({
  #   list(src = sprintf('Session4/S%02d.png', input$S4frame),
  #     contentType = 'image/png',
  #     width = 800,
  #     height = 600,
  #     alt = "RSessions image goes here")
  # }, deleteFile = FALSE)
  # output$RS5png <- renderImage ({
  #   list(src = sprintf('Session5/S%02d.png', input$S5frame),
  #     contentType = 'image/png',
  #     width = 800,
  #     height = 600,
  #     alt = "RSessions image goes here")
  # }, deleteFile = FALSE)
  # output$RS5apng <- renderImage ({
  #   list(src = sprintf('Session5/S%02d.png', input$S5aframe),
  #     contentType = 'image/png',
  #     width = 800,
  #     height = 600,
  #     alt = "RSessions image goes here")
  # }, deleteFile = FALSE)
  # 
  # output$RS5bpng <- renderImage ({
  #   list(src = sprintf('Session5/S%02d.png', input$S5bframe),
  #     contentType = 'image/png',
  #     width = 800,
  #     height = 600,
  #     alt = "RSessions image goes here")
  # }, deleteFile = FALSE)
  # output$RS5cpng <- renderImage ({
  #   list(src = sprintf('Session5/S%02d.png', input$S5cframe),
  #     contentType = 'image/png',
  #     width = 800,
  #     height = 600,
  #     alt = "RSessions image goes here")
  # }, deleteFile = FALSE)
  # output$RS6png <- renderImage ({
  #   list(src = sprintf('Session6/S%02d.png', input$S6frame),
  #     contentType = 'image/png',
  #     width = 800,
  #     height = 600,
  #     alt = "RSessions image goes here")
  # }, deleteFile = FALSE)
  # output$RS7png <- renderImage ({
  #   list(src = sprintf('Session7/S%02d.png', input$S7frame),
  #     contentType = 'image/png',
  #     width = 800,
  #     height = 600,
  #     alt = "RSessions image goes here")
  # }, deleteFile = FALSE)
  # output$RS8png <- renderImage ({
  #   list(src = sprintf('Session8/S%02d.png', input$S8frame),
  #     contentType = 'image/png',
  #     width = 800,
  #     height = 600,
  #     alt = "RSessions image goes here")
  # }, deleteFile = FALSE)
  # output$RS9png <- renderImage ({
  #   list(src = sprintf('Session9/S%02d.png', input$S9frame),
  #     contentType = 'image/png',
  #     width = 800,
  #     height = 600,
  #     alt = "RSessions image goes here")
  # }, deleteFile = FALSE)


  output$pdfviewer <- renderText({
    if (grepl('PDF', input$Contrast)) { 
      return('<iframe style="height:600px; width:100%; scrolling=yes;", src="CONTRASTcalAOA.pdf"></iframe>')
    } else {
      return('')
    }
  })
  output$RnwViewer <- renderText({
    if (grepl('source', input$Contrast)) {
      # src='https://CONTRASTcalAOA.Rnw'
      # x <- readLines(con='./www/CONTRASTcalAOA.Rnw')
      # x <- paste(x,'<br>')
      return('<iframe style="height:600px; width:100%; scrolling=yes;", src="CONTRASTcalAOAlisting.pdf"></iframe>')
      # return(x)
    } else {
      return('')
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

